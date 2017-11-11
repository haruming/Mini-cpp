#include <stdio.h>
#include <stdlib.h>
#include "traverseTree.h"

/* FILE pointers used to write the AST and symboltable into file */
extern FILE *fp1;
extern FILE *fp2;

//global variable for making symbol table
int row_num;
Type_e currentType;		// used for distinguish the int and float and class type
bool isParameter = false;   //determine whether it is a parameter or a variable.
bool printDeclaration = false;   // print the variables and parameters in the symbol table
bool printScopePathAndTitle = false;    // print the location and title of the symbol table
bool AnotherCompoundStmt = false;
bool isCompoundStmt = false;

//make scope, from bottom to top because the feature of bison, stack the nodes up
struct Scope* createNewScope(SCOPETYPE type, struct Scope* parent) {
	struct Scope* scope = (struct Scope*)malloc(sizeof(struct Scope));
    scope->type = type;
    scope->do_num = 0;
    scope->while_num = 0;
    scope->for_num  = 0;
    scope->if_num = 0;
    scope->compound_num = 0;

    /* add the new scope to the end of the linked list */
    if(parent != NULL) {
        scope->parent = parent;
        parent->child = scope;
    } else {
        scope->parent = NULL;
    }

    scope->child = NULL;
	return scope;
}

/* delete the last scope of the linked list */
void deleteScope(struct Scope** endScope) {
    struct Scope* currentScope = *endScope;
    struct Scope* parent = currentScope->parent;

    if(parent != NULL) {
        parent->child = NULL;
        (*endScope) = parent;
        free(currentScope);
	}
}

//returns the order of current node scope
int getScopeNum(SCOPETYPE type, struct Scope* scope) {
    switch(type) {
        case DO_scope:
            return (scope->do_num);

        case WHILE_scope:
            return (scope->while_num);

        case FOR_scope:
            return (scope->for_num);

        case IF_scope:
            return (scope->if_num);

        case COMPOUND_scope:
            return (scope->compound_num);

	}
}

// print the symbol table to the tree.txt file according the node path
void printScopePath() {
    //when printing global variable
    if(endScope->type == CLASS_scope) {
        fprintf(fp2, "Location: %s\n", currentClassName);
        return;  //print nothing at "location"
    }
    else {
        fprintf(fp2, "\nLocation : ");
        
        fprintf(fp2, "%s", currentFuncName);
        
        struct Scope* curScope = startScope->child; //start from Function node
        while(curScope->child != NULL) {
            fprintf(fp2, " - ");
            switch(curScope->child->type) {
                case DO_scope:
                    fprintf(fp2, "DO");
                    break;

                case WHILE_scope:
                    fprintf(fp2, "WHILE");
                    break;

                case FOR_scope:
                    fprintf(fp2, "FOR");
                    break;

                case IF_scope:
                    fprintf(fp2, "IF");
                    break;

                case COMPOUND_scope:
                    fprintf(fp2, "COMPOUND");
                    break;
            }
            fprintf(fp2, "(%d) ", getScopeNum(curScope->child->type, curScope));
            curScope = curScope->child;
        }
        fprintf(fp2, "\n");
    }
}

//print symboltable, called before entering visitDeclaration
void printEachColumnTitle() {
	/* count the variable or parameter from number 1 */
    row_num = 1;
    /* print the path of the current scope */
    printScopePath();
    /* print the title of each column in a neat way */
    fprintf( fp2, "%10s%10s%10s%10s%10s\n", "Count","Type","Name","Array","Role");
}




void visitProgram(struct Program* program) {
	if (program->_class != NULL) {
		visitClass(program->_class);
		fprintf(fp1, "\n");
	}
	if (program->classMethodDef != NULL) {
		visitClassMethod(program->classMethodDef);
		fprintf(fp1, "\n");
	}
	visitmainFunc(program->mainFunc);
	fprintf(fp1, "\n");
}

void visitClass(struct Class* _class) {
    /* recursion to visit all the class */
    if(_class->prev != NULL) {
        visitClass(_class->prev);
    }

    fprintf(fp1, "class %s {\n", _class->id);
    /* get the class name */
	currentClassName = _class->id;

    if(_class->priMember != NULL) {
    	fprintf(fp1, "private:\n");
    	visitMember(_class->priMember);
    }
    if(_class->pubMember != NULL) {
    	fprintf(fp1, "public:\n");
    	visitMember(_class->pubMember);
    }
    fprintf(fp1, "};\n");
}

void visitMember(struct Member* member) {
	if (member->varDecl != NULL) visitVarDecl(member->varDecl);
	if (member->methodDecl != NULL) visitMethodDecl(member->methodDecl);
	if (member->methodDef != NULL) visitMethodDef(member->methodDef);
}

void visitVarDecl(struct VarDecl* decl) {
	isParameter = false;

	if (decl->prev != NULL) {
		visitVarDecl(decl->prev);
	}

	visitType(decl->type);

	if (!printScopePathAndTitle) {
		printEachColumnTitle();
		printScopePathAndTitle = true;
	}

	printDeclaration = true;
	visitIdent(decl->ident);
	printDeclaration = false;

	if (decl->assignType != eNon) {
		fprintf(fp1, "=");
		if (decl->assignType == eAsInt) fprintf(fp1, "%d;\n", decl->assigner.intnum);
		if (decl->assignType == eAsFloat) fprintf(fp1, "%f;\n", decl->assigner.floatnum);		
	} else {
		fprintf(fp1, ";\n");		
	}
}

void visitMethodDecl(struct MethodDecl* decl) {
	if (decl->prev != NULL) {
		visitMethodDecl(decl->prev);
	}

	visitType(decl->type);
	fprintf(fp1, "%s(", decl->id);
	if (decl->param != NULL) {
		visitParam(decl->param);
	}
	fprintf(fp1, ");\n");
}

void visitMethodDef(struct MethodDef* decl) {
	if (decl->prev != NULL) {
		visitMethodDef(decl->prev);
	}

	currentFuncName = decl->id;
	endScope = createNewScope(FUNC_scope, endScope);

	visitType(decl->type);
	fprintf(fp1, "%s(", decl->id);

	printScopePathAndTitle = false;

	if (decl->param != NULL) {
		printEachColumnTitle();
		printScopePathAndTitle = true;
		visitParam(decl->param);
	}
	fprintf(fp1, ")");
	visitCompoundStmt(decl->compoundStmt);

	deleteScope(&endScope);
	printScopePathAndTitle = false;
}

void visitClassMethod(struct ClassMethodDef* def) {
	if (def->prev != NULL) {
		visitClassMethod(def->prev);
	}

	currentFuncName = def->methodName;
	endScope = createNewScope(FUNC_scope, endScope);

	visitType(def->type);
	fprintf(fp1, "%s::%s(", def->className, def->methodName);

	printScopePathAndTitle = false;

	if (def->param != NULL) {
		printEachColumnTitle();
		printScopePathAndTitle = true;
		visitParam(def->param);
	}
	fprintf(fp1, ")");
	visitCompoundStmt(def->compoundStmt);

	deleteScope(&endScope);
	printScopePathAndTitle = false;
}

void visitmainFunc(struct MainFunc* mainfunc) {
	fprintf(fp1, "int main()");

	currentFuncName = "mainFunc";
	deleteScope(&endScope);
	endScope = createNewScope(FUNC_scope, endScope);

	printScopePathAndTitle = false;
	printEachColumnTitle();
	printScopePathAndTitle = true;

	visitCompoundStmt(mainfunc->compoundStmt);

	deleteScope(&endScope);
	printScopePathAndTitle = false;
}

void visitParam(struct Param* param) {
	isParameter = true;

	if (param->prev != NULL) {
		visitParam(param->prev);
		fprintf(fp1, ",");
	}

	if (!printScopePathAndTitle) {
		printEachColumnTitle();
		printScopePathAndTitle = true;
	}

	visitType(param->type);
	printDeclaration = true;
	visitIdent(param->ident);
	printDeclaration = false;
}

void visitIdent(struct Ident* ident) {
	if (ident->len == 0) {
		fprintf(fp1, "%s", ident->id);

		if (printDeclaration == true) {
			char* t;
			if (currentType == eInt) {
				t = "int";
			} else if(currentType == eFloat) {
				t = "float";
			} else {
				t = "class";
			}
			fprintf(fp2, "%10d%10s%10s%10s%10s\n", row_num++ , t, ident->id, "", isParameter ? "parameter" : "variable");
		}
	} else {
		fprintf(fp1, "%s[%d]", ident->id, ident->len);

		if (printDeclaration == true) {
			char* t;
			if (currentType == eInt) {
				t = "int";
			} else if(currentType == eFloat) {
				t = "float";
			} else {
				t = "class";
			}
			fprintf(fp2, "%10d%10s%10s%10d%10s\n", row_num++ , t, ident->id, ident->len, isParameter ? "parameter" : "variable");
		}
	}
}

void visitType(struct Type* type) {
	if (type->e == eInt) {
		fprintf(fp1, "int ");
		currentType = eInt;
	}
	if (type->e == eFloat) {
		fprintf(fp1, "float ");
		currentType = eFloat;
	}
	if (type->e == eClass) {
		fprintf(fp1, "%s ", type->id);
		currentType = eClass;
	}
}

void visitCompoundStmt(struct CompoundStmt* comp) {
	if (isCompoundStmt == true) {
		endScope = createNewScope(COMPOUND_scope, endScope);
		printScopePathAndTitle = false;
		endScope->parent->compound_num++;
	}

	AnotherCompoundStmt = false;

	fprintf(fp1, "{\n");
	if (comp->varDecl != NULL) visitVarDecl(comp->varDecl);
	if (comp->stmt != NULL) visitStmt(comp->stmt);
	fprintf(fp1, "}\n");

	if (isCompoundStmt == true) {
		deleteScope(&endScope);
	}
	isCompoundStmt = false;
	AnotherCompoundStmt =false;
}

void visitStmt(struct Stmt* stmt) {
	if (stmt->prev != NULL) {
		visitStmt(stmt->prev);
	}

	switch(stmt->e) {
		case eExpr:
			visitExprStmt(stmt->type.exprStmt);
			break;

		case eAssign:
			visitAssignStmt(stmt->type.assignStmt);
			break;

		case eRet:
			visitReturnStmt(stmt->type.retStmt);
			break;

		case eWhile:
			AnotherCompoundStmt = true;
			visitWhileStmt(stmt->type.whileStmt);
			break;

		case eDo:
			AnotherCompoundStmt = true;
			visitDoStmt(stmt->type.doStmt);
			break;

		case eFor:
			AnotherCompoundStmt = true;
			visitForStmt(stmt->type.forStmt);
			break;

		case eIf:
			AnotherCompoundStmt = true;
			visitIfStmt(stmt->type.ifStmt);
			break;

		case eCompound:
			if (AnotherCompoundStmt == false) isCompoundStmt = true;
			visitCompoundStmt(stmt->type.compoundStmt);
			break;

		case eSemi:
			fprintf(fp1, ";");
			break;
	}
	//fprintf(fp1, "\n");
}

void visitExprStmt(struct ExprStmt* exprstmt) {
	visitExpr(exprstmt->expr);
	fprintf(fp1, ";\n");
}

void visitAssignStmt(struct AssignStmt* assign) {
	visitRefVarExpr(assign->refVarExpr);
	fprintf(fp1, "=");
	visitExpr(assign->expr);
	fprintf(fp1, ";\n");
}

void visitReturnStmt(struct RetStmt* retstmt) {
	fprintf(fp1, "return");
	if (retstmt->expr != NULL) {
		fprintf(fp1, " ");
		visitExpr(retstmt->expr);
	}
	fprintf(fp1, ";\n");
}

void visitWhileStmt(struct WhileStmt* whilestmt) {
	endScope = createNewScope(WHILE_scope, endScope);
	printScopePathAndTitle = false;
	endScope->parent->while_num++;

	fprintf(fp1, "while(");
	visitExpr(whilestmt->cond);
	fprintf(fp1, ")\n");
	visitStmt(whilestmt->body);

	deleteScope(&endScope);
}

void visitDoStmt(struct DoStmt* dostmt) {
	endScope = createNewScope(DO_scope, endScope);
	printScopePathAndTitle = false;
	endScope->parent->do_num++;

	fprintf(fp1, "do\n");
	visitStmt(dostmt->body);
	fprintf(fp1, "while (");
	visitExpr(dostmt->cond);
	fprintf(fp1, ");\n");

	deleteScope(&endScope);
}

void visitForStmt(struct ForStmt* forstmt) {
	endScope = createNewScope(FOR_scope, endScope);
	printScopePathAndTitle = false;
	endScope->parent->for_num++;

	fprintf(fp1, "for(");
	visitExpr(forstmt->init);
	fprintf(fp1, "; ");
	visitExpr(forstmt->cond);
	fprintf(fp1, "; ");
	visitExpr(forstmt->incr);
	fprintf(fp1, ")\n");
	visitStmt(forstmt->body);

	deleteScope(&endScope);
}

void visitIfStmt(struct IfStmt* ifstmt) {
	endScope = createNewScope(IF_scope, endScope);
	printScopePathAndTitle = false;
	endScope->parent->if_num++;

	fprintf(fp1, "if (");
	visitExpr(ifstmt->cond);
	fprintf(fp1, ")\n");
	visitStmt(ifstmt->ifBody);
	if (ifstmt->elseBody != NULL) {
		fprintf(fp1, "\nelse\n");
		visitStmt(ifstmt->elseBody);
	}

	deleteScope(&endScope);
}

void visitExpr(struct Expr* expr) {
	switch(expr->e) {
		case eOper:
			visitOperExpr(expr->type.operExpr);
			break;

		case eRef:
			visitRefExpr(expr->type.refExpr);
			break;

		case eIntnum:
			fprintf(fp1, "%d", expr->type.intnum);
			break;

		case eFloatnum:
			fprintf(fp1, "%f", expr->type.floatnum);
			break;

	}
}

void visitOperExpr(struct OperExpr* operexpr) {
	switch(operexpr->e) {
		case eUn:
			fprintf(fp1, "-");
			visitExpr(operexpr->type.un->expr);
			break;

		case eAddi:
			visitExpr(operexpr->type.addi->lhs);
			if (operexpr->type.addi->e == ePlus) {
				fprintf(fp1, "+");
			} else {
				fprintf(fp1, "-");
			}
			visitExpr(operexpr->type.addi->rhs);
			break;

		case eMult:
			visitExpr(operexpr->type.mult->lhs);
			if (operexpr->type.mult->e == eMul) {
				fprintf(fp1, "*");
			} else {
				fprintf(fp1, "/");
			}
			visitExpr(operexpr->type.mult->rhs);
			break;

		case eRela:
			visitExpr(operexpr->type.rela->lhs);
			switch(operexpr->type.rela->e) {
				case eGT:
					fprintf(fp1, ">");
					break;

				case eLT:
					fprintf(fp1, "<");
					break;

				case eLE:
					fprintf(fp1, "<=");
					break;

				case eGE:
					fprintf(fp1, ">=");
					break;

			}
			visitExpr(operexpr->type.rela->rhs);
			break;			

		case eEqlt:
			visitExpr(operexpr->type.eqlt->lhs);
			if (operexpr->type.eqlt->e == eEQ) {
				fprintf(fp1, "==");
			} else {
				fprintf(fp1, "!=");
			}
			visitExpr(operexpr->type.eqlt->rhs);
			break;			

		case eBracket:
			fprintf(fp1, "(");
			visitExpr(operexpr->type.bracket);
			fprintf(fp1, ")");
			break;

	}
}

void visitRefExpr(struct RefExpr* ref) {
	if (ref->e == eVar) {
		visitRefVarExpr(ref->type.refVarExpr);
	} else if (ref->e == eCall) {
		visitRefCallExpr(ref->type.refCallExpr);
	}
}

void visitRefVarExpr(struct RefVarExpr* ref) {
	if (ref->refExpr != NULL) {
		visitRefExpr(ref->refExpr);
		fprintf(fp1, ".");
	}
	visitIdentExpr(ref->identExpr);
}

void visitRefCallExpr(struct RefCallExpr* ref) {
	if (ref->refExpr != NULL) {
		visitRefExpr(ref->refExpr);
		fprintf(fp1, ".");
	}
	visitCallExpr(ref->callExpr);
}

void visitIdentExpr(struct IdentExpr* identexpr) {
	fprintf(fp1, "%s", identexpr->id);
	if (identexpr->expr != NULL) {
		fprintf(fp1, "[");
		visitExpr(identexpr->expr);
		fprintf(fp1, "]");
	}
}

void visitCallExpr(struct CallExpr* callexpr) {
	fprintf(fp1, "%s(", callexpr->id);
	if (callexpr->arg != NULL) {
		visitArg(callexpr->arg);
	}
	fprintf(fp1, ")");
}

void visitArg(struct Arg* arg) {
	if (arg->prev != NULL) {
		visitArg(arg->prev);
		fprintf(fp1, ",");
	}

	visitExpr(arg->expr);
}
