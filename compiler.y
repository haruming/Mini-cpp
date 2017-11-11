/* Definitions */
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "AST.h"
#include "traverseTree.h"
/* global variables which can be used in other .c .h */
struct Program *head;

FILE *fp1;   // FILE pointer to write AST to file
FILE *fp2;  // FILE pointer to write symbol tree to file
/* yyerror function */
void yyerror(char* text) {
    fprintf(stderr, "%s\n", text);
}
%}

/* definitions of tokens and types passed by FLEX */
%union {
	/* pointers for the AST struct nodes */
    struct Program 			*Program_ptr;
    struct Class			*Class_ptr;
    struct Member			*Member_ptr;
    struct VarDecl			*VarDecl_ptr;
    struct MethodDecl		*MethodDecl_ptr;
    struct MethodDef		*MethodDef_ptr;
    struct ClassMethodDef	*ClassMethodDef_ptr;
    struct MainFunc			*MainFunc_ptr;
    struct Param 			*Param_ptr;
    struct Ident 			*Ident_ptr;
    struct Type 			*Type_ptr;
    struct CompoundStmt 	*CompoundStmt_ptr;
    struct Stmt 			*Stmt_ptr;
    struct ExprStmt 		*ExprStmt_ptr;
    struct AssignStmt       *AssignStmt_ptr;
    struct RetStmt			*RetStmt_ptr;
    struct WhileStmt		*WhileStmt_ptr;
    struct DoStmt			*DoStmt_ptr;
    struct ForStmt			*ForStmt_ptr;
    struct IfStmt			*IfStmt_ptr;
    struct Expr 			*Expr_ptr;
    struct OperExpr			*OperExpr_ptr;
    struct RefExpr			*RefExpr_ptr;
    struct RefVarExpr		*RefVarExpr_ptr;
    struct RefCallExpr		*RefCallExpr_ptr;
    struct IdentExpr		*IdentExpr_ptr;
    struct CallExpr 		*CallExpr_ptr;
    struct Arg 				*Arg_ptr;
    /* int, float to char* */
    int intnum;
    float floatnum;
    char *id;
}

/* token definitions */
%token <intnum>INTNUM
%token <floatnum>FLOATNUM
%token <id>ID
%token INT FLOAT MAIN PRIVATE PUBLIC
%token IF ELSE FOR WHILE DO RETURN
%token DOUBLE_QUO SINGLE_QUO AND OR PLUS MINUS MULT DIV GT LT LE GE EQ NE

/* type(non-terminal) definitions */
%type <Program_ptr> PROGRAM
%type <Class_ptr> CLASS CLASSLIST
%type <Member_ptr> MEMBER
%type <VarDecl_ptr> VARDECL VARDECLLIST
%type <MethodDecl_ptr> METHODDECL METHODDECLLIST 
%type <MethodDef_ptr> METHODDEF METHODDEFLIST
%type <ClassMethodDef_ptr> CLASSMETHODDEF CLASSMETHODDEFLIST
%type <MainFunc_ptr> MAINFUNC 
%type <Param_ptr> PARAM PARAMLIST
%type <Ident_ptr> IDENT
%type <Type_ptr> TYPE
%type <CompoundStmt_ptr> COMPOUNDSTMT
%type <Stmt_ptr> STMT STMTLIST
%type <ExprStmt_ptr> EXPRSTMT
%type <AssignStmt_ptr> ASSIGNSTMT
%type <RetStmt_ptr> RETSTMT
%type <WhileStmt_ptr> WHILESTMT
%type <DoStmt_ptr> DOSTMT
%type <ForStmt_ptr> FORSTMT
%type <IfStmt_ptr> IFSTMT
%type <Expr_ptr> EXPR
%type <OperExpr_ptr> OPEREXPR
%type <RefExpr_ptr> REFEXPR
%type <RefVarExpr_ptr> REFVAREXPR
%type <RefCallExpr_ptr> REFCALLEXPR
%type <IdentExpr_ptr> IDENTEXPR
%type <CallExpr_ptr> CALLEXPR
%type <Arg_ptr> ARG ARGLIST

/* priority definitions from top to down, the priority becomes higher */
%right '=' 
%left EQ NE
%left LE GE GT LT
%left PLUS MINUS
%left MULT DIV
%right UNARY
%left '(' ')' 

/*no associativity symbols */
%nonassoc IF_THEN
%nonassoc ELSE

/* the start token */
%start PROGRAM




/* C++ Program Grammar Rules */
%%
// Program := (ClassList)? (ClassMethod)? MainFunc
PROGRAM: CLASSLIST CLASSMETHODDEFLIST MAINFUNC {
            struct Program *p = (struct Program*)malloc(sizeof(struct Program));	// create a new node and allocate the space
            p->_class = $1;
            p->classMethodDef = $2;
            p->mainFunc = $3;
            head = p;
            $$ = p;
       }
       | CLASSLIST MAINFUNC {
            struct Program *p = (struct Program*)malloc(sizeof(struct Program));
            p->_class = $1;
            p->classMethodDef = NULL;
            p->mainFunc = $2;
            head = p;
            $$ = p;
       }
       | CLASSMETHODDEFLIST MAINFUNC {
            struct Program *p = (struct Program*)malloc(sizeof(struct Program));
            p->_class = NULL;
            p->classMethodDef = $1;
            p->mainFunc = $2;
            head = p;
            $$ = p;
       }
       | MAINFUNC {
       		struct Program *p = (struct Program*)malloc(sizeof(struct Program));
       		p->_class = NULL;
       		p->classMethodDef = NULL;
       		p->mainFunc = $1;
       		head = p;
       		$$ = p;
       }
       ;

// ClassList := (Class)+
// Class := class id { (private : Member)? (public : Member)? }
CLASSLIST: CLASS {
            struct Class *c;
            c = $1;
            c->prev = NULL;
            $$ = c;
        }
        | CLASSLIST CLASS {
            struct Class *c;
            c = $2;
            c->prev = $1;
            $$ = c;
        }
        ;

CLASS: TYPE ID '{' PRIVATE ':' MEMBER PUBLIC ':' MEMBER '}' ';' {
			struct Class *c = (struct Class*)malloc(sizeof(struct Class));
			c->id = $2;
			c->priMember = $6;
			c->pubMember = $9;
			$$ = c;
		}
		| TYPE ID '{' PRIVATE ':' MEMBER '}' ';' {
			struct Class *c = (struct Class*)malloc(sizeof(struct Class));
			c->id = $2;
			c->priMember = $6;
			c->pubMember = NULL;
			$$ = c;
		}
		| TYPE ID '{' PUBLIC ':' MEMBER '}' ';' {
			struct Class *c = (struct Class*)malloc(sizeof(struct Class));
			c->id = $2;
			c->priMember = NULL;
			c->pubMember = $6;
			$$ = c;	
		}
		| TYPE ID '{' '}' ';' {
			struct Class *c = (struct Class*)malloc(sizeof(struct Class));
			c->id = $2;
			c->priMember = NULL;
			c->pubMember = NULL;
			$$ = c;				
		}
		;

// Member := (VarDeclList)? (MethodDeclList)? (MethodDefList)?
MEMBER: VARDECLLIST METHODDECLLIST METHODDEFLIST {
			struct Member *m = (struct Member*)malloc(sizeof(struct Member));
			m->varDecl = $1;
			m->methodDecl = $2;
			m->methodDef = $3;
			$$ = m;
		}
		| VARDECLLIST METHODDECLLIST {
			struct Member *m = (struct Member*)malloc(sizeof(struct Member));
			m->varDecl = $1;
			m->methodDecl = $2;
			m->methodDef = NULL;
			$$ = m;
		}
		| VARDECLLIST METHODDEFLIST {
			struct Member *m = (struct Member*)malloc(sizeof(struct Member));
			m->varDecl = $1;
			m->methodDecl = NULL;
			m->methodDef = $2;
			$$ = m;
		}
		| METHODDECLLIST METHODDEFLIST {
			struct Member *m = (struct Member*)malloc(sizeof(struct Member));
			m->varDecl = NULL;
			m->methodDecl = $1;
			m->methodDef = $2;
			$$ = m;
		}
		| VARDECLLIST {
			struct Member *m = (struct Member*)malloc(sizeof(struct Member));
			m->varDecl = $1;
			m->methodDecl = NULL;
			m->methodDef = NULL;
			$$ = m;
		}
		| METHODDECLLIST {
			struct Member *m = (struct Member*)malloc(sizeof(struct Member));
			m->varDecl = NULL;
			m->methodDecl = $1;
			m->methodDef = NULL;
			$$ = m;
		}
		| METHODDEFLIST {
			struct Member *m = (struct Member*)malloc(sizeof(struct Member));
			m->varDecl = NULL;
			m->methodDecl = NULL;
			m->methodDef = $1;
			$$ = m;
		}
		;

// VarDeclList := (VarDecl)+
// VarDecl := Type Ident (= (intnum|floatnum))?;
VARDECLLIST: VARDECL {
			struct VarDecl *v;
			v = $1;
			v->prev = NULL;
			$$ = v;
		}
		| VARDECLLIST VARDECL {
			struct VarDecl *v;
			v = $2;
			v->prev = $1;
			$$ = v;
		}
		;

VARDECL: TYPE IDENT '=' INTNUM ';' {
			struct VarDecl *v = (struct VarDecl*)malloc(sizeof(struct VarDecl));
			v->type = $1;
			v->ident = $2;
			v->assignType = eAsInt;
			v->assigner.intnum = $4;
			$$ = v;
		}
		| TYPE IDENT '=' FLOATNUM ';' {
			struct VarDecl *v = (struct VarDecl*)malloc(sizeof(struct VarDecl));
			v->type = $1;
			v->ident = $2;
			v->assignType = eAsFloat;
			v->assigner.floatnum = $4;
			$$ = v;
		}
		| TYPE IDENT ';' {
			struct VarDecl *v = (struct VarDecl*)malloc(sizeof(struct VarDecl));
			v->type = $1;
			v->ident = $2;
			v->assignType = eNon;
			$$ = v;
		}
		;

// MethodDeclList := (MethodDecl)+
// MethodDecl := Type id ( (ParamList)? ) ;
METHODDECLLIST: METHODDECL {
			struct MethodDecl *m;
			m = $1;
			m->prev = NULL;
			$$ = m;
		}
		| METHODDECLLIST METHODDECL {
			struct MethodDecl *m;
			m = $2;
			m->prev = $1;
			$$ = m;
		}
		;

METHODDECL: TYPE ID '(' PARAMLIST ')' ';' {
			struct MethodDecl *m = (struct MethodDecl*)malloc(sizeof(struct MethodDecl));
			m->type = $1;
			m->id = $2;
			m->param = $4;
			$$ = m;
		}
		| TYPE ID '(' ')' ';' {
			struct MethodDecl *m = (struct MethodDecl*)malloc(sizeof(struct MethodDecl));
			m->type = $1;
			m->id = $2;
			m->param = NULL;
			$$ = m;
		}
		;

// MethodDefList := (MethodDef)+
// MethodDef := Type id ( (ParamList)? ) CompoundStmt
METHODDEFLIST: METHODDEF {
			struct MethodDef *m;
			m = $1;
			m->prev = NULL;
			$$ = m;
		}
		| METHODDEFLIST METHODDEF {
			struct MethodDef *m;
			m = $2;
			m->prev = $1;
			$$ = m;
		}
		;

METHODDEF: TYPE ID '(' PARAMLIST ')' COMPOUNDSTMT {
			struct MethodDef *m = (struct MethodDef*)malloc(sizeof(struct MethodDef));
			m->type = $1;
			m->id = $2;
			m->param = $4;
			m->compoundStmt = $6;
			$$ = m;
		}
		| TYPE ID '(' ')' COMPOUNDSTMT {
			struct MethodDef *m = (struct MethodDef*)malloc(sizeof(struct MethodDef));
			m->type = $1;
			m->id = $2;
			m->param = NULL;
			m->compoundStmt = $5;
			$$ = m;
		}
		;

// ClassMethodList := (ClassMethodDef)+
// ClassMethodDef := Type id :: id ( (ParamList)? ) CompoundStmt
CLASSMETHODDEFLIST: CLASSMETHODDEF {
			struct ClassMethodDef *c;
			c = $1;
			c->prev = NULL;
			$$ = c;
		}
		| CLASSMETHODDEFLIST CLASSMETHODDEF {
			struct ClassMethodDef *c;
			c = $2;
			c->prev = $1;
			$$ = c;
		}
		;

CLASSMETHODDEF: TYPE ID ':' ':' ID '(' PARAMLIST ')' COMPOUNDSTMT {
			struct ClassMethodDef *c = (struct ClassMethodDef*)malloc(sizeof(struct ClassMethodDef));
			c->type = $1;
			c->className = $2;
			c->methodName = $5;
			c->param = $7;
			c->compoundStmt = $9;
			$$ = c;
		}
		| TYPE ID ':' ':' ID '(' ')' COMPOUNDSTMT {
			struct ClassMethodDef *c = (struct ClassMethodDef*)malloc(sizeof(struct ClassMethodDef));
			c->type = $1;
			c->className = $2;
			c->methodName = $5;
			c->param = NULL;
			c->compoundStmt = $8;
			$$ = c;
		}
		;

// MainFunc := int main ( ) CompoundStmt
MAINFUNC: INT MAIN '(' ')' COMPOUNDSTMT {
			struct MainFunc *m = (struct MainFunc*)malloc(sizeof(struct MainFunc));
			m->compoundStmt = $5;
			$$ = m;
		}
		;

// ParamList := Param (, Param)*
// Param := Type Ident
PARAMLIST: PARAM {
			struct Param* p;
			p = $1;
			p->prev = NULL;
			$$ = p;
		}
		| PARAMLIST ',' PARAM {
			struct Param* p;
			p = $3;
			p->prev = $1;
			$$ = p;
		}
		;

PARAM: TYPE IDENT {
			struct Param *p = (struct Param*)malloc(sizeof(struct Param));
			p->type = $1;
			p->ident = $2;
			p->prev = NULL;
			$$ = p;
		}
		;

// Ident := id
//        | id [ intnum ]
IDENT: ID {
			struct Ident *i = (struct Ident*)malloc(sizeof(struct Ident));
			i->id = $1;
			i->len = 0;
			$$ = i;
		}
		| ID '[' INTNUM ']' {
			struct Ident *i = (struct Ident*)malloc(sizeof(struct Ident));
			i->id = $1;
			i->len = $3;
			$$ = i;
		}
		;

// Type := int
//       | float
//       | id
TYPE: INT {
			struct Type *t = (struct Type*)malloc(sizeof(struct Type));
			t->id = NULL;
			t->e = eInt;
			$$ = t;
		}
		| FLOAT {
			struct Type *t = (struct Type*)malloc(sizeof(struct Type));
			t->id = NULL;
			t->e = eFloat;
			$$ = t;			
		}
		| ID {
			struct Type *t = (struct Type*)malloc(sizeof(struct Type));
			t->id = $1;
			t->e = eClass;
			$$ = t;
		}

// CompoundStmt := { (VarDeclList)? (StmtList)? }
COMPOUNDSTMT: '{' '}' {
                struct CompoundStmt *comp = (struct CompoundStmt*)malloc(sizeof(struct CompoundStmt));
                comp->varDecl = NULL;
                comp->stmt = NULL;
                $$ = comp;
                
            }
            | '{' STMTLIST '}'  {
                struct CompoundStmt *comp = (struct CompoundStmt*)malloc(sizeof(struct CompoundStmt));
                comp->varDecl = NULL;
                comp->stmt = $2;
                $$ = comp;
            }
            |  '{' VARDECLLIST STMTLIST '}' {
                struct CompoundStmt *comp = (struct CompoundStmt*)malloc(sizeof(struct CompoundStmt));
                comp->varDecl = $2;
                comp->stmt = $3;
                $$ = comp;
            }
            |  '{' VARDECLLIST '}' {
                struct CompoundStmt *comp = (struct CompoundStmt*)malloc(sizeof(struct CompoundStmt));
                comp->varDecl = $2;
                comp->stmt = NULL;
                $$ = comp;
            }
            ;

// StmtList := (Stmt)+
// Stmt := ExprStmt
//       | AssignStmt
//       | RetStmt
//       | WhileStmt
//       | DoStmt
//       | ForStmt
//       | IfStmt
//       | CompoundStmt
//       | ;
STMTLIST: STMT {
            struct Stmt *stmt;
            stmt = $1;
            stmt->prev = NULL;
            $$ = stmt;
        }
        | STMTLIST STMT {
            struct Stmt *stmt;
            stmt = $2;
            stmt->prev = $1;
            $$ = stmt;
        }
        ;

STMT: EXPRSTMT {
        struct Stmt *stmt = (struct Stmt*)malloc(sizeof(struct Stmt));
        stmt->e = eExpr;
        stmt->type.exprStmt = $1;
        $$ = stmt;
	}
	| ASSIGNSTMT { 
        struct Stmt *stmt = (struct Stmt*)malloc(sizeof(struct Stmt));
        stmt->e = eAssign;
        stmt->type.assignStmt = $1;
        $$ = stmt;
    }
    | RETSTMT {
        struct Stmt *stmt = (struct Stmt*)malloc(sizeof(struct Stmt));
        stmt->e = eRet;
        stmt->type.retStmt = $1;
        $$ = stmt;
    }
    | WHILESTMT {
        struct Stmt *stmt = (struct Stmt*)malloc(sizeof(struct Stmt));
        stmt->e = eWhile;
        stmt->type.whileStmt = $1;
        $$ = stmt;
    }
    | DOSTMT {
        struct Stmt *stmt = (struct Stmt*)malloc(sizeof(struct Stmt));
        stmt->e = eDo;
        stmt->type.doStmt = $1;
        $$ = stmt;   	
    }
    | FORSTMT {
        struct Stmt *stmt = (struct Stmt*)malloc(sizeof(struct Stmt));
        stmt->e = eFor;
        stmt->type.forStmt = $1;
        $$ = stmt;
    }
    | IFSTMT {
        struct Stmt *stmt = (struct Stmt*)malloc(sizeof(struct Stmt));
        stmt->e = eIf;
        stmt->type.ifStmt = $1;
        $$ = stmt;
    }
    | COMPOUNDSTMT {
        struct Stmt *stmt = (struct Stmt*)malloc(sizeof(struct Stmt));
        stmt->e = eCompound;
        stmt->type.compoundStmt = $1;
        $$ = stmt;
    }
    | ';' {
        struct Stmt *stmt = (struct Stmt*)malloc(sizeof(struct Stmt));
        stmt->e = eSemi;
        $$ = stmt;
    }
    ;

// ExprStmt := Expr ;
EXPRSTMT: EXPR ';' {
		struct ExprStmt *e = (struct ExprStmt*)malloc(sizeof(struct ExprStmt));
		e->expr = $1;
		$$ = e;
	}
	;

// AssignStmt := RefVarExpr = Expr ;
ASSIGNSTMT: REFVAREXPR '=' EXPR ';' {
		struct AssignStmt *a = (struct AssignStmt*)malloc(sizeof(struct AssignStmt));
		a->refVarExpr = $1;
		a->expr = $3;
		$$ = a;
	}
	;

// RetStmt := return (Expr)? ;
RETSTMT: RETURN ';' {
		struct RetStmt *r = (struct RetStmt*)malloc(sizeof(struct RetStmt));
		r->expr = NULL;
		$$ = r;
	}
	| RETURN EXPR ';' {
		struct RetStmt *r = (struct RetStmt*)malloc(sizeof(struct RetStmt));
		r->expr = $2;
		$$ = r;
	}
	;

// WhileStmt := while ( Expr ) Stmt
WHILESTMT: WHILE '(' EXPR ')' STMT {
		struct WhileStmt *w = (struct WhileStmt*)malloc(sizeof(struct WhileStmt));
		w->cond = $3;
		w->body = $5;
		$$ = w;
	}
	;

// DoStmt := do Stmt while ( Expr ) ;
DOSTMT: DO STMT WHILE '(' EXPR ')' ';' {
		struct DoStmt *d = (struct DoStmt*)malloc(sizeof(struct DoStmt));
		d->cond = $5;
		d->body = $2;
		$$ = d;
	}
	;

// ForStmt := for ( Expr ; Expr ; Expr ) Stmt
FORSTMT: FOR '(' EXPR ';' EXPR ';' EXPR ')' STMT {
		struct ForStmt *f = (struct ForStmt*)malloc(sizeof(struct ForStmt));
		f->init = $3;
		f->cond = $5;
		f->incr = $7;
		f->body = $9;
		$$ = f;
	}
	;

/* priority and conflict by using %prec */
// IfStmt := if ( Expr ) Stmt (else Stmt)?
IFSTMT: IF '(' EXPR ')' STMT %prec IF_THEN {
		struct IfStmt *i = (struct IfStmt*)malloc(sizeof(struct IfStmt));
		i->cond = $3;
		i->ifBody = $5;
		i->elseBody = NULL;
		$$ = i;
	}
	| IF '(' EXPR ')' STMT ELSE STMT {
		struct IfStmt *i = (struct IfStmt*)malloc(sizeof(struct IfStmt));
		i->cond = $3;
		i->ifBody = $5;
		i->elseBody = $7;
		$$ = i;
	}
	;

// Expr := OperExpr
//       | RefExpr
//       | intnum
//       | floatnum
EXPR: OPEREXPR {
		struct Expr *expr = (struct Expr*)malloc(sizeof(struct Expr));
		expr->e = eOper;
		expr->type.operExpr = $1;
		$$ = expr;
	}
	| REFEXPR {
		struct Expr *expr = (struct Expr*)malloc(sizeof(struct Expr));
		expr->e = eRef;
		expr->type.refExpr = $1;
		$$ = expr;
	}
	| INTNUM {
		struct Expr *expr = (struct Expr*)malloc(sizeof(struct Expr));
		expr->e = eIntnum;
		expr->type.intnum = $1;
		$$ = expr;	
	}
	| FLOATNUM {
		struct Expr *expr = (struct Expr*)malloc(sizeof(struct Expr));
		expr->e = eFloatnum;
		expr->type.floatnum = $1;
		$$ = expr;			
	}
	;

// OperExpr := unop Expr
//           | Expr addiop Expr
//           | Expr multop Expr
//           | Expr relaop Expr
//           | Expr eqltop Expr
//           | ( Expr )
OPEREXPR: MINUS EXPR %prec UNARY {
		struct UnOp *unop = (struct UnOp*)malloc(sizeof(struct UnOp));
		unop->e = eNegative;
		unop->expr = $2;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eUn;
		operexpr->type.un = unop;
		$$ = operexpr;
	}


	| EXPR PLUS EXPR {
		struct AddiOp *addiop = (struct AddiOp*)malloc(sizeof(struct AddiOp));
		addiop->lhs = $1;
		addiop->rhs = $3;
		addiop->e = ePlus;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eAddi;
		operexpr->type.addi = addiop;
		$$ = operexpr;
	}
	| EXPR MINUS EXPR {
		struct AddiOp *addiop = (struct AddiOp*)malloc(sizeof(struct AddiOp));
		addiop->lhs = $1;
		addiop->rhs = $3;
		addiop->e = eMinus;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eAddi;
		operexpr->type.addi = addiop;
		$$ = operexpr;
	}


	| EXPR MULT EXPR {
		struct MultOp *multop = (struct MultOp*)malloc(sizeof(struct MultOp));
		multop->lhs = $1;
		multop->rhs = $3;
		multop->e = eMul;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eMult;
		operexpr->type.mult = multop;
		$$ = operexpr;
	}
	| EXPR DIV EXPR {
		struct MultOp *multop = (struct MultOp*)malloc(sizeof(struct MultOp));
		multop->lhs = $1;
		multop->rhs = $3;
		multop->e = eDiv;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eMult;
		operexpr->type.mult = multop;
		$$ = operexpr;
	}


	| EXPR LT EXPR {
		struct RelaOp *relaop = (struct RelaOp*)malloc(sizeof(struct RelaOp));
		relaop->lhs = $1;
		relaop->rhs = $3;
		relaop->e = eLT;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eRela;
		operexpr->type.rela = relaop;
		$$ = operexpr;
	}
	| EXPR GT EXPR {
		struct RelaOp *relaop = (struct RelaOp*)malloc(sizeof(struct RelaOp));
		relaop->lhs = $1;
		relaop->rhs = $3;
		relaop->e = eGT;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eRela;
		operexpr->type.rela = relaop;
		$$ = operexpr;
	}
	| EXPR LE EXPR {
		struct RelaOp *relaop = (struct RelaOp*)malloc(sizeof(struct RelaOp));
		relaop->lhs = $1;
		relaop->rhs = $3;
		relaop->e = eLE;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eRela;
		operexpr->type.rela = relaop;
		$$ = operexpr;
	}
	| EXPR GE EXPR {
		struct RelaOp *relaop = (struct RelaOp*)malloc(sizeof(struct RelaOp));
		relaop->lhs = $1;
		relaop->rhs = $3;
		relaop->e = eGE;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eRela;
		operexpr->type.rela = relaop;
		$$ = operexpr;
	}


	| EXPR EQ EXPR {
		struct EqltOp *eqltop = (struct EqltOp*)malloc(sizeof(struct EqltOp));;
		eqltop->lhs = $1;
		eqltop->rhs = $3;
		eqltop->e = eEQ;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eEqlt;
		operexpr->type.eqlt = eqltop;
		$$ = operexpr;
	}
	| EXPR NE EXPR {
		struct EqltOp *eqltop = (struct EqltOp*)malloc(sizeof(struct EqltOp));;
		eqltop->lhs = $1;
		eqltop->rhs = $3;
		eqltop->e = eNE;

		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eEqlt;
		operexpr->type.eqlt = eqltop;
		$$ = operexpr;
	}


	| '(' EXPR ')' {
		struct OperExpr *operexpr = (struct OperExpr*)malloc(sizeof(struct OperExpr));
		operexpr->e = eBracket;
		operexpr->type.bracket = $2;
		$$ = operexpr;
	}
	;

// RefExpr := RefVarExpr
//          | RefCallExpr
REFEXPR: REFVAREXPR {
		struct RefExpr *r = (struct RefExpr*)malloc(sizeof(struct RefExpr));
		r->e = eVar;
		r->type.refVarExpr = $1;
		$$ = r;
	}
	| REFCALLEXPR {
		struct RefExpr *r = (struct RefExpr*)malloc(sizeof(struct RefExpr));
		r->e = eCall;
		r->type.refCallExpr = $1;
		$$ = r;
	}
	;

// RefVarExpr := (RefExpr .)? IdentExpr
REFVAREXPR: REFEXPR '.' IDENTEXPR {
		struct RefVarExpr *r = (struct RefVarExpr*)malloc(sizeof(struct RefVarExpr));
		r->refExpr = $1;
		r->identExpr = $3;
		$$ = r;
	}
	| IDENTEXPR {
		struct RefVarExpr *r = (struct RefVarExpr*)malloc(sizeof(struct RefVarExpr));
		r->refExpr = NULL;
		r->identExpr = $1;
		$$ = r;	
	}
	;

// RefCallExpr := (RefExpr .)? CallExpr
REFCALLEXPR: REFEXPR '.' CALLEXPR {
		struct RefCallExpr *r = (struct RefCallExpr*)malloc(sizeof(struct RefCallExpr));
		r->refExpr = $1;
		r->callExpr = $3;
		$$ = r;	
	}
	| CALLEXPR {
		struct RefCallExpr *r = (struct RefCallExpr*)malloc(sizeof(struct RefCallExpr));
		r->refExpr = NULL;
		r->callExpr = $1;
		$$ = r;	
	}
	;

// IdentExpr := id [ Expr ]
//            | id
IDENTEXPR: ID '[' EXPR ']' {
		struct IdentExpr* identexpr = (struct IdentExpr*)malloc(sizeof(struct IdentExpr));
		identexpr->id = $1;
		identexpr->expr = $3;
		$$ = identexpr;
	}
	| ID {
		struct IdentExpr* identexpr = (struct IdentExpr*)malloc(sizeof(struct IdentExpr));
		identexpr->id = $1;
		identexpr->expr = NULL;
		$$ = identexpr;
	}
	;

// CallExpr := id ( (ArgList)? )
CALLEXPR: ID '(' ARGLIST ')' {
		struct CallExpr *callexpr = (struct CallExpr*)malloc(sizeof(struct CallExpr));
		callexpr->id = $1;
		callexpr->arg = $3;
		$$ = callexpr;
	}
	| ID '(' ')' {
		struct CallExpr *callexpr = (struct CallExpr*)malloc(sizeof(struct CallExpr));
		callexpr->id = $1;
		callexpr->arg = NULL;
		$$ = callexpr;
	}
	;

// ArgList := Expr (, Expr)*
ARGLIST: ARG {
		struct Arg* arg;
		arg = $1;
		arg->prev = NULL;
		$$ = arg;
	}
	| ARGLIST ',' ARG {
		struct Arg* arg;
		arg = $3;
		arg->prev = $1;
		$$ = arg;
	}
	;

ARG: EXPR {
		struct Arg *arg = (struct Arg*)malloc(sizeof(struct Arg));
		arg->expr = $1;
		arg->prev = NULL;
		$$ = arg;
	}
	;

%%


int main(int argc, char* argv[]) {
    fp1 = fopen("ASTtree.txt","w");
    fp2 = fopen("symbolTable.txt","w");
    /* initialize the start and end scope */
    startScope = endScope = NULL;
    /* generate the AST tree in the process */
    yyparse();
    /* the start scope should be the GLOBAL means CLASS scope type */
    startScope = createNewScope(CLASS_scope, NULL);
    /* same at the beginning */
    endScope = startScope;
    /* traverse the AST generated by the yyparse() before */
    visitProgram(head);
    
    fprintf(fp1, "\n");
    fclose(fp1);
    fclose(fp2);
    return 0;
}