#include "AST.h"

/* There are 8 statements which contain { } brackets. Each pair of bracket can express a scope */
typedef enum { CLASS_scope, FUNC_scope, DO_scope, WHILE_scope, FOR_scope, IF_scope, COMPOUND_scope } SCOPETYPE;

/* start scope and end scope used for make the linked list */
struct Scope *startScope, *endScope;
/* store the class name and function name to make a symbol table*/
char* currentClassName;
char* currentFuncName;

/* count down the number of statements in each scope */
struct Scope {
	/* type */
    SCOPETYPE type;
    /* number of the scope type */
    int do_num;
    int while_num;
    int for_num;
    int if_num;
    int compound_num;
    /* parent and children */
    struct Scope* parent;
    struct Scope* child;
};

struct Scope* createNewScope(SCOPETYPE type, struct Scope* parent);

void deleteScope(struct Scope** endScope);

int getScopeNum(SCOPETYPE type, struct Scope* parent);


/* avoid some warning from the terminal */
int yylex();
int yyparse();

void visitProgram(struct Program* program);

void visitClass(struct Class* _class);

void visitMember(struct Member* member);

void visitVarDecl(struct VarDecl* decl);

void visitMethodDecl(struct MethodDecl* decl);

void visitMethodDef(struct MethodDef* decl);

void visitClassMethod(struct ClassMethodDef* def);

void visitmainFunc(struct MainFunc* mainfunc);

void visitParam(struct Param* param);

void visitIdent(struct Ident* ident);

void visitType(struct Type* type);

void visitCompoundStmt(struct CompoundStmt* comp);

void visitStmt(struct Stmt* stmt);

void visitExprStmt(struct ExprStmt* exprstmt);

void visitAssignStmt(struct AssignStmt* assign);

void visitReturnStmt(struct RetStmt* retstmt);

void visitWhileStmt(struct WhileStmt* whilestmt);

void visitDoStmt(struct DoStmt* dostmt);

void visitForStmt(struct ForStmt* forstmt);

void visitIfStmt(struct IfStmt* ifstmt);

void visitExpr(struct Expr* expr);

void visitOperExpr(struct OperExpr* operexpr);

void visitRefExpr(struct RefExpr* ref);

void visitRefVarExpr(struct RefVarExpr* ref);

void visitRefCallExpr(struct RefCallExpr* ref);

void visitIdentExpr(struct IdentExpr* identexpr);

void visitCallExpr(struct CallExpr* callexpr);

void visitArg(struct Arg* arg);
