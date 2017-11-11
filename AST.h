/**
 * Copyright (C) 2017 ARCS lab - All Rights Reserved
 * You may use, distribute and modify this code under the
 * terms of the ARCS license
 *
 * You should have received a copy of the ARCS license with
 * this file. If not, please write to: jhpark@arcs.skku.edu
 *
 * Freely use this source code, but not allowed for any
 * commercial uses.
 */

#ifndef AST_H
#define AST_H

typedef char bool;
#define true 1
#define false 0

/**
 * Before you started..
 *
 * This file is a header file to implement the Abstract Syntax Tree(AST).
 * You have to use the following structs as the nodes of the tree.
 * This file will be included in bison implementation and tree walker.
 * There are some informations that helps you using this header easly.
 * So we recommend you to read carefully not olny this comment,
 * but also those written in each line.
 * 
 * First of all, all nodes are designed to target the visitor model.
 * There are two models of tree walkers; visitor and listener.
 * In visitor model, the developer controls the order of tree walks one by one.
 * You are developer in this case.
 * For example, when the tree walker program meets "Expr - Expr",
 * you have to make the program visit the right-hand side first at this time.
 *
 * The next information is how to implement "List" structs.
 * All structs of List is implemented in linked list.
 * The variable named "prev" connect between node.
 * The reason not using "next" variable is related to the execution model of Bison.
 * Bison tool is designed for visiting leaf node first.
 * It means that the parser will stack the nodes up while generating AST.
 */

typedef enum
{eInt, eFloat, eClass} Type_e;
typedef enum
{eExpr, eAssign, eRet, eWhile, eDo, eFor, eIf, eCompound, eSemi} Stmt_e;
typedef enum
{eOper, eRef, eIntnum, eFloatnum} Expr_e;
typedef enum
{eUn, eAddi, eMult, eRela, eEqlt, eBracket} Oper_e;
typedef enum
{eVar, eCall} Ref_e;
typedef enum
{eNegative} Unop_e;
typedef enum
{ePlus, eMinus} Addi_e;
typedef enum
{eMul, eDiv} Mult_e;
typedef enum
{eLT, eGT, eLE, eGE} Rela_e;
typedef enum
{eEQ, eNE} Eqlt_e;
typedef enum
{eNon, eAsInt, eAsFloat} Assign_e;

// Program := (ClassList)? (ClassMethod)? MainFunc
struct Program {
	struct Class *_class;
	struct ClassMethodDef *classMethodDef;
	struct MainFunc *mainFunc;
};

// ClassList := (Class)+
// Class := class id { (private : Member)? (public : Member)? }
struct Class {
	char *id;
	struct Member *priMember;
	struct Member *pubMember;
	struct Class *prev;
};

// Member := (VarDeclList)? (MethodDeclList)? (MethodDefList)?
struct Member {
	struct VarDecl *varDecl;
	struct MethodDecl *methodDecl;
	struct MethodDef *methodDef;
};

// VarDeclList := (VarDecl)+
// VarDecl := Type Ident (= (intnum|floatnum))? ;
struct VarDecl {
	struct Type *type;
	struct Ident *ident;
	Assign_e assignType;
	union {
		int intnum;
		float floatnum;
	} assigner;
	struct VarDecl *prev;
};

// MethodDeclList := (MethodDecl)+
// MethodDecl := Type id ( (ParamList)? ) ;
struct MethodDecl {
	char *id;
	struct Type *type;
	struct Param *param;
	struct MethodDecl *prev;
};

// MethodDefList := (MethodDef)+
// MethodDef := Type id ( (ParamList)? ) CompoundStmt
struct MethodDef {
	char *id;
	struct Type *type;
	struct Param *param;
	struct CompoundStmt *compoundStmt;
	struct MethodDef *prev;
};

// ClassMethodList := (ClassMethodDef)+
// ClassMethodDef := Type id :: id ( (ParamList)? ) CompoundStmt
struct ClassMethodDef {
	struct Type *type;
	char *className;
	char *methodName;
	struct Param *param;
	struct CompoundStmt *compoundStmt;
	struct ClassMethodDef *prev;
};

// MainFunc := int main ( ) CompoundStmt
struct MainFunc {
	struct CompoundStmt *compoundStmt;
};

// ParamList := Param (, Param)*
// Param := Type Ident
struct Param {
	struct Type *type;
	struct Ident *ident;
	struct Param *prev;
};

// Ident := id
//        | id [ intnum ]
struct Ident {
	char *id;
	int len; // 0 if scalar
};

// Type := int
//       | float
//       | id
struct Type {
	char *id; // NULL unless class type
	Type_e e;
};

// CompoundStmt := { (VarDeclList)? (StmtList)? }
struct CompoundStmt {
	struct VarDecl *varDecl;
	struct Stmt *stmt;
};

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
struct Stmt {
	Stmt_e e;
	union {
		struct ExprStmt *exprStmt;
		struct AssignStmt *assignStmt;
		struct RetStmt *retStmt;
		struct WhileStmt *whileStmt;
		struct DoStmt *doStmt;
		struct ForStmt *forStmt;
		struct IfStmt *ifStmt;
		struct CompoundStmt *compoundStmt;
	} type;
	struct Stmt *prev;
};

// ExprStmt := Expr ;
struct ExprStmt {
	struct Expr *expr;
};

// AssignStmt := RefVarExpr = Expr ;
struct AssignStmt {
	struct RefVarExpr *refVarExpr;
	struct Expr *expr;
};

// RetStmt := return (Expr)? ;
struct RetStmt {
	struct Expr *expr;
};

// WhileStmt := while ( Expr ) Stmt
struct WhileStmt {
	struct Expr *cond;
	struct Stmt *body;
};

// DoStmt := do Stmt while ( Expr ) ;
struct DoStmt {
	struct Expr *cond;
	struct Stmt *body;
};

// ForStmt := for ( Expr ; Expr ; Expr ) Stmt
struct ForStmt {
	struct Expr *init, *cond, *incr;
	struct Stmt *body;
};

// IfStmt := if ( Expr ) Stmt (else Stmt)?
struct IfStmt {
	struct Expr *cond;
	struct Stmt *ifBody;
	struct Stmt *elseBody;
};

// Expr := OperExpr
//       | RefExpr
//       | intnum
//       | floatnum
struct Expr {
	Expr_e e;
	union {
		struct OperExpr *operExpr;
		struct RefExpr *refExpr;
		int intnum;
		float floatnum;
	} type;
};

// OperExpr := unop Expr
//           | Expr addiop Expr
//           | Expr multop Expr
//           | Expr relaop Expr
//           | Expr eqltop Expr
//           | ( Expr )
struct OperExpr {
	Oper_e e;
	union {
		struct UnOp *un;
		struct AddiOp *addi;
		struct MultOp *mult;
		struct RelaOp *rela;
		struct EqltOp *eqlt;
		struct Expr *bracket;
	} type;
};

// RefExpr := RefVarExpr
//          | RefCallExpr
struct RefExpr {
	Ref_e e;
	union {
		struct RefVarExpr *refVarExpr;
		struct RefCallExpr *refCallExpr;
	} type;
};

// RefVarExpr := (RefExpr .)? IdentExpr
struct RefVarExpr {
	struct RefExpr *refExpr;
	struct IdentExpr *identExpr;
};

// RefCallExpr := (RefExpr .)? CallExpr
struct RefCallExpr {
	struct RefExpr *refExpr;
	struct CallExpr *callExpr;
};

// IdentExpr := id [ Expr ]
//            | id
struct IdentExpr {
	char *id;
	struct Expr *expr; // NULL if scalar
};

// CallExpr := id ( (ArgList)? )
struct CallExpr {
	char *id;
	struct Arg *arg;
};

// ArgList := Expr (, Expr)*
struct Arg {
	struct Expr *expr;
	struct Arg *prev;
};

/** Following definitions are additional structs for OperExpr. **/
struct UnOp {
	Unop_e e;
	struct Expr *expr;
};

struct AddiOp {
	Addi_e e;
	struct Expr *lhs;
	struct Expr *rhs;
};

struct MultOp {
	Mult_e e;
	struct Expr *lhs;
	struct Expr *rhs;
};

struct RelaOp {
	Rela_e e;
	struct Expr *lhs;
	struct Expr *rhs;
};

struct EqltOp {
	Eqlt_e e;
	struct Expr *lhs;
	struct Expr *rhs;
};

#endif
