# Building the Abstract Syntax Tree and Symbol Table

## Objectives
This project is to create a parser which builds an abstract syntax tree(AST) and symbol tables from reading a miniC++ program. In the process of parsing, we will build an AST and symbol table. At the end of parsing, it will print out an AST and symbol tables in 2 different text files. When printing out the AST, it will print out C++ like format code

## miniC++ grammar
//(a)+ : one or more repetition of a

//(a)* : zero or more repetition of a

//(a)? : a optionally exists

//unop : -

//addiop : +, -

//multop : *, /

//relaop :  <, >, <=, >=

//eqltop : ==, !=

//id : [A-Za-z_][A-Za-z0-9_]*

//intnum : [0-9]+

//floatnum : [0-9]+.[0-9]+



Program := (ClassList)? (ClassMethodList)? MainFunc



ClassList := (Class)+

Class := class id { (private : Member)? (public : Member)? } Member := (VarDeclList)? (MethodDeclList)? (MethodDefList)?

VarDeclList := (VarDecl)+
MethodDeclList := (FuncDecl)+

MethodDefList := (FuncDef)+

VarDecl := Type Ident (= (int | float))? ; FuncDecl := Type id ( (ParamList)? ) ;

FuncDef := Type id ( (ParamList)? ) CompoundStmt

ClassMethodList := (ClassMethodDef)+

ClassMethodDef := Type id :: id ( (ParamList)? ) CompoundStmt

MainFunc := int main ( ) CompoundStmt

ParamList := Param (, Param)*

Param := Type Ident

Ident := id | id [ intnum ]

Type := int | float | id


CompoundStmt := { (VarDeclList)? (StmtList)? }

StmtList := (Stmt)+

Stmt := ExprStmt

| AssignStmt | RetStmt

| WhileStmt | DoStmt

| ForStmt
| IfStmt

| CompoundStmt | ;

ExprStmt := Expr ;

AssignStmt := RefVarExpr = Expr ;

RetStmt := return (Expr)? ;

WhileStmt := while ( Expr ) Stmt

DoStmt := do Stmt while ( Expr ) ;

ForStmt := for ( Expr ; Expr ; Expr ) Stmt

IfStmt := if ( Expr ) Stmt (else Stmt)?

Expr := OperExpr

| RefExpr | intnum | floatnum

OperExpr :=	unop Expr
|	Expr addiop Expr
|	Expr multop Expr
|	Expr relaop Expr
|	Expr eqltop Expr
|	( Expr )

RefExpr := RefVarExpr | RefCallExpr

RefVarExpr := (RefExpr .)? IdentExpr

RefCallExpr := (RefExpr .)? CallExpr

IdentExpr := id [ Expr ]

| id

CallExpr := id ( (ArgList)? )

ArgList := Expr (, Expr)*

## Operator Precedence and Associativity
Precedence	Operator	Description	Associativity</br>
1	( )	Function call	Left-to-right</br>
2	-	Unary minus	Right-to-left</br>
3	*, /	Multiplication and division	Left-to-right</br>
4	+, -	Addition and subtraction	Left-to-right</br>
5	<, >, <=, >=	Relational operators	Left-to-right</br>
6	==, !=	Equality operators	Left-to-right</br>
7	=	Assignment	Right-to-left</br>

### 1. The structure of the project

AST.h : the data structure of miniC++ grammar
input.txt: the input C++ standard format program used to build AST and SymbolTable(source code)
makefile: use "make" command to execute flex+bison command and link them into a executable program, also provide make clean to clear all the generated files
compiler.l: flex file
compiler.y: bison file
traverseTree.h: a header file used to declare some function to traverse the AST
traverseTree.c: the implementation of the .h file

### 2. How to run the program

STEP 1: enter the root directory of the all the files i mentioned above in the linux terminal
STEP 2: use the command:  "make", then will generate all the files
STEP 3: use the command: "./compiler <input.txt", then will write the AST and symbol table respectively to each file
STEP 4: check the 2 files generated from STEP 3.
