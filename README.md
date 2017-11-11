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
