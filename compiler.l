/* Definitions */
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "AST.h"
#include "compiler.tab.h"
%}


/* Regular Expression Units */
INTNUM [0-9]+
FLOATNUM [0-9]+\.[0-9]+
ID [A-Za-z][A-Za-z0-9_]*     
SPACE [ \t\r\n]

/* Rules */
/* return tokens by Regular Expression(RE) */
%%
int return INT;
float return FLOAT;
main return MAIN;
private return PRIVATE;
public return PUBLIC;
return return RETURN;
while return WHILE;
do return DO;
if return IF;
else return ELSE;
for return FOR;

":" return yytext[0];
";" return yytext[0];
"." return yytext[0];
"," return yytext[0];
"=" return yytext[0];
"!" return yytext[0];
"[" return yytext[0];
"]" return yytext[0];
"{" return yytext[0];
"}" return yytext[0];
"(" return yytext[0];
")" return yytext[0];
"\"" {return DOUBLE_QUO;}
"\'" {return SINGLE_QUO;}
"\&" {return AND;}
"\|" {return OR;}
"+" return PLUS;
"-" return MINUS;
"*" return MULT;
"/" return DIV;
">"  return GT;
"<"  return LT;
"<=" return LE;
">=" return GE;
"==" return EQ;
"!=" return NE;

{INTNUM} {
    yylval.intnum = atoi(yytext);
    return INTNUM;
}

{FLOATNUM} {
    yylval.floatnum = atof(yytext);
    return FLOATNUM;
}

{ID} {
    yylval.id = strndup(yytext, yyleng);
    return ID;
}

{SPACE} {/*do nothing here*/}
%%

/* User Code */