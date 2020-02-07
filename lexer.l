%option noyywrap

%{
#include <iostream>
#include <stdlib.h>
#include <string>

#include "parser.tab.h"

#define YY_DECL int yylex()

using namespace std;

extern void yyerror(const char*);
int lineno = 1;

%}

%%

[ \t]+	;
"["[^\]]*"]"	;
"ENDWHILE"	{return ENDWHILE;}
"ENDFOR"	{return ENDFOR;}
"ENDDO"		{return ENDDO;}
"ENDIF"		{return ENDIF;}
"WHILE"		{return WHILE;}
"DECLARE"	{return DECLARE;}
"BEGIN"		{return BEGINN;}
"END"		{return END;}
[-]?[0-9][0-9]*	{yylval.value = atol(yytext); return num;}
";"		{return SEMICOLON;}
":"		{return COLON;}
","		{return COMMA;}
"PLUS"		{return PLUS;}
"MINUS"		{return MINUS;}
"TIMES"		{return TIMES;}
"DIV"		{return DIV;}
"MOD"		{return MOD;}
"NEQ"		{return NEQ;}
"EQ"		{return EQ;}
"LEQ"		{return LEQ;}
"GEQ"		{return GEQ;}
"LE"		{return LE;}
"GE"		{return GE;}
"ASSIGN"	{return ASSIGN;}
"("		{return NLEFT;}
")"		{return NRIGHT;}
"IF"		{return IF;}
"THEN"		{return THEN;}
"ELSE"		{return ELSE;}
"FOR"		{return FOR;}
"FROM"		{return FROM;}
"DOWNTO"	{return DOWNTO;}
"TO"		{return TO;}
"DO"		{return DO;}
"READ"		{return READ;}
"WRITE"		{return WRITE;}
[_a-z]+		{yylval.idname = strdup(yytext); return pidentifier;}
[_a-zA-Z0-9]+	{
	// this is the way
	char er[50] = "Nierozpoznywalny napis ";
	const char* er2 = strdup(yytext);
	const char* err = strcat(er,er2);
	yyerror(err);
	yyterminate();
}
\n 		{lineno++;}
.		;

%%
