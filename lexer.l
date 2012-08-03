%{
#include <stdio.h>
#include <iostream>
#include <map>
using namespace std;
#define YY_DECL extern "C" int yylex()

#include "parser.tab.h"
map<string,string> ConstantsSymTab;
void installConstantStrings(string);
void installConstantNumerics(string);
%} 

letter [a-zA-Z]
digit [0-9]
id {letter}({letter}|{digit}|_)*
int {digit}+
ws [[:space:]]+
quotes \"
str {quotes}[^\n"]+{quotes}
comments "{"[^}]*"}"
%option noyywrap 

%%
and	{yylval.sval = strdup(yytext);return(AND);}

begin	{yylval.sval = strdup(yytext);return(PBEGIN);}

forward	{yylval.sval = strdup(yytext);return(FORWARD);}
div	{yylval.sval = strdup(yytext);return(DIV);}
do	{yylval.sval = strdup(yytext);return(DO);}
else	{yylval.sval = strdup(yytext);return(ELSE);}
end	{yylval.sval = strdup(yytext);return(END);}
for	{yylval.sval = strdup(yytext);return(FOR);}
function {yylval.sval = strdup(yytext);return(FUNCTION);}
if	 {yylval.sval = strdup(yytext);return(IF);}
array	 {yylval.sval = strdup(yytext);return(ARRAY);}
mod	 {yylval.sval = strdup(yytext);return(MOD);}
not	 {yylval.sval = strdup(yytext);return(NOT);}
of	 {yylval.sval = strdup(yytext);return(OF);}
or	 {yylval.sval = strdup(yytext);return(OR);}
procedure {yylval.sval = strdup(yytext);return(PROCEDURE);}
program	{yylval.sval = strdup(yytext);return(PROGRAM);}
record	{yylval.sval = strdup(yytext);return(RECORD);}
then	{yylval.sval = strdup(yytext);return(THEN);}
to	{yylval.sval = strdup(yytext);return(TO);}
type	{yylval.sval = strdup(yytext);return(TYPE);}
var	{yylval.sval = strdup(yytext);return(VAR);}
while	{yylval.sval = strdup(yytext);return(WHILE);}
	
"="  {yylval.sval = strdup(yytext);return (EQUAL);}
"<"  {yylval.sval = strdup(yytext);return (LT);}
"<=" {yylval.sval = strdup(yytext);return (LE);}
">"  {yylval.sval = strdup(yytext);return (GT);} 
">=" {yylval.sval = strdup(yytext);return (GE);}
"<>" {yylval.sval = strdup(yytext);return (NOTEQUAL);}
"+"  {yylval.sval = strdup(yytext);return (PLUS);}
"*"  {yylval.sval = strdup(yytext);return (MULTIPLICATION);}
"-"  {yylval.sval = strdup(yytext);return (MINUS);}
"."  {yylval.sval = strdup(yytext);return (DOT);}
","  {yylval.sval = strdup(yytext);return (COMMA);}
":"  {yylval.sval = strdup(yytext);return (COLON);}
";"  {yylval.sval = strdup(yytext);return (SEMICOLON);}
":=" {yylval.sval = strdup(yytext);return (ASSIGNMENT);}
".." {yylval.sval = strdup(yytext);return (DOTDOT);}
"("  {yylval.sval = strdup(yytext);return (LPAREN);}
")"  {yylval.sval = strdup(yytext);return (RPAREN);}
"["  {yylval.sval = strdup(yytext);return (LBRAC);}
"]"  {yylval.sval = strdup(yytext);return (RBRAC);}

{ws}	{}
{comments} {}
{int}	{ yylval.sval = strdup(yytext); installConstantNumerics(yytext); return(INT);}
{id}	{ yylval.sval = strdup(yytext);return(ID);}
{str}	{ yylval.sval = strdup(yytext);installConstantStrings(yytext); return(STRING);}
.       {printf("ERROR: %s is not a Valid Token!!!\n",yytext);}
%%

void installConstantStrings(string temp) {
ConstantsSymTab.insert(pair<string,string> (temp,"STRING LITERAL"));
}
void installConstantNumerics(string temp) {
ConstantsSymTab.insert(pair<string,string> (temp,"INTEGER"));
}

