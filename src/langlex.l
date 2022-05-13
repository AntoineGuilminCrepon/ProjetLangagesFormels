%option noyywrap

DIG	[0-9]

%%

"proc"	{ return PROC; }
"var"	{ return VAR; }
":="	{ return ASSIGN; }
"do"	{ return DO; }
"od"	{ return OD; }
"if"	{ return IF; }
"fi"	{ return FI; }
"::"	{ return GARD; }
"->"	{ return ARROW; }
"else"	{ return ELSE; }
"end"	{ return END; }
"reach"	{ return REACH; }

"+"		{ return PLUS; }
"-"		{ return MINUS; }
"=="	{ return EQUAL; }
"&&"	{ return AND; }
"||"	{ return OR; }

{DIG}+		{ yylval.i = atoi(yytext);
				return INT; }
[a-z][a-z0-9]*  { yylval.c = strdup(yytext); 
				return ID; }
"//".*"\n"	{}
[	 \n]	{}
.			{ return *yytext; }

%%