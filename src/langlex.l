%option no yywrap

DIG	[0-9]
ALP	[a-zA-Z]

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
{ALP}+		{ yylval.c = yytext; 
				return ID; }
"//".*"\n"	{}
[	 \n]	{}
.			{}

%%