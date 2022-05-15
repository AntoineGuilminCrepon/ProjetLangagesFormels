%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


int yylex();

void yyerror(char *s)
{
	fflush(stdout);
	fprintf(stderr, "%s\n", s);
}

/***************************************************************************/
/* Data structures for storing a programme.                                */

typedef struct proc // a process
{
	char *name;
	struct var *var_loc;
	struct stmt *exec;
	struct proc *next;
} proc;

typedef struct var	// a variable
{
	char *name;
	int value;
	struct var *next;
} var;

typedef struct expr	// an integer expression
{
	int type;
	struct var *var;
	int *val;
	struct expr *left, *right;
} expr;

typedef struct stmt	// a command
{
	int type;
	struct var *var;
	struct expr *expr;
	struct stmt *left, *right;
} stmt;

typedef struct cond // a condition
{
	int is_reached;
	struct expr *expr;
	struct cond *next;
} cond;

/****************************************************************************/
/* All data pertaining to the programme are accessible from these two vars. */

var *program_vars;
proc *program_procs;
cond *program_conditions;

/****************************************************************************/
/* Functions for settting up data structures at parse time.                 */

proc *make_proc (char *s, var *var_loc, stmt *exec, proc *next)
{
	proc *p = malloc(sizeof(proc));
	p->name = s;
	p->var_loc = var_loc;
	p->exec = exec;
	p->next = next;
	return p;
}

var* make_ident (char *s)
{
	var *v = malloc(sizeof(var));
	v->name = s;
	v->value = 0;	// make variable zero initially
	v->next = NULL;
	return v;
}

var* find_ident (char *s)
{
	var *v = program_vars;
	while (v && strcmp(v->name,s)) v = v->next;
	if (!v) { 
		proc *p = program_procs;
		while (p) {
			v = p->var_loc;
			while (v && strcmp(v->name,s)) v = v->next;
			if (!v) {
				p = p->next;
			}
			else {
				break;
			}
		}
	}
	return v;
}

expr* make_expr (int type, var *var, int *val, expr *left, expr *right)
{
	expr *e = malloc(sizeof(expr));
	e->type = type;
	e->var = var;
	e->val = val;
	e->left = left;
	e->right = right;
	return e;
}

stmt* make_stmt (int type, var *var, expr *expr,
			stmt *left, stmt *right)
{
	stmt *s = malloc(sizeof(stmt));
	s->type = type;
	s->var = var;
	s->expr = expr;
	s->left = left;
	s->right = right;
	return s;
}

cond* make_cond (expr *expr, cond* next)
{
	cond *c = malloc(sizeof(cond));
	c->expr = expr;
	c->is_reached = 0;
	c->next = next;
	return c;
}


%}

/****************************************************************************/

/* types used by terminals and non-terminals */

%union {
	char *c;
	int i;
	var *v;
	expr *e;
	stmt *s;
	proc *p;
	cond *cd;
}

%type <v> declist vars_proc
%type <e> expr
%type <s> stmt stmtcase
%type <p> proc
%type <cd> condition 
%token PROC END VAR ASSIGN DO BREAK OD IF FI GARD ARROW ELSE REACH SKIP
%token <c> ID
%token <i> INT

%left ';'
%left EQUAL
%left AND OR PLUS MINUS NOT

%%

prog	: vars proc condition { program_procs = $2; program_conditions = $3; }

vars	: VAR declist ';'	{ program_vars = $2; }

vars_proc	: VAR declist ';'	{ $$ = $2; }

declist	: ID			{ $$ = make_ident($1); }
	| ID ',' declist	{ ($$ = make_ident($1))->next = $3; }

proc	: PROC ID VAR declist stmt END
		{ $$ = make_proc($2, $4, $5, NULL); } 
	| PROC ID vars_proc stmt END proc
		{ $$ = make_proc($2, $3, $4, $6); }

stmt	: ID ASSIGN expr
		{ $$ = make_stmt(ASSIGN,find_ident($1),$3,NULL,NULL); }
	| stmt ';'
		{ $$ = $1; }
	| stmt ';' stmt	
		{ $$ = make_stmt(';',NULL,NULL,$1,$3); }
	| DO stmtcase OD
		{ $$ = make_stmt(DO,NULL,NULL,NULL,$2); }
	| IF stmtcase FI
		{ $$ = make_stmt(IF,NULL,NULL,NULL,$2); }
	| BREAK
		{ $$ = make_stmt(BREAK,NULL,NULL,NULL,NULL); }
	| SKIP
		{ $$ = make_stmt(SKIP,NULL,NULL,NULL,NULL); }

stmtcase: GARD expr ARROW stmt
		{ $$ = make_stmt(GARD, NULL, $2, $4, NULL); }
	| GARD expr ARROW stmt stmtcase
		{ $$ = make_stmt(GARD, NULL, $2, $4, $5); }

expr	: ID			{ $$ = make_expr(0,find_ident($1),NULL,NULL,NULL); }
	| INT				{ $$ = make_expr(1,NULL,&$1,NULL,NULL); }
	| expr EQUAL expr	{ $$ = make_expr(EQUAL,NULL,NULL,$1,$3); }
	| expr AND expr		{ $$ = make_expr(AND,NULL,NULL,$1,$3); }
	| expr OR expr 		{ $$ = make_expr(OR,NULL,NULL,$1,$3); }
	| expr PLUS expr	{ $$ = make_expr(PLUS,NULL,NULL,$1,$3); }
	| expr MINUS expr	{ $$ = make_expr(MINUS,NULL,NULL,$1,$3); }
	| NOT expr			{ $$ = make_expr(NOT,NULL,NULL,NULL,$2); }
	| ELSE				{ $$ = make_expr(ELSE,NULL,NULL,NULL,NULL); }
	| '(' expr ')'		{ $$ = $2; }

condition : REACH expr 
		{ $$ = make_cond($2, NULL); }
	| REACH expr condition 
		{ $$ = make_cond($2, $3); }

%%

#include "langlex.c"

/****************************************************************************/
/* programme interpreter      :                                             */

void print_vars(var *v, int depth)
{
	struct var *current = v;
	for (int i=0; i<depth; i++) {
		printf("	");
	}
	printf("var ");
	while (current->next) {
		printf("%s, ", current->name);
		current = current->next;
	}
}

void print_expr (expr *e)
{
	switch (e->type)
	{
		case EQUAL: 
			print_expr(e->left);
			printf(" == ");
			print_expr(e->right);
			break;
		case AND: 
			print_expr(e->left);
			printf(" && ");
			print_expr(e->right);
			break;
		case OR:
			print_expr(e->left);
			printf(" || ");
			print_expr(e->right);
		case PLUS: 
			print_expr(e->left);
			printf(" + ");
			print_expr(e->right);
			break;
		case MINUS: 
			print_expr(e->left);
			printf(" - ");
			print_expr(e->right);
			break;
		case NOT:
			printf("!");
			print_expr(e->right);
		case 1:
			printf("%i", *e->val);
			break;
		case 0: 
			printf("%s", e->var->name);
			break;
	}
}

void print_stmt (stmt *s, int depth)
{
	for (int i=0; i<depth; i++) {
		printf("	");
	}
	switch(s->type)
	{
		case ASSIGN:
			printf("%s := ", s->var->name);
			print_expr(s->expr);
			printf(";\n");
			break;
		case GARD:
			printf(":: ");
			print_expr(s->expr);
			printf(" ->\n");
			print_stmt(s->left, depth+1);
			if (s->right) {
				print_stmt(s->right, depth+1);
			}
			break;
		case ';':
			print_stmt(s->left, depth);
			print_stmt(s->right, depth);
			break;
		case DO:
			printf("do\n");
			print_stmt(s->left, depth+1);
			printf("od\n");
			break;
		case IF:
			printf("if\n");
			print_stmt(s->left, depth+1);
			printf("fi\n");
			break;
		case BREAK:
			printf("break\n");
			break;
		case SKIP:
			printf("skip\n");
			break;
	}
}



void print_proc (proc *p)
{
	printf("proc %s\n", p->name);
	print_vars(p->var_loc, 1);
	print_stmt(p->exec, 1);
	printf("	end\n");
	if (p->next) {
		print_proc(p->next);
	}
}

void print_cond (cond *c)
{
	struct cond *current;
	while (current->next) {
		printf("reach ");
		print_expr(current->expr);
		printf("\n");
		current = current->next;
	}
}

/****************************************************************************/

int main (int argc, char **argv)
{
	if (argc <= 1) { yyerror("no file specified"); exit(1); }
	yyin = fopen(argv[1],"r");
	if (!yyparse()) {
		print_vars(program_vars, 0);
		print_proc(program_procs);
		print_cond(program_conditions);
	}
	return 0;
}
