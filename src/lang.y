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
	int *loop_depth;
	struct stmt *prog;
	struct stmt *current;
	struct proc *next;
} proc;

typedef struct var	// a variable
{
	char *name;
	int value;
	struct proc *proc;
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

proc *make_proc (char *s, int *var_loc, stmt *prog, proc *next)
{
	proc *p = malloc(sizeof(proc));
	p->name = s;
	p->var_loc = var_loc;
	p->prog = prog;
	p->current = prog;
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
	if (!v) { yyerror("undeclared variable"); exit(1); }
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

%type <v> declist
%type <e> expr
%type <s> stmt assign stmtcase
%type <p> proc prog
%type <cd> condition 
%token PROC END VAR ASSIGN DO BREAK OD IF FI GARD ARROW ELSE PLUS MINUS EQUAL AND OR REACH
%token <c> ID
%token <i> INT

%left ';'


%%

prog	: vars proc condition { program_procs = $2; program_conditions = $3}

vars	: VAR declist ';'	{ program_vars = $2; }

declist	: ID			{ $$ = make_ident($1); }
	| declist ',' ID	{ ($$ = make_ident($3))->next = $1; }

proc	: PROC ID VAR declist stmt END proc
		{ $$ = make_proc($2, $4, $5, $7); }
	| PROC ID VAR declist stmt END
		{ $$ = make_proc($2, $4, $5, NULL); }

stmt	: assign
	| stmt ';' stmt	
		{ $$ = make_stmt(';',NULL,NULL,$1,$3); }
	| DO stmtcase OD
		{ $$ = make_stmt(DO,NULL,NULL,NULL,$2); }
	| IF stmtcase FI
		{ $$ = make_stmt(IF,NULL,NULL,NULL,$2); }
	| BREAK
		{ $$ = make_stmt(BREAK,NULL,NULL,NULL,NULL); }

assign	: ID ASSIGN expr
		{ $$ = make_stmt(ASSIGN,find_ident($1),$3,NULL,NULL); }

stmtcase: GARD expr ARROW stmt stmtcase
		{ $$ = make_stmt(GARD, NULL, $2, $4, $5); }
	| GARD expr ARROW stmt
		{ $$ = make_stmt(GARD, NULL, $2, $4, $5); }

expr	: ID			{ $$ = make_expr(0,find_ident($1),NULL,NULL,NULL); }
	| INT				{ $$ = make_expr(1,NULL,&$1,NULL,NULL); }
	| expr EQUAL expr	{ $$ = make_expr(EQUAL,NULL,NULL,$1,$3); }
	| expr AND expr		{ $$ = make_expr(AND,NULL,NULL,$1,$3); }
	| expr PLUS expr	{ $$ = make_expr(PLUS,NULL,NULL,$1,$3); }
	| expr MINUS expr	{ $$ = make_expr(MINUS,NULL,NULL,$1,$3); }
	| ELSE				{ $$ = make_expr(ELSE,NULL,NULL,NULL,NULL); }
	| '(' expr ')'		{ $$ = $2; }

condition : REACH expr condition 
		{ $$ = make_cond($2, $3); }
	| REACH expr 		
		{ $$ = make_cond($2, NULL); }

%%

#include "langlex.c"

/****************************************************************************/
/* programme interpreter      :                                             */

int eval (expr *e)
{
	switch (e->type)
	{
		case EQUAL: return eval(e->left) == eval(e->right);
		case AND: return eval(e->left) && eval(e->right);
		case PLUS: return eval(e->left) + eval(e->right);
		case MINUS: return eval(e->left) - eval(e->right);
		case 1: return *e->val;
		case 0: return e->var->value;
	}
}

void execute (stmt *s)
{
	int local_loop;
	switch(s->type)
	{
		case ASSIGN:
			s->var->value = eval(s->expr);
			break;
		case GARD:
			if (eval(s->expr)) execute(s->left);
			else execute(s->right);
			break;
		case ';':
			execute(s->left);
			execute(s->right);
			break;
		case DO:
			loop_depth++;
			if (loop_depth)
			{
			execute(s->left)
			};
			break;
		case IF:
			execute(s->left);
			break;
		case BREAK:

	}
}



void execute_proc(proc *p)
{
	srand(time(0));
	int i = rand() % 2;
	if (i == 0)
	{
		execute(p->current);
		p->current = p->current->right;
		check_reached(program_conditions);
		execute_proc(program_procs);
	}
	else
	{
		execute_proc(p->next)
	}
}

/****************************************************************************/

int main (int argc, char **argv)
{
	if (argc <= 1) { yyerror("no file specified"); exit(1); }
	yyin = fopen(argv[1],"r");
	if (!yyparse()) execute_proc(program_procs);
}