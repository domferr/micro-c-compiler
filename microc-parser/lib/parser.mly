/*
* MicroC Parser specification
*/

%{
     (* Auxiliary definitions *)   

%}

/* Tokens declarations */
%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
//%token IF
%token EOF

/* Precedence and associativity specification */
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

/* Starting symbol */

(*%start program
//%type <Ast.program> program    /* the parser returns a Ast.program value */
%type <int> program
%%*)
%start program             /* the entry point */
%type <int> program
%%
program:
    expression EOF                { $1 }
;

/* Grammar specification */

expression:
    INT                               { $1 }
  | LPAREN expression RPAREN          { $2 }
  | expression PLUS expression        { $1 + $3 }
  | expression MINUS expression       { $1 - $3 }
  | expression TIMES expression       { $1 * $3 }
  | expression DIV expression         { $1 / $3 }
  | MINUS expression %prec UMINUS     { - $2 }
;
