/*
* MicroC Parser specification
*/

%{
  (* Auxiliary definitions *)
%}

/* Tokens declarations */
%token EOF
%token <string> ID
%token <int> INTEGER
%token <bool> BOOLEAN
/* Operators */
%token ADD SUB MULT DIV
%token EQ GT LT GEQ LEQ
/* Other symbols */
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token SEMICOL
/* Keywords */
%token INT CHAR BOOL VOID NULL
%token IF RETURN ELSE FOR WHILE

/* Precedence and associativity specification */
%left ADD SUB        /* lowest precedence */
%left MULT DIV         /* medium precedence */
%nonassoc NEG        /* highest precedence */

/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */
%%

/* Grammar specification */

program:
    expression EOF                { Ast.Prog([]) }
;

expression:
    INTEGER                               { Ast.Prog([]) }
  | INT ID SEMICOL                        { Ast.Prog([]) }
  | ID EQ expression SEMICOL                          { Ast.Prog([]) }
  | VOID ID LPAREN INT ID RPAREN LBRACE expression RBRACE                        { Ast.Prog([]) }
  | INT ID LPAREN RPAREN LBRACE expression RBRACE                        { Ast.Prog([]) }
  | RETURN expression SEMICOL                          { Ast.Prog([]) }
  | expression ADD expression        { Ast.Prog([]) } // { $1 + $3 }
  | expression SUB expression       { Ast.Prog([]) } // { $1 - $3 }
  | expression MULT expression       { Ast.Prog([]) } // { $1 * $3 }
  | expression DIV expression         { Ast.Prog([]) } // { $1 / $3 }
  | SUB expression %prec NEG    { Ast.Prog([]) } // { - $2 }
;
