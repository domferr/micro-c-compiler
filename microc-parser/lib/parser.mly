/*
* MicroC Parser specification
*/

%{
  (* Auxiliary definitions *)
%}

/* Tokens declarations */
%token EOF
%token <int> INT
%token ADD SUB MULT DIV
%token LPAREN RPAREN
//%token IF

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
    INT                               { Ast.Prog([]) } // { $1 }
  | LPAREN expression RPAREN          { Ast.Prog([]) } // { $2 }
  | expression ADD expression        { Ast.Prog([]) } // { $1 + $3 }
  | expression SUB expression       { Ast.Prog([]) } // { $1 - $3 }
  | expression MULT expression       { Ast.Prog([]) } // { $1 * $3 }
  | expression DIV expression         { Ast.Prog([]) } // { $1 / $3 }
  | SUB expression %prec NEG    { Ast.Prog([]) } // { - $2 }
  | EOF                      {Ast.Prog([])}
;
