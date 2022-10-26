/*
* MicroC Parser specification
*/

%{
  (* Auxiliary definitions *)
%}

/* Tokens declarations */
%token EOF
%token <int> INTEGER
%token <string> ID
%token ADD SUB MULT DIV
%token EQ
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token RETURN
%token SEMICOL
%token INT

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
    INTEGER                               { Ast.Prog([]) } // { $1 }
  | INT ID                          { Ast.Prog([]) } // { $1 }
  | RETURN expression SEMICOL                          { Ast.Prog([]) } // { $1 }
  | ID EQ expression SEMICOL                          { Ast.Prog([]) } // { $1 }
  | LPAREN expression RPAREN          { Ast.Prog([]) } // { $2 }
  | LBRACE expression RBRACE          { Ast.Prog([]) } // { $2 }
  | LBRACKET expression RBRACKET          { Ast.Prog([]) } // { $2 }
  | expression ADD expression        { Ast.Prog([]) } // { $1 + $3 }
  | expression SUB expression       { Ast.Prog([]) } // { $1 - $3 }
  | expression MULT expression       { Ast.Prog([]) } // { $1 * $3 }
  | expression DIV expression         { Ast.Prog([]) } // { $1 / $3 }
  | SUB expression %prec NEG    { Ast.Prog([]) } // { - $2 }
;
