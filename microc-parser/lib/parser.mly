/*
* MicroC Parser specification
*/

%{
  (* Auxiliary definitions *)
  open Ast

  let build_annotated_node l n =
    { loc = Location.to_code_position(l); node = n }
%}

/* Tokens declarations */
%token EOF
%token <string> ID
%token <int> INTEGER
%token <bool> BOOLEAN
/* Operators */
%token ADD SUB MULT DIV MOD ASSIGN
%token EQ GT LT GEQ LEQ NEQ
/* Other symbols */
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token SEMICOL COMMA
/* Keywords */
%token INT CHAR VOID BOOL NULL
%token IF RETURN ELSE FOR WHILE

(*
/* Precedence and associativity specification */
%left ADD SUB        /* lowest precedence */
%left MULT DIV         /* medium precedence */
%nonassoc NEG        /* highest precedence */
*)
/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */
%%

/* Grammar specification */

program:
    topdecl* EOF                { Ast.Prog($1) }
;

topdecl:
    v = vardecl SEMICOL 
    { 
      build_annotated_node $loc (Ast.Vardec( fst v, snd v )) 
    }
  | typ ID LPAREN form = separated_list(COMMA, vardecl) RPAREN b = block 
    { 
      let block_node = build_annotated_node $loc b in 
      build_annotated_node $loc (Ast.Fundecl { 
        typ = $1; fname = $2; formals = form; body = block_node })
    }
;

typ:
    INT   { Ast.TypI }
  | CHAR  { Ast.TypC }
  | VOID  { Ast.TypV }
  | BOOL  { Ast.TypB }
;

vardecl:
    typ vardesc { ($1, $2) }
;

vardesc:
    ID                                { $1 }
  (*| "*" Vardesc *)
  | LPAREN vardesc RPAREN             { $2 }
  | vardesc LBRACKET RBRACKET         { $1 }
  /* | vardesc LBRACKET INTEGER RBRACKET { $1 } (* todo how can I handle the integer?! *) */
;

block:  // (stmt | vardecl SEMICOL)*
    LBRACE list(stmtordec) RBRACE { Ast.Block($2) }
;

stmtordec:
    v = vardecl SEMICOL   { build_annotated_node $loc (Ast.Dec( fst v, snd v )) }
  | stmt                  { build_annotated_node $loc (Ast.Stmt($1)) }
;

stmt:
    RETURN expr? SEMICOL  { build_annotated_node $loc (Ast.Return($2)) }
  | expr SEMICOL          { build_annotated_node $loc (Ast.Expr($1)) } // todo it was 'expr?'
  | block                 { build_annotated_node $loc $1 }
  // | WHILE LPAREN expr RPAREN stmt { Ast.While(expr, stmt) }
  // | FOR LPAREN expr? SEMICOL expr? SEMICOL expr? RPAREN stmt
  // | IF LPAREN expr RPAREN stmt ELSE stmt
  // | IF LPAREN expr RPAREN stmt
;

expr:
    lexpr   { build_annotated_node $loc (Ast.Access($1)) }
  //| rexpr
;

 lexpr:
    ID                  { build_annotated_node $loc (Ast.AccVar($1)) }
  | LPAREN lexpr RPAREN { $2 }
  // | "*" lexpr
  // | "*" aexpr
  | lexpr LBRACKET expr RBRACKET  { build_annotated_node $loc (Ast.AccIndex($1, $3)) }
;

/* rexpr:
    aexpr
  | ID LPAREN ((expr COMMA)* expr)? RPAREN
  | lexpr EQ expr
  //| "!" expr
  | NEG expr
  | expr binop expr
;

aexpr:
    INTEGER
  // | CHARACTER
  | BOOLEAN
  | NULL
  | LPAREN rexpr RPAREN
  // | "&" lexpr
;

binop:
    PLUS
  | SUB
  | MULT
  | MOD
  | DIV
  //| "&&"
  //| "||"
  | EQ
  | LT
  | GT
  | LEG
  | GEQ
  | NEQ
; */

(*
expression:
    INTEGER                               { Ast.Prog([]) }
  | INT ID SEMICOL                        { Ast.Prog([]) }
  | ID EQ expression SEMICOL              { Ast.Prog([]) }
  | VOID ID LPAREN INT ID RPAREN LBRACE expression RBRACE                        { Ast.Prog([]) }
  | INT ID LPAREN RPAREN LBRACE expression RBRACE                        { Ast.Prog([]) }
  | RETURN expression SEMICOL                          { Ast.Prog([]) }
  | expression ADD expression        { Ast.Prog([]) } // { $1 + $3 }
  | expression SUB expression       { Ast.Prog([]) } // { $1 - $3 }
  | expression MULT expression       { Ast.Prog([]) } // { $1 * $3 }
  | expression DIV expression         { Ast.Prog([]) } // { $1 / $3 }
  | SUB expression %prec NEG    { Ast.Prog([]) } // { - $2 }
;*)
