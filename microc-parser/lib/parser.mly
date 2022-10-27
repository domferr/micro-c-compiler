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
      let vardec = Ast.Vardec( fst v, snd v ) in 
      build_annotated_node $loc vardec 
    }
  | typ ID LPAREN form = separated_list(COMMA, vardecl) RPAREN b = block 
    { 
      let block_node = build_annotated_node $loc b in 
      let fun_decl = Ast.Fundecl { typ = $1; fname = $2; formals = form; body = block_node } in
      build_annotated_node $loc fun_decl
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
    // stmt            { build_annotated_node($loc, $1)}
  | v = vardecl SEMICOL 
    { let dec = Ast.Dec( fst v, snd v ) in
      build_annotated_node $loc dec
    }
;

/* stmt: // todo return stmt_node
    RETURN expr? SEMICOL
  | expr? SEMICOL
  | block
  | WHILE LPAREN expr RPAREN stmt
  | FOR LPAREN expr? SEMICOL expr? SEMICOL expr? RPAREN stmt
  | IF LPAREN expr RPAREN stmt ELSE stmt
  | IF LPAREN expr RPAREN stmt
;

expr:
    rexpr
  | lexpr
;

lexpr:
    ID
  | LAPREN lexpr RPAREN
  // | "*" lexpr
  // | "*" aexpr
  | lexpr LBRACKET expr RBRACKET
;

rexpr:
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
  | LT
  | GT
  | LEG
  | GEQ
  | EQEQ
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
