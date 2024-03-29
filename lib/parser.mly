/*
* MicroC Parser specification
*/

(*  
  'for' loop is transformed into 'while' loop.
  for (init; cond; incr) { 
    content; 
  }
  The above 'for' loop becomes:
  init; 
  while(cond) { 
    content; 
    incr; 
  }
*)

%{
  (* Auxiliary definitions *)
  open Ast

  let build_node l n =
    { loc = Location.to_code_position(l); node = n }
%}


/* ---------------------------- Tokens declarations --------------------------- */
%token EOF
%token <string> ID
%token <int>  INTEGER
%token <bool> BOOLEAN
%token <char> CHARACTER
/* Operators */
%token ADD SUB MULT DIV MOD ASSIGN
%token SHORTADD SHORTSUB SHORTMULT SHORTMOD SHORTDIV
%token INCREMENT DECREMENT
%token EQ GT LT GEQ LEQ NEQ
%token OR AND NOT
/* Other symbols */
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token SEMICOL COMMA
%token AMPERSAND
/* Keywords */
%token INT CHAR VOID BOOL NULL
%token IF RETURN THEN ELSE FOR WHILE DO


/* ------ Precedence and associativity specification ------ */
(* The instruction [if c1 then if c2 then i1 else i2] involves a classic
   shift/reduce conflict, known as the dangling-else conflict. The conflict
   occurs when the token [ELSE] is discovered. At this point, reducing the
   production [instruction -> IF condition THEN instruction] leads
   to interpreting this instruction as [if c1 then (if c2 then i1) else i2],
   while shifting the token [ELSE] leads to interpreting it as
   [if c1 then (if c2 then i1 else i2)]. The desired behavior is the latter,
   so we must resolve the conflict in favor of shifting. By default, the
   precedence level associated with reducing the above production is the
   level of the token [THEN]. (This convention is explained in Menhir's
   manual. It is inherited from yacc.) So, we give [THEN] a lower precedence
   level than [ELSE]. This is done by the last two lines in the declarations
   that follow. 

  From: http://gallium.inria.fr/~fpottier/X/INF564/html/parser.mly.html   
*)
/* Fix for the dangling-else conflict */
%nonassoc THEN
%nonassoc ELSE

%right ASSIGN SHORTADD SHORTSUB SHORTMULT SHORTMOD SHORTDIV /* lowest precedence */
%left OR 
%left AND 
%left EQ NEQ
%nonassoc GT LT GEQ LEQ
%left ADD SUB 
%left MULT DIV MOD
%nonassoc NOT AMPERSAND
%nonassoc NEG
%nonassoc LBRACKET    /* highest precedence  */

/* Starting symbol */
%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */
%%


/* -------------------------- Grammar specification --------------------------- */
program:
    topdecls EOF    { Ast.Prog($1) }
;

topdecls:
    { [] }
  | typ ds = separated_nonempty_list(COMMA, vardecl) SEMICOL lis = topdecls 
    { 
      let decls = List.rev_map (fun buildvardecl ->
        let locandvar = buildvardecl $1 in
          build_node (fst locandvar) (Ast.Vardec(snd locandvar))
      ) ds in
      List.rev_append decls lis
    }
  | return_typ ID LPAREN form = separated_list(COMMA, formal) RPAREN b = block lis = topdecls
    { 
      let block_node = build_node $loc(b) b in 
      (build_node $loc (Ast.Fundecl{ 
        typ = $1; fname = $2; formals = form; body = Some(block_node)
      }))::lis
    }
;

/* Variable declaration with optional initialization */
vardecl:
    vardesc init = option(preceded(ASSIGN, expr)) 
      { 
        fun t -> ($loc, { typ = (fst $1) t; vname = snd $1; init})
      }
;

/* Types are all of them but not void */
typ:
    INT   { Ast.TypI }
  | CHAR  { Ast.TypC }
  | BOOL  { Ast.TypB }
;

/* Functions can return all the types plus void type and pointer, but not arrays */
%inline return_typ:
    VOID      { Ast.TypV }
  | typ       { $1 }
  | typ MULT  { Ast.TypP($1) }
;

formal:
    typ vardesc           { ((fst $2) $1, snd $2) }
;

vardesc:
    ID                                { ((fun t -> t), $1) }
  | MULT vardesc                      { ((fun t -> fst $2 (Ast.TypP(t))), snd $2 ) }
  | LPAREN vardesc RPAREN             { $2 }
  | vardesc LBRACKET integer=option(INTEGER) RBRACKET
                                      { ((fun t -> fst $1 (Ast.TypA(t, integer))), snd $1 ) } 
;

block:  // (stmt | vardecl SEMICOL)*
    LBRACE stmtordec RBRACE { Ast.Block($2) }
;

stmtordec:
    { [] }
  | stmt lis = stmtordec 
    { 
      (build_node $loc (Ast.Stmt($1)))::lis 
    }
  | typ ds = separated_nonempty_list(COMMA, vardecl) SEMICOL lis = stmtordec 
    {
      let decls = List.rev_map (fun buildvardecl ->
        let locandvar = buildvardecl $1 in
          build_node (fst locandvar) (Ast.Dec(snd locandvar))
      ) ds in
      List.rev_append decls lis
    }
;

stmt:
    RETURN expr? SEMICOL          { build_node $loc (Ast.Return($2)) }
  | expr SEMICOL                  { build_node $loc (Ast.Expr($1)) } // todo it was 'expr?', why?
  | block                         { build_node $loc $1 }
  | WHILE cond = delimited(LPAREN, expr, RPAREN) body = stmt 
    { 
      build_node $loc (Ast.While(cond, body))
    }
  | DO body = stmt WHILE cond = delimited(LPAREN, expr, RPAREN) SEMICOL
    { 
      let while_node = build_node $loc (Ast.While(cond, body)) in (* while -> stmt *)
      build_node $loc (Ast.Block([
        build_node $loc(body) (Ast.Stmt(body));    (* stmt -> stmtordec *)
        build_node $loc (Ast.Stmt(while_node))     (* stmt -> stmtordec *)
      ]))
    }
  | FOR LPAREN init = option(expr) SEMICOL cond = option(expr) SEMICOL incr = option(expr) RPAREN body = stmt  
    { 
      let while_body = match incr with 
        | None    ->  body
        | Some v  ->  let incr_stmt = build_node $loc(incr) (Ast.Expr(v)) in  (* expr -> stmt *)
                      build_node $loc (Ast.Block([
                        build_node $loc(body) (Ast.Stmt(body));     (* stmt -> stmtordec *)
                        build_node $loc(incr) (Ast.Stmt(incr_stmt)) (* stmt -> stmtordec *)
                      ]))
      in
      let condition = match cond with
        | None    ->  build_node $loc(cond) (Ast.BLiteral(true))
        | Some v  ->  v
      in
      let while_stmt_node = build_node $loc (Ast.While(condition, while_body)) in  (* while -> stmt *)
      match init with
        | None    ->  while_stmt_node
        | Some v  ->  let init_stmt = build_node $loc(init) (Ast.Expr(v)) in  (* expr -> stmt *)
                      build_node $loc (Ast.Block([
                        build_node $loc(init) (Ast.Stmt(init_stmt));          (* stmt -> stmtordec *)
                        build_node $loc (Ast.Stmt(while_stmt_node))     (* stmt -> stmtordec *)
                      ]))
    }
  | IF LPAREN cond = expr RPAREN then_branch = stmt %prec THEN
    {
      let else_branch = build_node $loc (Ast.Block([])) in
      build_node $loc (Ast.If(cond, then_branch, else_branch))
    }
  | IF LPAREN cond = expr RPAREN then_branch = stmt ELSE else_branch = stmt
    {
      build_node $loc (Ast.If(cond, then_branch, else_branch))
    }
;

expr:
    lexpr   { build_node $loc (Ast.Access($1)) }
  | rexpr   { build_node $loc $1 }
;

lexpr:
    ID                            { build_node $loc (Ast.AccVar($1)) }
  | LPAREN lexpr RPAREN           { $2 }
  | MULT lexpr                    { 
                                    let acc = build_node $loc($2) (Ast.Access($2)) in
                                    build_node $loc (Ast.AccDeref(acc))
                                  }
  | MULT AMPERSAND lexpr          { 
                                    let acc = build_node $loc($3) (Ast.Addr($3)) in
                                    build_node $loc (Ast.AccDeref(acc))
                                  }
  | lexpr LBRACKET expr RBRACKET  { build_node $loc (Ast.AccIndex($1, $3)) }
;

rexpr:
    aexpr                   { $1 }
  | ID LPAREN params = separated_list(COMMA, expr) RPAREN // ((expr COMMA)* expr)?
                            { Ast.Call($1, params) }
  | lexpr ASSIGN expr       { Ast.Assign($1, $3) }
  | NOT e = expr            { Ast.UnaryOp(Ast.Not, e) }
  | SUB e = expr %prec NEG  { Ast.UnaryOp(Ast.Neg, e) }
  | expr binop expr         { Ast.BinaryOp($2, $1, $3) }
  | lexpr shortop expr      {
                              let leftExpr = build_node $loc($1) (Ast.Access($1)) in
                              Ast.Assign($1, build_node $loc (Ast.BinaryOp($2, leftExpr, $3)))
                            }
  | INCREMENT lexpr         { Ast.UnaryOp(Ast.PreIncr, build_node $loc (Ast.Access($2))) }
  | DECREMENT lexpr         { Ast.UnaryOp(Ast.PreDecr, build_node $loc (Ast.Access($2))) }
  | lexpr INCREMENT         { Ast.UnaryOp(Ast.PostIncr, build_node $loc (Ast.Access($1))) }
  | lexpr DECREMENT         { Ast.UnaryOp(Ast.PostDecr, build_node $loc (Ast.Access($1))) }
;

aexpr:
    INTEGER               { Ast.ILiteral($1) }
  | CHARACTER             { Ast.CLiteral($1) }
  | BOOLEAN               { Ast.BLiteral($1) }
  | NULL                  { Ast.Null }
  | LPAREN rexpr RPAREN   { $2 }
  | AMPERSAND lexpr       { Ast.Addr($2) }
;

%inline binop: // inline to fix shift/reduce conflicts
    ADD   { Ast.Add }
  | SUB   { Ast.Sub }
  | MULT  { Ast.Mult }
  | MOD   { Ast.Mod }
  | DIV   { Ast.Div }
  | AND   { Ast.And }
  | OR    { Ast.Or }
  | EQ    { Ast.Equal }
  | NEQ   { Ast.Neq }
  | LT    { Ast.Less }
  | GT    { Ast.Greater }
  | LEQ   { Ast.Leq }
  | GEQ   { Ast.Geq }
;

%inline shortop:
  | SHORTADD  { Ast.Add } 
  | SHORTSUB  { Ast.Sub }
  | SHORTMULT { Ast.Mult }
  | SHORTMOD  { Ast.Mod }
  | SHORTDIV  { Ast.Div }
;