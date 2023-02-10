open Ast

(* A symbol which can be flagged as used or not used. A not used symbol 
   is a dead symbol which will be counted as dead code *)
type used_symbol = {
  mutable used: bool;
  isfun: bool; (* true if the symbol is a function, false otherwise *)
  loc: Location.code_pos;
}

(** Enque the deadcode found in the current scope. *)
let enqueue_deadcode_of_block queue decltbl = 
  let declaredlis = Symbol_table.get_current_block decltbl in
  let add_to_queue msg loc = Queue.add (Warning.create msg loc) queue in
  List.iter (fun (ide, decl) -> 
    if decl.used then () 
    else match decl.isfun with
      false -> add_to_queue (Printf.sprintf "Variable '%s' declared but never used" ide) decl.loc
    | true -> add_to_queue (Printf.sprintf "Function '%s' declared but never used" ide) decl.loc
  ) declaredlis

(** Sets to true the used flag of the symbol associated to the given identifier *)
let set_used ide decltbl = match Symbol_table.lookup ide decltbl with
    Some s -> s.used <- true
  | None -> ()

let rec check_deadcode queue node decltbl =
  match node.node with
    Ast.Block stmtordeclist ->
      (* Start of new scope *)
      Symbol_table.begin_block decltbl;
      (* Find deadcode in the block *)
      let block_res = check_block queue decltbl stmtordeclist in
      (* Enqueue dead code of the current scope *)
      enqueue_deadcode_of_block queue decltbl;
      (* End of the current scope *)
      Symbol_table.end_block decltbl;
      block_res
  | Ast.If(_, thbr, elbr) ->
      check_deadcode queue thbr decltbl && check_deadcode queue elbr decltbl
  | Ast.While(_, body) ->
      check_deadcode queue body decltbl
  | Ast.Return(Some e) -> check_expr e decltbl; true
  | Ast.Return(None) -> true
  | Ast.Expr(expr) -> check_expr expr decltbl; false
and check_expr expr decltbl =
  match expr.node with
    Ast.Call(ide, params) -> 
      List.iter (fun e -> check_expr e decltbl) params;
      set_used ide decltbl
  | Ast.Access acc -> check_access acc decltbl
  | Ast.Assign(acc, e) -> check_access acc decltbl; check_expr e decltbl
  | Ast.Addr acc -> check_access acc decltbl
  | Ast.UnaryOp(_, e) -> check_expr e decltbl
  | Ast.BinaryOp(_, e1, e2) -> check_expr e1 decltbl; check_expr e2 decltbl
  | _ -> ()
and check_access acc decltbl = 
  match acc.node with
      Ast.AccVar ide -> set_used ide decltbl
    | Ast.AccDeref e -> check_expr e decltbl
    | Ast.AccIndex (accArr, e) -> 
        check_access accArr decltbl; 
        check_expr e decltbl
and check_block queue decltbl = function
    [] -> false
  | [{ node = Ast.Stmt(stmt); _ }] -> 
      check_deadcode queue stmt decltbl
  | { node = Ast.Stmt(stmt); _ }::next::tl ->
      if check_deadcode queue stmt decltbl then (
        Queue.add (Warning.create "Unreacheable line" next.loc) queue;
        true
      ) else check_block queue decltbl (next::tl)
  | { node = Ast.Dec { vname; init; _ }; loc }::tl ->
      Symbol_table.add_entry vname ({ used = false; isfun = false; loc; }) decltbl;
      (match init with Some e -> check_expr e decltbl | None -> ());
      check_block queue decltbl tl

let detect (Ast.Prog(topdecls)) (Ast.Prog(externals)) =
  let queue = Queue.create() in
  let decltbl = Symbol_table.empty_table() in
  (* Utility function to add a top declaration to the symbol table *)
  let declare_topdecl topdecl = 
    let (ide, isfun, loc) = match topdecl with
      { node = Ast.Vardec { vname; _ }; loc } ->
      (vname, false, loc)
    | { node = Ast.Fundecl { fname; _ }; loc } ->
      (fname, true, loc)
    in
    Symbol_table.add_entry ide ({ used = false; isfun; loc; }) decltbl
  in
  (* Declare external top declarations *)
  List.iter declare_topdecl externals;
  (* Add each top declaration *)
  List.iter declare_topdecl topdecls;
  (* Don't report main function as unused *)
  set_used "main" decltbl;
  (* Don't report run-time support library as unused *)
  Rtsupport.iter (function
      Ast.Vardec(vardecl) ->
      set_used vardecl.vname decltbl
    | Ast.Fundecl(fundecl) ->
      set_used fundecl.fname decltbl  
  );
  (* Detect dead code of each function *)
  List.iter (fun td ->
    match td.node with
      Ast.Fundecl { body = Some(blk); _ } ->
        check_deadcode queue blk decltbl |> ignore
    | _ -> ()
  ) topdecls;
  (* Detect and enqueue dead code from the global scope *)
  enqueue_deadcode_of_block queue decltbl;
  (* Build a list of the detected deadcode *)
  List.of_seq (Queue.to_seq queue)