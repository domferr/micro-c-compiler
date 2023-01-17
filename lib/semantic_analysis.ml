open Ast

exception Semantic_error of Location.code_pos * string

type symbol = 
  (* variable pos, name, variable type *)
    Variable of Location.code_pos * Ast.identifier * Ast.typ 
  (* function pos, name, return type, list of formals *)
  | Function of Location.code_pos * string * Ast.typ * (Ast.typ * Ast.identifier) list

let st_add_symbol tbl new_symbol =
  let loc_and_ide = match new_symbol with
      Variable (loc, ide, _)       -> (loc, ide)
    | Function (loc, fname, _, _)  -> (loc, fname)
  in 
  try
    Symbol_table.add_entry (snd loc_and_ide) new_symbol tbl
  with
    | Symbol_table.DuplicateEntry(entry) -> 
        raise(Semantic_error(
          (fst loc_and_ide), 
          Printf.sprintf "Duplicate declaration of '%s'" entry
        ))

let rt_support_functions = [
  "print", Function(
    Location.dummy_code_pos, 
    "print", 
    Ast.TypV, 
    [(Ast.TypI, "num")]
  );
  "getint", Function(
    Location.dummy_code_pos, 
    "getint", 
    Ast.TypI, 
    []
)]

let check_main_function_pass topdeclList =
  let checker ann_node =
    match ann_node.node with
      Ast.Vardec (_, "main") -> raise(Semantic_error(
        ann_node.loc, 
        "Cannot declare 'main' variable: this name is reserved for the 'main' function")
      )
    | Ast.Fundecl { typ = Ast.TypI; fname = "main"; formals = []; _ } -> true
    | Ast.Fundecl { typ = Ast.TypV; fname = "main"; formals = []; _ } -> true
    | Ast.Fundecl { fname = "main"; _ } -> raise(Semantic_error(
        ann_node.loc, 
        "Invalid definition of the 'main' function. The signature must be 'int main()' or 'void main()'")
      )
    | _ -> false
  in
  let has_main = List.exists checker topdeclList in
  if has_main then ()
  else raise(Semantic_error(
    Location.dummy_code_pos, 
    "Missing definition of the 'main' function")
  )

let rec type_check_expr expr symbtbl =
  let rec type_check_access acc = match acc.node with
    Ast.AccVar ide -> 
      (match Symbol_table.lookup ide symbtbl with
          Some Variable(_, _, t) -> Some t
        | _ -> raise(Semantic_error(
          acc.loc, 
          Printf.sprintf "Variable '%s' not declared" ide)
        ))
  | Ast.AccDeref e -> 
    (match type_check_expr e symbtbl with
        Some (Ast.TypP(t)) -> Some t
      | _ -> raise(Semantic_error(acc.loc, "Invalid pointer dereferencing")))
  | Ast.AccIndex (accArr, e) -> (match type_check_access accArr with
      Some (Ast.TypA(t, _)) -> 
        (match type_check_expr e symbtbl with
            Some Ast.TypI -> Some t
          | _ -> raise(Semantic_error(acc.loc, "Array index must be and integer")))
    | _ -> raise(Semantic_error(acc.loc, "Invalid array indexing")))
  in 
  match expr.node with 
    Ast.Access acc -> type_check_access acc
  | Ast.Assign (acc, ex) -> 
      let acc_typ = type_check_access acc in
      let exp_typ = type_check_expr ex symbtbl in
      let assign_err left_typ right_typ = Semantic_error(
        expr.loc, 
        Printf.sprintf "Invalid assignment of %s to %s" (Ast_descr.descr_typ right_typ) (Ast_descr.descr_typ left_typ))
      in
      (match acc_typ, exp_typ with
          Some Ast.TypA(a1, a2), Some Ast.TypA(e1, e2) -> raise(assign_err (Ast.TypA(a1, a2)) (Ast.TypA(e1, e2)))
        | Some a_typ, Some e_typ when a_typ != e_typ -> raise(assign_err a_typ e_typ)
        | Some _, Some _ -> acc_typ
        | _ -> raise(Semantic_error(
          expr.loc, 
          "Invalid assignment")
        ))
  | Ast.Addr acc -> 
    (match type_check_access acc with
      Some t -> Some (Ast.TypP(t))
    | _ -> raise(Semantic_error(
      expr.loc, 
      "Invalid pointer type")
    ))
  | Ast.ILiteral _ -> Some Ast.TypI
  | Ast.CLiteral _ -> Some Ast.TypC
  | Ast.BLiteral _ -> Some Ast.TypB
  | Ast.UnaryOp (op, ex) -> 
      let expr_typ = type_check_expr ex symbtbl in
      (match op, expr_typ with
        Neg, Some Ast.TypI -> Some Ast.TypI
      | Not, Some Ast.TypB -> Some Ast.TypB
      | _ -> raise(Semantic_error(expr.loc, "Invalid unary operation")))
  | Ast.BinaryOp (op, left, right) -> 
      let left_typ = type_check_expr left symbtbl in
      let right_typ = type_check_expr right symbtbl in
      let op_type = match op with 
          Ast.Add 
        | Ast.Sub 
        | Ast.Mult
        | Ast.Div
        | Ast.Mod -> Ast.TypI, Ast.TypI (* op between ints returns int *)
        | Ast.And
        | Ast.Or -> Ast.TypB, Ast.TypB (* op between bools returns bool *)
        | _ -> Ast.TypI, Ast.TypB (* ints comparison returns bool *)
      in if (left_typ = right_typ && left_typ = Some (fst op_type)) then Some (snd op_type) else raise(Semantic_error(
        expr.loc, 
        "Invalid binary operation")
      )
  | Ast.Call (ide, exprList) -> 
      match Symbol_table.lookup ide symbtbl with
        Some Function(_, _, ret_typ, formalsList) ->
          (try
            List.iter2 (fun ex formal ->
              let formal_typ = fst formal in
              (match type_check_expr ex symbtbl with
                Some t when t = formal_typ -> ()
                | Some t -> raise(Semantic_error(ex.loc, Printf.sprintf "Invalid argument, expected %s but found %s" (Ast_descr.descr_typ formal_typ) (Ast_descr.descr_typ t)))
                | _ -> raise(Semantic_error(ex.loc, "Invalid argument")))
            ) exprList formalsList;
            Some ret_typ
          with Invalid_argument _ -> 
            let declArgsLen = List.length formalsList in
            let callArgsLen = List.length exprList in
            raise(Semantic_error(
              expr.loc,
              Printf.sprintf "Function '%s' expects %d arguments but here %d arguments are passed to it" 
                              ide declArgsLen callArgsLen
            )))
      | _ -> raise(Semantic_error(
          expr.loc,
          Printf.sprintf "Missing declaration of function '%s'" ide
        ))

let rec type_check_stmt ret_typ stmt symbtbl =
  let type_check_guard guard = 
    (match type_check_expr guard symbtbl with
      Some Ast.TypB -> ()
    | None -> raise(Semantic_error(guard.loc, "Missing guard"))
    | _ -> raise(Semantic_error(guard.loc, "Guard's type is not boolean"))
  ) in
  match stmt.node with 
    Ast.If (guard, thbr, elbr) ->
      (* Type check guard *)
      type_check_guard guard;
      (* Type check then branch *)
      Symbol_table.begin_block symbtbl;
      let th_ret_typ = type_check_stmt ret_typ thbr symbtbl in
      Symbol_table.end_block symbtbl;
      (* Type check else branch if any *)
      Symbol_table.begin_block symbtbl;
      let el_ret_typ = type_check_stmt ret_typ elbr symbtbl in
      Symbol_table.end_block symbtbl;
      (* Compute if statement's return type if any *)
      (match th_ret_typ, el_ret_typ with
        None, None  -> None
      | _, _        -> th_ret_typ)
  | Ast.While (guard, stmt) -> 
      (* Type check guard *)
      type_check_guard guard;
      (* Type check while body *)
      Symbol_table.begin_block symbtbl;
      let while_ret = type_check_stmt ret_typ stmt symbtbl in
      Symbol_table.end_block symbtbl;
      while_ret
  | Ast.Expr e -> type_check_expr e symbtbl
  | Ast.Block stmtordecList -> 
      Symbol_table.begin_block symbtbl;
      let ret_list = List.filter_map (fun ann_node ->
        match ann_node.node with
          Ast.Stmt stmt -> type_check_stmt ret_typ stmt symbtbl
        | Ast.Dec (typ, ide) -> 
          st_add_symbol symbtbl (Variable(ann_node.loc, ide, typ));
          None
      ) stmtordecList in 
      (match ret_list with
        t::_  -> Some t
      | _     -> None)
  | Ast.Return None when ret_typ = Ast.TypV -> Some ret_typ
  | Ast.Return None -> raise(Semantic_error(stmt.loc, "Missing return value"))
  | Ast.Return (Some expr) -> 
      match type_check_expr expr symbtbl with
        Some expr_typ when expr_typ = ret_typ -> Some ret_typ
      | _ -> raise(Semantic_error(stmt.loc, "Invalid return type"))

let type_check_function_decl loc fun_decl symbtbl =
  st_add_symbol symbtbl (Function(loc, fun_decl.fname, fun_decl.typ, fun_decl.formals));
  Symbol_table.begin_block symbtbl;
  (* Add each formal to the symbol table *)
  List.iter (fun arg -> st_add_symbol symbtbl (Variable(loc, snd arg, fst arg))) fun_decl.formals;
  (* Check function body *)
  (match type_check_stmt fun_decl.typ fun_decl.body symbtbl with
      None when fun_decl.typ != Ast.TypV -> raise(Semantic_error(
        loc, "Missing return statement")
      )
    | _ -> ()
  );
  Symbol_table.end_block symbtbl

(* TODO: check that global variables are initialized with constant values *)
let type_check (Ast.Prog topdeclList) =
  let symbtbl = Symbol_table.empty_table() in
  (* Add runtime support functions before anything else *)
  List.iter (fun fn -> Symbol_table.add_entry (fst fn) (snd fn) symbtbl) rt_support_functions;
  (* Check main function and its return type *)
  check_main_function_pass topdeclList;
  (* Analyze each top declaration *)
  List.iter (fun ann_node ->
    match ann_node.node with
        Ast.Vardec (typ, ide) -> st_add_symbol symbtbl (Variable(ann_node.loc, ide, typ))
      | Ast.Fundecl fun_decl  -> type_check_function_decl ann_node.loc fun_decl symbtbl
  ) topdeclList;
  (* Finally return AST *)
  Ast.Prog topdeclList