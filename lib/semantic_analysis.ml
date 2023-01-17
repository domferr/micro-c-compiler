open Ast

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
    | Symbol_table.DuplicateEntry(entry) -> Sem_error.raise_duplicate_declaration (fst loc_and_ide) entry

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
      Ast.Vardec (_, "main") -> Sem_error.raise_variable_main ann_node
    | Ast.Fundecl { typ = Ast.TypI; fname = "main"; formals = []; _ } -> true
    | Ast.Fundecl { typ = Ast.TypV; fname = "main"; formals = []; _ } -> true
    | Ast.Fundecl { fname = "main"; _ } -> Sem_error.raise_invalid_def_main ann_node
    | _ -> false
  in
  let has_main = List.exists checker topdeclList in
  if has_main then () else Sem_error.raise_missing_main_definition()

let rec type_check_expr expr symbtbl =
  let rec type_check_access acc = match acc.node with
    Ast.AccVar ide -> 
      (match Symbol_table.lookup ide symbtbl with
          Some Variable(_, _, t) -> Some t
        | _ -> Sem_error.raise_variable_not_declared acc ide)
  | Ast.AccDeref e -> 
    (match type_check_expr e symbtbl with
        Some (Ast.TypP(t)) -> Some t
      | _ -> Sem_error.raise_invalid_pointer_deref acc)
  | Ast.AccIndex (accArr, e) -> (match type_check_access accArr with
      Some (Ast.TypA(t, _)) -> 
        (match type_check_expr e symbtbl with
            Some Ast.TypI -> Some t
          | _ -> Sem_error.raise_invalid_array_index_type acc)
    | _ -> Sem_error.raise_invalid_array_index acc)
  in 
  match expr.node with 
    Ast.Access acc -> type_check_access acc
  | Ast.Assign (acc, ex) -> 
      let acc_typ = type_check_access acc in
      let exp_typ = type_check_expr ex symbtbl in
      (match acc_typ, exp_typ with
          Some Ast.TypA(a1, a2), Some Ast.TypA(e1, e2) -> 
            Sem_error.raise_invalid_assignment_type expr (Ast.TypA(a1, a2)) (Ast.TypA(e1, e2))
        | Some a_typ, Some e_typ when a_typ != e_typ -> 
            Sem_error.raise_invalid_assignment_type expr a_typ e_typ
        | Some _, Some _ -> acc_typ
        | _ -> Sem_error.raise_invalid_assignment expr)
  | Ast.Addr acc -> 
    (match type_check_access acc with
      Some t -> Some (Ast.TypP(t))
    | _ -> Sem_error.raise_invalid_pointer_type expr)
  | Ast.ILiteral _ -> Some Ast.TypI
  | Ast.CLiteral _ -> Some Ast.TypC
  | Ast.BLiteral _ -> Some Ast.TypB
  | Ast.UnaryOp (op, ex) -> 
      let expr_typ = type_check_expr ex symbtbl in
      (match op, expr_typ with
        Neg, Some Ast.TypI -> Some Ast.TypI
      | Not, Some Ast.TypB -> Some Ast.TypB
      | _ -> Sem_error.raise_invalid_unary_op expr)
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
      in if (left_typ = right_typ && left_typ = Some (fst op_type)) then 
          Some (snd op_type) 
        else 
          Sem_error.raise_invalid_binary_op expr
  | Ast.Call (ide, exprList) -> 
      match Symbol_table.lookup ide symbtbl with
        Some Function(_, _, ret_typ, formalsList) ->
          (try
            List.iter2 (fun ex formal ->
              let formal_typ = fst formal in
              let given_typ = type_check_expr ex symbtbl in
              (match given_typ, formal_typ with
                  Some Ast.TypA(t1, _), Ast.TypA(t2, _) when t1 = t2 -> ()
                | Some t1, t2 when t1 = t2 -> ()
                | Some t1, t2 -> Printf.printf "%s:%s\n" (Ast.show_typ t1) (Ast.show_typ t2); Sem_error.raise_invalid_function_arg_type ex t2 t1
                | _ -> Sem_error.raise_invalid_argument ex)
            ) exprList formalsList;
            Some ret_typ
          with Invalid_argument _ -> 
            let declArgsLen = List.length formalsList in
            let callArgsLen = List.length exprList in
            Sem_error.raise_invalid_arguments_number expr ide declArgsLen callArgsLen)
      | _ -> Sem_error.raise_missing_fun_declaration expr ide

let rec type_check_stmt ret_typ stmt symbtbl =
  let type_check_guard guard = 
    (match type_check_expr guard symbtbl with
      Some Ast.TypB -> ()
    | None -> Sem_error.raise_missing_guard guard
    | _ -> Sem_error.raise_invalid_guard_type guard
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
  | Ast.Return None -> Sem_error.raise_missing_return_value stmt
  | Ast.Return (Some expr) -> 
      match type_check_expr expr symbtbl with
        Some expr_typ when expr_typ = ret_typ -> Some ret_typ
      | _ -> Sem_error.raise_invalid_return_type stmt

let type_check_function_decl node fun_decl symbtbl =
  st_add_symbol symbtbl (Function(node.loc, fun_decl.fname, fun_decl.typ, fun_decl.formals));
  (* Add each formal to the symbol table *)
  List.iter (fun arg -> st_add_symbol symbtbl (Variable(node.loc, snd arg, fst arg))) fun_decl.formals;
  (* Check function body *)
  match type_check_stmt fun_decl.typ fun_decl.body symbtbl with
      None when fun_decl.typ != Ast.TypV -> Sem_error.raise_missing_return node
    | _ -> ()


(* TODO: check that global variables are initialized with constant values *)
let type_check (Ast.Prog topdeclList) =
  let symbtbl = Symbol_table.empty_table() in
  (* Check main function and its return type *)
  check_main_function_pass topdeclList;
  (* Add runtime support functions before anything else *)
  List.iter (fun fn -> Symbol_table.add_entry (fst fn) (snd fn) symbtbl) rt_support_functions;
  (* Analyze each top declaration *)
  List.iter (fun ann_node ->
    match ann_node.node with
        Ast.Vardec (typ, ide) -> st_add_symbol symbtbl (Variable(ann_node.loc, ide, typ))
      | Ast.Fundecl fun_decl  -> type_check_function_decl ann_node fun_decl symbtbl
  ) topdeclList;
  (* Finally return AST *)
  Ast.Prog topdeclList