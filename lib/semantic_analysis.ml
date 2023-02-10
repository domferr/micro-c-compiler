open Ast

(** Definition of symbol, which is added into the symbol table *)
type symbol = 
  (* variable pos, name, variable type *)
    Variable of Location.code_pos * Ast.typ 
  (* function pos, name, return type, list of formals *)
  | Function of Location.code_pos * Ast.typ * (Ast.typ * Ast.identifier) list

(** Function to add the symbol into the symbol table. 
   It wraps the add_entry function to handle the raise of duplicate
   declaration exception *)
let st_add_symbol tbl ide new_symbol =
  let loc = match new_symbol with
    Variable(l, _) | Function(l, _, _) -> l
  in
  try Symbol_table.add_entry ide new_symbol tbl with
  | Symbol_table.DuplicateEntry(entry) -> 
    Sem_error.raise_duplicate_declaration loc entry

let declare_function symbtbl loc { typ; fname; formals; _ } =
  st_add_symbol symbtbl fname (Function(loc, typ, formals))

let declare_variable symbtbl loc { typ; vname; _ } = 
  st_add_symbol symbtbl vname (Variable(loc, typ))

(** Checks whether variable type is compatible with assignment type and 
   returns variable type, raises a Semantic_error exception otherwise *)
let rec type_check_assign node acc_typ exp_typ =
  match acc_typ, exp_typ with
    Ast.TypA(_), Ast.TypA(_) -> false
  | Ast.TypP t1, Ast.TypA(t2, _) -> 
    type_check_assign node (Ast.TypP(t1)) t2
  | Ast.TypP ptr1, Ast.TypP ptr2 -> ptr1 = ptr2
  | Ast.TypP _, Ast.TypNull -> true
  | Ast.TypP t1, t2 -> t1 = t2
  | a_typ, e_typ -> a_typ = e_typ

let type_check_binary_op expr binaryop left_typ right_typ =
  match binaryop with 
  (* math op between ints returns int *)
    Ast.Add | Ast.Sub | Ast.Mult | Ast.Div 
  | Ast.Mod -> 
    (match left_typ, right_typ with
        Ast.TypI, Ast.TypI -> Ast.TypI
      | Ast.TypI, Ast.TypP(_) -> right_typ
      | Ast.TypP(_), Ast.TypI -> left_typ
      | _ -> Sem_error.raise_invalid_binary_op expr)
  (* logical op between bools returns bool *)
  | Ast.And | Ast.Or -> if left_typ = right_typ && left_typ = Ast.TypB then Ast.TypB 
                        else Sem_error.raise_invalid_binary_op expr
  (* comparison op *)
  | _ -> (match left_typ, right_typ with
            t1, t2 when t1 = t2 -> () (* between same type *)
          | Ast.TypP(_), Ast.TypNull -> ()
          | Ast.TypNull, Ast.TypP(_) -> ()
          | Ast.TypA(_), Ast.TypNull -> ()
          | Ast.TypNull, Ast.TypA(_) -> ()
          | t1, t2 -> Sem_error.raise_invalid_binary_comparison expr t1 t2);
          Ast.TypB

(** Checks whether given type is compatible with required type. *)
let rec are_types_compatible = function
    Ast.TypA(t1, _), Ast.TypA(t2, None) -> 
      are_types_compatible (t1, t2)
  | Ast.TypA(t1, Some s1), Ast.TypA(t2, Some s2) -> 
    if s1 = s2 then are_types_compatible (t1, t2)
    else false
  | Ast.TypP(t1), Ast.TypA(t2, _) -> 
    are_types_compatible (t1, t2)
  | Ast.TypA(t1, _), Ast.TypP(t2) -> 
    are_types_compatible (t1, t2)
  | Ast.TypNull, Ast.TypP(_) -> true
  | Ast.TypNull, Ast.TypA(_) -> true
  | t1, t2 -> t1 = t2

(** Type checker function for expr. Returns the expression type *)
let rec type_check_expr expr symbtbl =
  match expr.node with 
      Ast.Access acc ->
        type_check_access acc symbtbl
    | Ast.Assign (acc, ex) -> 
        let acc_typ = type_check_access acc symbtbl in
        let exp_typ = type_check_expr ex symbtbl in
        if type_check_assign expr acc_typ exp_typ then acc_typ
        else Sem_error.raise_invalid_assignment_type ex acc_typ exp_typ
    | Ast.Addr acc -> 
        let acctyp = type_check_access acc symbtbl in
        let rec addrof = function
            Ast.TypA(t, _) -> Ast.TypP(addrof t)
          | any -> Ast.TypP(any)
        in
        addrof acctyp
    | Ast.ILiteral _  -> Ast.TypI
    | Ast.CLiteral _  -> Ast.TypC
    | Ast.BLiteral _  -> Ast.TypB
    | Ast.Null        -> Ast.TypNull
    | Ast.UnaryOp (op, ex) -> 
        let expr_typ = (match op, ex.node with
            Ast.Neg, _ | Ast.Not, _
          | _, Ast.Access(_) -> type_check_expr ex symbtbl
          | _ -> Sem_error.raise_invalid_unary_op_access expr) in
        (match op, expr_typ with
          Ast.Neg, Ast.TypI 
        | Ast.PreIncr, Ast.TypI
        | Ast.PostIncr, Ast.TypI
        | Ast.PreDecr, Ast.TypI
        | Ast.PostDecr, Ast.TypI -> Ast.TypI (* op between ints returns int *)
        | Ast.Not, Ast.TypB -> Ast.TypB (* negating a bool returns bool *)
        | _ -> Sem_error.raise_invalid_unary_op expr)
    | Ast.BinaryOp (op, left, right) -> 
        let left_typ = type_check_expr left symbtbl in
        let right_typ = type_check_expr right symbtbl in
        type_check_binary_op expr op left_typ right_typ
    | Ast.Call (ide, exprList) ->
        match Symbol_table.lookup ide symbtbl with
          Some Function(_, ret_typ, formalsList) ->
            (try  
              List.iter2 (fun ex formal ->
                let formal_typ = fst formal in
                let given_typ = type_check_expr ex symbtbl in
                if are_types_compatible (given_typ, formal_typ) then ()
                else Sem_error.raise_invalid_function_arg_type ex formal_typ given_typ
              ) exprList formalsList;
              ret_typ
            with Invalid_argument _ -> (* the two lists are determined to have different length *)
              let declArgsLen = List.length formalsList in
              let callArgsLen = List.length exprList in
              Sem_error.raise_invalid_arguments_number expr ide declArgsLen callArgsLen)
        | _ -> Sem_error.raise_missing_fun_declaration expr ide
and type_check_access acc symbtbl = 
  match acc.node with
    Ast.AccVar ide -> 
      (match Symbol_table.lookup ide symbtbl with
          Some Variable(_, t) -> t
        | _ -> Sem_error.raise_variable_not_declared acc ide)
    | Ast.AccDeref e -> 
    (match type_check_expr e symbtbl with
        (Ast.TypP(t)) -> t
      | _ -> Sem_error.raise_invalid_pointer_deref acc)
    | Ast.AccIndex (accArr, e) -> 
      (match type_check_access accArr symbtbl with
          Ast.TypA(t, _)
       |  Ast.TypP(t) -> 
        (match type_check_expr e symbtbl with (* ensure the index has int type *)
            Ast.TypI -> t | _ -> Sem_error.raise_invalid_array_index_type acc)
       | _ -> Sem_error.raise_invalid_array_index acc)

(** Type checker function for stmt. Returns the statement type, if any *)
let rec type_check_stmt ret_typ stmt symbtbl =
  let type_check_guard guard = 
    if type_check_expr guard symbtbl = Ast.TypB then ()
    else Sem_error.raise_invalid_guard_type guard
  in
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
  | Ast.Expr e -> Some (type_check_expr e symbtbl)
  | Ast.Block stmtordecList ->
      Symbol_table.begin_block symbtbl;
      let blk_ret = type_check_block ret_typ stmtordecList symbtbl in
      Symbol_table.end_block symbtbl;
      blk_ret
  | Ast.Return None when ret_typ = Ast.TypV -> Some ret_typ
  | Ast.Return None -> Sem_error.raise_missing_return_value stmt
  | Ast.Return (Some expr) -> 
      let exprtyp = type_check_expr expr symbtbl in
      if are_types_compatible (exprtyp, ret_typ) then
        Some(ret_typ)
      else
        Sem_error.raise_invalid_return_type stmt
and type_check_block ret_typ stmtordecList symbtbl = 
  let ret_list = List.filter_map (fun ann_node ->
    match ann_node.node with
      Ast.Stmt stmt -> 
        type_check_stmt ret_typ stmt symbtbl
    | Ast.Dec { typ = Ast.TypA(_, None);  _ } ->
        Sem_error.raise_missing_array_size ann_node 
    | Ast.Dec { typ; vname; init } ->
        type_check_var_decl typ init symbtbl;
        declare_variable symbtbl ann_node.loc { typ; vname; init };       
        None
  ) stmtordecList in
  (match ret_list with
    t::_  -> Some t
  | _     -> None)
(** Check if the variable declaration has an assignment aswell. If that's the
    case it checks if the assignment is valid. Raises Semantic_error if the 
    assignment's type check operation fails *)
and type_check_var_decl typ initexpr symbtbl = 
  match initexpr with
      Some ex ->
        let init_type = type_check_expr ex symbtbl in
        if type_check_assign ex typ init_type then ()
        else Sem_error.raise_invalid_assignment_type ex typ init_type
    | None -> ()

(** Type checker for function declaration. For each function declared, it will 
    type check its formals and body. *)
let type_check_function node fun_decl body symbtbl =
  Symbol_table.begin_block symbtbl;
  (* Checker function to type check declaration of array as parameter of a function 
  int arr[][size1]..[sizeN] is allowed. int arr[][].. is NOT allowed. int arr[] is allowed *)
  let rec validate_array = function
      Ast.TypA(Ast.TypA(t, Some _), _) -> validate_array t
    | Ast.TypA(Ast.TypA(_, None), None) -> 
      Sem_error.raise_missing_multidimensional_array_size node
    | Ast.TypA(t, None) -> validate_array t
    | _ -> ()
  in
  (* Add each formal to the symbol table *)
  List.iter (fun arg ->
    validate_array (fst arg);
    declare_variable symbtbl node.loc ({ typ = fst arg; vname = snd arg; init = None })
  ) fun_decl.formals;
  (* Type check function body *)
  let blk_ret = type_check_block fun_decl.typ body symbtbl in
  Symbol_table.end_block symbtbl;
  if Option.is_none blk_ret && fun_decl.typ != Ast.TypV then
    Sem_error.raise_missing_return node
  else ()

(** A pass to check if the main function is defined properly *)
let check_main_function_pass symbtbl =
  match Symbol_table.lookup "main" symbtbl with
      Some(Variable(loc, _)) -> Sem_error.raise_variable_main loc
    | Some(Function(_, Ast.TypI, [])) -> ()
    | Some(Function(_, Ast.TypV, [])) -> ()
    | Some(Function(loc, _, _)) -> Sem_error.raise_invalid_def_main loc
    | None -> Sem_error.raise_missing_main_definition()

(** Returns if the given expression is made of constants, false otherwise *)
let rec is_constant_expr = function
    Ast.Access(_) | Ast.Call(_) -> false
  | Ast.BinaryOp(_, e1, e2) -> is_constant_expr e1.node && is_constant_expr e2.node
  | Ast.UnaryOp(_, e1) -> is_constant_expr e1.node
  | _ -> true

(** Utility function to add a top declaration to the symbol table *)
let declare_topdecl symbtbl = function
  { loc; node = Ast.Vardec (vardec) } -> 
    declare_variable symbtbl loc vardec
| { loc; node = Ast.Fundecl (fundecl) } ->
    declare_function symbtbl loc fundecl

(** Declare run-time support functions and global variables. It is
    important to note that it may be already included by some
    linked source or object code. Moreover, this functions checks
    if any linked code redeclares run-time support library in a 
    different way. *)
let declare_rts symbtbl =
  let compare_formals f1 f2 = List.equal (fun (t1, _) (t2, _) -> t1 == t2) f1 f2 in
  Rtsupport.iter (fun rts_topdecl ->
    let ide = match rts_topdecl with
      Ast.Vardec (vardec) -> vardec.vname
    | Ast.Fundecl (fundecl) -> fundecl.fname
    (* run time support may be already declared *)
    in match Symbol_table.lookup ide symbtbl, rts_topdecl with
        Some(Function(_, rettyp, formals)), Ast.Fundecl(fundecl)
        when rettyp = fundecl.typ && compare_formals formals fundecl.formals -> () (* already declared *)
      | Some(Variable(_, typ)), Ast.Vardec (vardec)
        when typ = vardec.typ -> () (* already declared *)
      | Some(Function(loc, _, _)), _
      | Some(Variable(loc, _)), _ -> (* already declared but it is different *)
        Sem_error.raise_reserved_for_rts ide loc
      | None, _ -> 
        let loc = Location.dummy_code_pos in
        declare_topdecl symbtbl { loc; node = rts_topdecl} (* not declared before *)
  )

let type_check (Ast.Prog(topdecls)) (Ast.Prog(externals)) requires_main =
  let symbtbl = Symbol_table.empty_table() in
  (* Declare external top declarations *)
  List.iter (declare_topdecl symbtbl) externals;
  (* Declare run-time support *)
  declare_rts symbtbl;
  (* Declare each top declaration *)
  List.iter (declare_topdecl symbtbl) topdecls;
  (* Type check each top declaration *)
  List.iter (fun node ->
    match node.node with
      Ast.Vardec { typ; init; _ } -> 
        (* Ensure global variable is initalized with compile-time constant value *)
        (match init with 
            None -> () 
          | Some(expr) -> 
            if is_constant_expr expr.node then () 
            else Sem_error.raise_not_compiletime_constant expr);
        type_check_var_decl typ init symbtbl |> ignore
    | Ast.Fundecl fun_decl -> (* Ensure that, if the function has a body, it must be a code block *)
        match fun_decl.body with
            Some({ node = Ast.Block(body); _ }) -> 
              type_check_function node fun_decl body symbtbl
          | Some(node) -> Sem_error.raise_invalid_function_body node 
          | None -> () (* If the body is not available, then it is an external function declared somewhere else *)
  ) topdecls;
  (* Check if the main function is declared properly *)
  if requires_main then check_main_function_pass symbtbl;
  (* Return semantically correct program *)
  Ast.Prog(topdecls)