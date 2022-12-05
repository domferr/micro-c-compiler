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
        let error_msg = Printf.sprintf "Duplicate declaration of '%s'" entry in
        raise(Semantic_error((fst loc_and_ide), error_msg))
  
let add_rt_support_functions tbl = 
  let printFun = Function(Location.dummy_code_pos, "print", Ast.TypV, [(Ast.TypI, "num")]) in
  let _ = Symbol_table.add_entry "print" printFun tbl in
  let getIntFun = Function(Location.dummy_code_pos, "getint", Ast.TypI, []) in
  Symbol_table.add_entry "getint" getIntFun tbl

let check_main_function_pass topdeclList =
  let checker ann_node =
    match ann_node.node with
      Ast.Vardec (_, "main") -> raise(Semantic_error(
        ann_node.loc, 
        "Cannot declare 'main' variable: this name is reserved for the 'main' function")
      )
    | Ast.Fundecl { typ = Ast.TypI; fname = "main"; formals = []; body = _ } -> true
    | Ast.Fundecl { typ = Ast.TypV; fname = "main"; formals = []; body = _ } -> true
    | Ast.Fundecl { typ = _; fname = "main"; formals = _; body = _ } -> raise(Semantic_error(
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

let rec check_stmt ret_typ stmt symbol_table =
  match stmt.node with 
    Ast.If (_, thbr, elbr) ->
      (* todo check guard is boolean *) 
      Symbol_table.begin_block symbol_table;
      let th_ret_typ = check_stmt ret_typ thbr symbol_table in
      Symbol_table.end_block symbol_table;
      Symbol_table.begin_block symbol_table;
      let el_ret_typ = check_stmt ret_typ elbr symbol_table in
      Symbol_table.end_block symbol_table;
      (match th_ret_typ, el_ret_typ with
      | Some _, None  -> th_ret_typ
      | None , Some _ -> el_ret_typ
      | _ , _         -> th_ret_typ)
  | Ast.While (_, stmt) -> 
      Symbol_table.begin_block symbol_table;
      let while_ret = check_stmt ret_typ stmt symbol_table in
      Symbol_table.end_block symbol_table;
      while_ret
  | Ast.Expr _ -> Some Ast.TypV
  | Ast.Block stmtordecList -> 
    Symbol_table.begin_block symbol_table;
    let checker ann_node = 
      match ann_node.node with
        Ast.Stmt stmt -> 
          check_stmt ret_typ stmt symbol_table
      | Ast.Dec (typ, ide) -> 
        st_add_symbol symbol_table (Variable(ann_node.loc, ide, typ));
        None
    in 
    let ret_list = List.map checker stmtordecList in
    Symbol_table.end_block symbol_table;
    let elem_typ agg typ = 
      match agg, typ with
        _, None -> agg
      | Some a, Some b when a != b -> 
        raise(Semantic_error(
          stmt.loc,
          "THIS SHOULDN'T HAPPEN!!!!"
        ))
      | _, Some _ -> typ
    in
    List.fold_left elem_typ None ret_list
  | Ast.Return None when ret_typ = Ast.TypV -> Some ret_typ
  | Ast.Return None -> raise(Semantic_error(
      stmt.loc,
      "Missing return value"
    ))
  | Ast.Return (Some _) -> Some ret_typ (* todo *)
 
let type_check (Ast.Prog topdeclList) =
  let symbol_table = Symbol_table.empty_table() in
  (* Add runtime support functions before anything else *)
  add_rt_support_functions symbol_table;
  (* Check main function and its return type *)
  check_main_function_pass topdeclList;
  (* Analyze each top declaration *)
  let st_add_topdecl_node tbl ann_node = 
    let new_symbol = match ann_node.node with
      Ast.Vardec (typ, ide) -> Variable(ann_node.loc, ide, typ)
    | Ast.Fundecl fun_decl  -> Function(ann_node.loc, fun_decl.fname, fun_decl.typ, fun_decl.formals)
    in st_add_symbol tbl new_symbol
  in
  List.iter (fun ann_node ->
    (* Add global variable or function *)
    st_add_topdecl_node symbol_table ann_node;
    match ann_node.node with
      Ast.Vardec _ -> ()
    | Ast.Fundecl { typ = ret_typ; fname = _; formals = args; body = body_stmt } ->
        Symbol_table.begin_block symbol_table;
        (* Add each formal to the symbol table *)
        List.iter (fun arg -> 
          st_add_topdecl_node symbol_table { 
            node = Ast.Vardec(fst arg, snd arg); 
            loc = ann_node.loc 
          }
        ) args;
        (match check_stmt ret_typ body_stmt symbol_table with
            None -> raise(Semantic_error(
              ann_node.loc,
              "Missing return statement"
            ))
          | Some _ -> ()
        );
        Symbol_table.end_block symbol_table;
  ) topdeclList;
  (* Finally return AST *)
  Ast.Prog topdeclList