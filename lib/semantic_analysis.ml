open Ast

exception Semantic_error of Location.code_pos * string

type symbol = 
  (* variable name, variable type *)
    Variable of Ast.identifier * Ast.typ 
  (* function name, return type, list of formals *)
  | Function of string * Ast.typ * (Ast.typ * Ast.identifier) list

let add_symbol tbl sym loc = 
  let ide = 
    match sym with
      Variable (ide, _)       -> ide
    | Function (fname, _, _)  -> fname
  in 
  try
    Symbol_table.add_entry ide sym tbl
  with
    | Symbol_table.DuplicateEntry(entry) -> 
        let error_msg = Printf.sprintf "Duplicate declaration of '%s'" entry in
        raise(Semantic_error(loc, error_msg))

let analyze_topdecl tbl ann_node = 
  let new_symbol =
    match ann_node.node with
      Ast.Vardec (typ, ide) -> Variable(ide, typ)
    | Ast.Fundecl fun_decl  -> Function(fun_decl.fname, fun_decl.typ, fun_decl.formals)
  in add_symbol tbl new_symbol ann_node.loc

let add_rt_support_functions tbl = 
  let printFun = Function("print", Ast.TypV, [(Ast.TypI, "num")]) in
  let _ = Symbol_table.add_entry "print" printFun tbl in
  let getIntFun = Function("getint", Ast.TypI, []) in
  Symbol_table.add_entry "getint" getIntFun tbl

let type_check (Ast.Prog topdeclList) =
  let symbol_table = Symbol_table.empty_table() in
  let _ = add_rt_support_functions symbol_table in
  List.iter (analyze_topdecl symbol_table) topdeclList;
  Ast.Prog topdeclList