open Ast

exception Semantic_error of Location.code_pos * string

type symbol = 
  (* variable pos, name, variable type *)
    Variable of Location.code_pos * Ast.identifier * Ast.typ 
  (* function pos, name, return type, list of formals *)
  | Function of Location.code_pos * string * Ast.typ * (Ast.typ * Ast.identifier) list

let st_add_symbol tbl sym = 
  let loc_and_ide = 
    match sym with
      Variable (loc, ide, _)       -> (loc, ide)
    | Function (loc, fname, _, _)  -> (loc, fname)
  in 
  try
    Symbol_table.add_entry (snd loc_and_ide) sym tbl
  with
    | Symbol_table.DuplicateEntry(entry) -> 
        let error_msg = Printf.sprintf "Duplicate declaration of '%s'" entry in
        raise(Semantic_error((fst loc_and_ide), error_msg))

let add_rt_support_functions tbl = 
  let printFun = Function(Location.dummy_code_pos, "print", Ast.TypV, [(Ast.TypI, "num")]) in
  let _ = Symbol_table.add_entry "print" printFun tbl in
  let getIntFun = Function(Location.dummy_code_pos, "getint", Ast.TypI, []) in
  Symbol_table.add_entry "getint" getIntFun tbl

let add_topdecl_pass tbl topdeclList =
  let add_global tbl ann_node = 
    let new_symbol =
      match ann_node.node with
        Ast.Vardec (typ, ide) -> Variable(ann_node.loc, ide, typ)
      | Ast.Fundecl fun_decl  -> Function(ann_node.loc, fun_decl.fname, fun_decl.typ, fun_decl.formals)
    in st_add_symbol tbl new_symbol
  in
  List.iter (add_global tbl) topdeclList

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

let type_check (Ast.Prog topdeclList) =
  let symbol_table = Symbol_table.empty_table() in
  (* Add runtime support functions before any variable or function *)
  add_rt_support_functions symbol_table;
  (* Add global variables and functions *)
  add_topdecl_pass symbol_table topdeclList;
  (* Check main function and its return type *)
  check_main_function_pass topdeclList;
  (* Finally return AST *)
  Ast.Prog topdeclList