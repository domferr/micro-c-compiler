open Ast

exception Semantic_error of Location.code_pos * string

type symbol = 
  (* variable pos, name, variable type *)
    Variable of Location.code_pos * Ast.identifier * Ast.typ 
  (* function pos, name, return type, list of formals *)
  | Function of Location.code_pos * string * Ast.typ * (Ast.typ * Ast.identifier) list

let add_symbol tbl sym = 
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

let check_main_function tbl =
  let main_notdecl_error = Semantic_error(
    Location.dummy_code_pos, 
    "Missing definition of the 'main' function") 
  in
  match Symbol_table.lookup "main" tbl with
      None -> raise(main_notdecl_error)
    | Some(Function(_, "main", Ast.TypI, [])) -> () (* int main() *)
    | Some(Function(_, "main", Ast.TypV, [])) -> () (* void main() *)
    | Some(Function(loc, "main", _, _)) -> raise(Semantic_error(
        loc, 
        "Invalid definition of the 'main' function. The signature must be 'int main()' or 'void main()'"))
    | _ -> raise(main_notdecl_error)

let type_check (Ast.Prog topdeclList) =
  let symbol_table = Symbol_table.empty_table() in
  let _ = add_rt_support_functions symbol_table in
  let add_global tbl ann_node = 
    let new_symbol =
      match ann_node.node with
        Ast.Vardec (typ, ide) -> Variable(ann_node.loc, ide, typ)
      | Ast.Fundecl fun_decl  -> Function(ann_node.loc, fun_decl.fname, fun_decl.typ, fun_decl.formals)
    in add_symbol tbl new_symbol
  in
  List.iter (add_global symbol_table) topdeclList;
  (* Check main function and its return type *)
  check_main_function symbol_table;
  Ast.Prog topdeclList