open Ast
exception Syntax_error of Location.lexeme_pos * string

let error_message state =
  try
    let msg = match ParserMessages.message state with
      "<YOUR SYNTAX ERROR MESSAGE HERE>\n" -> "Parsing error"
    | msg -> msg
  in
    Printf.sprintf "(state %d) %s" state msg
  with Not_found ->
    Printf.sprintf "Unknown syntax error (in state %d)" state

let parse filename scanner lexbuf =
  Lexing.set_filename lexbuf filename;
  try
    Parser.program scanner lexbuf
  with
  | Parser.Error state -> 
    let message = error_message state in
    let position = Location.to_lexeme_position lexbuf in
    raise(Syntax_error(position, message))

let parse_llvm_module filename llmodule =
  let node n = {
    loc = Location.dummy_code_pos_from filename;
    node = n;
  } in
  let ast_typ_of lltyp =
    match Codegen.get_ast_typ lltyp with
      Some t -> t
    | None ->
      let typstr = Llvm.string_of_lltype lltyp in
      raise(Syntax_error(
        Location.dummy_lexeme_pos_from(filename), "Unsupported type: " ^ typstr
      ));
  in
  let globals = Llvm.fold_left_globals (fun agg llval ->
    let vname = Llvm.value_name llval in
    let typ = llval |> Llvm.type_of |> Llvm.element_type |> ast_typ_of  in
    (node (Ast.Vardec({ vname; init = None; typ })))::agg
  ) [] llmodule in
  let topdecls = Llvm.fold_left_functions (fun agg llfundef ->
    let fname = Llvm.value_name llfundef in
    let rettyp = llfundef |> Llvm.type_of |> Llvm.return_type |> Llvm.element_type |> ast_typ_of in
    let formals = Array.mapi (fun i llp ->
      (ast_typ_of (Llvm.type_of llp), "%"^string_of_int(i))
    ) (Llvm.params(llfundef)) |> Array.to_list in
    (node (Ast.Fundecl({ typ = rettyp; fname; formals; body = None; })))::agg
  ) globals llmodule in
  Ast.Prog(topdecls)
