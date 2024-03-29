open Microc

let handle_syntatic_error source lexeme_pos msg = 
  let lines = String.split_on_char '\n' source in 
  let line = List.nth lines (lexeme_pos.Location.line - 1) in
  let prefix = String.make (lexeme_pos.Location.start_column - 1) ' ' in 
  let middle = String.make (lexeme_pos.Location.end_column - lexeme_pos.Location.start_column + 1) '^' in 
  Printf.eprintf "\n*** Error at line %d.\n%s\n%s%s\n*** %s\n\n" lexeme_pos.Location.line line prefix middle msg

let handle_semantic_error source code_pos msg = 
  let lines = 
    String.split_on_char '\n' source |>
    List.filteri (fun line _ -> 
      code_pos.Location.start_line-1 <= line && line <= code_pos.Location.end_line-1)  
  in
  let length = List.length lines in 
  if length = 1 then
    let line = List.hd lines in  
    let prefix = String.make (code_pos.Location.start_column - 1) ' ' in 
    let middle = String.make (code_pos.Location.end_column - code_pos.Location.start_column + 1) '^' in 
    Printf.eprintf "\n*** Error at line %d.\n%s\n%s%s\n*** %s\n\n" code_pos.Location.start_line line prefix middle msg
  else
    let text = 
      lines |>
      List.filteri (fun i _ -> i < 5) |>
      String.concat "\n" 
    in 
    Printf.eprintf "\n*** Error at lines %d-%d.\n%s\n*** %s\n\n" code_pos.Location.start_line (code_pos.Location.start_line + 5) text msg


let load_file filename =
  let ic = open_in filename in 
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let process_source filename = 
  let source = load_file filename in 
  let lexbuf = Lexing.from_string ~with_positions:true source in 
  try
    lexbuf |> Parsing.parse filename Scanner.next_token |>
    (fun program -> Semantic_analysis.type_check program (Ast.Prog([])) true) |>
    Ast.show_program |>
    Printf.printf "Semantic analysis succeded!\n\n%s\n"  
  with 
  | Scanner.Lexing_error (pos, msg)
  | Parsing.Syntax_error (pos,msg) -> 
    handle_syntatic_error source pos msg
  | Sem_error.Semantic_error (pos, msg) -> 
    handle_semantic_error source pos msg  

let () = 
  Printexc.record_backtrace true;
  let usage_msg = Printf.sprintf "%s <file>" (Sys.argv.(0)) in 
  let filename = ref "" in 
  Arg.parse [] (fun fname -> filename := fname) usage_msg;
  if String.equal !filename "" then
    Arg.usage [] usage_msg
  else
    process_source !filename  