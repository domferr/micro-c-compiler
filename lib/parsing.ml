exception Syntax_error of Location.lexeme_pos * string

let error_message state =
  try
    let msg = ParserMessages.message state in
    Printf.sprintf "(state %d) %s" state msg
  with Not_found ->
    Printf.sprintf "Unknown syntax error (in state %d)" state

let parse filename scanner lexbuf =
  Lexing.set_filename lexbuf filename;
  try
    Parser.program scanner lexbuf
  with
  | Parser.Error state -> let message = error_message state in
                          let position = Location.to_lexeme_position lexbuf in
                          raise(Syntax_error(position, message))
