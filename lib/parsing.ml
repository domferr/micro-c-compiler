exception Syntax_error of Location.lexeme_pos * string

let error_message state =
  try
    let msg = ParserMessages.message state in
    Printf.sprintf "%s (in state %d).\n" msg state
  with Not_found ->
    Printf.sprintf "Unknown syntax error (in state %d).\n" state

let parse scanner lexbuf =
  try
    Parser.program scanner lexbuf
  with
  | Parser.Error state -> let message = error_message state in
                          let position = Location.to_lexeme_position lexbuf in
                          raise(Syntax_error(position, message))
