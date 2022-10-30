exception Syntax_error of Location.lexeme_pos * string

let error_message state =
  try
    ParserMessages.message state
  with Not_found ->
    Printf.sprintf "Unknown syntax error (in state %d).\n" state

let parse scanner lexbuf =
  try
    Parser.program scanner lexbuf
  with
  | Parser.Error state -> raise(Syntax_error(Location.to_lexeme_position lexbuf, error_message state))
