exception Syntax_error of Location.lexeme_pos * string

let parse scanner lexbuf =
  try
    Parser.program scanner lexbuf
  with
    | Parser.Error -> raise(Syntax_error(Location.to_lexeme_position lexbuf, "Right now I don't understand how to give an error message and which one eheheh"))