exception Syntax_error of Location.lexeme_pos * string

let parse scanner lexbuf = Parser.program scanner lexbuf