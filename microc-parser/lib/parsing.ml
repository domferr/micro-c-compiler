exception Syntax_error of Location.lexeme_pos * string

let parse scanner lexbuf = Microc.Parser.program scanner lexbuf