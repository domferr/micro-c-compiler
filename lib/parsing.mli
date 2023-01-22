exception Syntax_error of Location.lexeme_pos * string

val parse : string -> (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.program