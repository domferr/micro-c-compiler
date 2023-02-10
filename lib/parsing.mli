exception Syntax_error of Location.lexeme_pos * string

(** Parse source code written in microc *)
val parse : string -> (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.program

(** Parse llvm bitcode. The resulting abstract syntax tree only contains global variables
    and functions declaration. Functions body is not parsed because not needed in further
    compilation steps. *)
val parse_llvm_module : string -> Llvm.llmodule -> Ast.program