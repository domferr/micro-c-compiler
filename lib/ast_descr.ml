let descr_typ typ = match typ with
    Ast.TypI -> "integer"
  | Ast.TypB -> "boolean"
  | Ast.TypC -> "char"
  | Ast.TypV -> "void"
  | Ast.TypA _ -> "array"
  | Ast.TypP _ -> "pointer"