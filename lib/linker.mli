(** Given a list of abstract syntax trees, merge them into a single ast *)
val link: Ast.program list -> Ast.program