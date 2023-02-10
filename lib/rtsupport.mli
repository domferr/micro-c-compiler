(** The path of the run-time support's source code *)
val sourcepath : string

(** Iterate through each topdeclaration of the run-time support library *)
val iter : (Ast.topdecl_node -> unit) -> unit