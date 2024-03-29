exception Semantic_error of Location.code_pos * string

val raise_variable_main: Location.code_pos -> 'b
val raise_invalid_def_main: Location.code_pos -> 'b
val raise_duplicate_declaration: Location.code_pos -> Ast.identifier -> 'a
val raise_variable_not_declared: 'a Ast.annotated_node -> Ast.identifier -> 'b
val raise_missing_main_definition: unit -> 'a
val raise_invalid_pointer_deref: 'a Ast.annotated_node -> 'b
val raise_invalid_array_index_type: 'a Ast.annotated_node -> 'b
val raise_invalid_array_index: 'a Ast.annotated_node -> 'b
val raise_invalid_assignment_type: 'a Ast.annotated_node -> Ast.typ -> Ast.typ -> 'b
val raise_not_compiletime_constant: 'a Ast.annotated_node -> 'b
val raise_invalid_unary_op: 'a Ast.annotated_node -> 'b
val raise_invalid_unary_op_access: 'a Ast.annotated_node -> 'b
val raise_invalid_binary_op: 'a Ast.annotated_node -> 'b
val raise_invalid_binary_comparison: 'a Ast.annotated_node -> Ast.typ -> Ast.typ -> 'b
val raise_invalid_function_body: 'a Ast.annotated_node -> 'b
val raise_invalid_function_arg_type: 'a Ast.annotated_node -> Ast.typ -> Ast.typ -> 'b
val raise_invalid_arguments_number: 'a Ast.annotated_node -> Ast.identifier -> int -> int -> 'b
val raise_missing_fun_declaration: 'a Ast.annotated_node -> Ast.identifier -> 'b
val raise_invalid_guard_type: 'a Ast.annotated_node -> 'b
val raise_missing_return_value: 'a Ast.annotated_node -> 'b
val raise_invalid_return_type: 'a Ast.annotated_node -> 'b
val raise_missing_return: 'a Ast.annotated_node -> 'b
val raise_missing_array_size: 'a Ast.annotated_node -> 'b
val raise_missing_multidimensional_array_size: 'a Ast.annotated_node -> 'b
val raise_reserved_for_rts : string -> Location.code_pos -> 'b