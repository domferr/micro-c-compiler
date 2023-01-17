open Ast

exception Semantic_error of Location.code_pos * string

let raise_variable_main node = raise(Semantic_error(node.loc,
  "Cannot declare 'main' variable: this name is reserved for the 'main' function"
))

let raise_invalid_def_main node = raise(Semantic_error(node.loc, 
  "Invalid definition of the 'main' function. The signature must be 'int main()' or 'void main()'"
))

let raise_duplicate_declaration loc entry = raise(Semantic_error(loc,
  Printf.sprintf "Duplicate declaration of '%s'" entry
))

let raise_variable_not_declared node ide = raise(Semantic_error(node.loc, 
  Printf.sprintf "Variable '%s' not declared" ide
))

let raise_missing_main_definition () = raise(Semantic_error(Location.dummy_code_pos, 
  "Missing definition of the 'main' function")
)

let raise_invalid_pointer_deref node = raise(Semantic_error(node.loc,
  "Invalid pointer dereferencing"
))


let raise_invalid_array_index_type node = raise(Semantic_error(node.loc, 
  "Array index must be and integer"
))

let raise_invalid_array_index node = raise(Semantic_error(node.loc, 
  "Invalid array indexing"
))

let raise_invalid_assignment node = raise(Semantic_error(node.loc, 
  "Invalid assignment"
))

let descr_typ typ = match typ with
    Ast.TypI -> "integer"
  | Ast.TypB -> "boolean"
  | Ast.TypC -> "char"
  | Ast.TypV -> "void"
  | Ast.TypA _ -> "array"
  | Ast.TypP _ -> "pointer"

let raise_invalid_assignment_type node left_typ right_typ = raise(Semantic_error(
  node.loc, 
  Printf.sprintf "Invalid assignment of %s to %s" (descr_typ right_typ) (descr_typ left_typ)
))

let raise_invalid_pointer_type node = raise(Semantic_error(node.loc, 
  "Invalid pointer type"
))

let raise_invalid_unary_op node = raise(Semantic_error(node.loc, 
  "Invalid unary operation"
))

let raise_invalid_binary_op node = raise(Semantic_error(node.loc, 
  "Invalid binary operation"
))

let raise_invalid_function_arg_type node req_typ given_typ = raise(Semantic_error(node.loc, 
  Printf.sprintf "Invalid argument, expected %s but found %s" (descr_typ req_typ) (descr_typ given_typ))
)

let raise_invalid_argument node = raise(Semantic_error(node.loc, 
  "Invalid argument"
))

let raise_invalid_arguments_number node funname req_len given_len = raise(Semantic_error(node.loc, 
  Printf.sprintf "Function '%s' expects %d arguments but here %d arguments are passed to it" 
    funname req_len given_len
))

let raise_missing_fun_declaration node funname = raise(Semantic_error(node.loc, 
  Printf.sprintf "Missing declaration of function '%s'" funname
))

let raise_missing_guard node = raise(Semantic_error(node.loc, 
  "Missing guard"
))

let raise_invalid_guard_type node = raise(Semantic_error(node.loc, 
  "Guard's type is not boolean"
))

let raise_missing_return_value node = raise(Semantic_error(node.loc, 
  "Missing return value"
))

let raise_invalid_return_type node = raise(Semantic_error(node.loc, 
  "Invalid return type"
))

let raise_missing_return node = raise(Semantic_error(node.loc, 
  "Missing return statement"
))

let raise_missing_array_size node = raise(Semantic_error(node.loc, 
  "Cannot declare array without a size"
))