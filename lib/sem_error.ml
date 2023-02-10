open Ast

exception Semantic_error of Location.code_pos * string

let raise_variable_main loc = raise(Semantic_error(loc,
  "Cannot declare 'main' variable: this name is reserved for the 'main' function"
))

let raise_invalid_def_main loc = raise(Semantic_error(loc, 
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

let rec descr_typ = function
    Ast.TypI    -> "int"
  | Ast.TypB    -> "bool"
  | Ast.TypC    -> "char"
  | Ast.TypV    -> "void"
  | Ast.TypA(t, size) ->
    let sizetostring = function
        Some s -> Printf.sprintf "[%d]" s
      | None -> "[]"
    in
    let rec looper sizes = function
        Ast.TypA(t2, s1) -> looper (Printf.sprintf "%s%s" sizes (sizetostring s1)) t2
      | anytyp -> (descr_typ anytyp, sizes)
    in
    let arrtyp, sizes = looper (sizetostring size) t in
    Printf.sprintf "%s%s" arrtyp sizes
  | Ast.TypP t  -> (descr_typ t)^"*"
  | Ast.TypNull -> "null"

let raise_invalid_assignment_type node left_typ right_typ = raise(Semantic_error(
  node.loc, 
  Printf.sprintf "Invalid assignment of %s to %s" (descr_typ right_typ) (descr_typ left_typ)
))

let raise_not_compiletime_constant node = raise(Semantic_error(node.loc, 
  "Initializer element is not a compile-time constant"
))

let raise_invalid_unary_op node = raise(Semantic_error(node.loc, 
  "Invalid unary operation"
))

let raise_invalid_unary_op_access node = raise(Semantic_error(node.loc, 
  "Cannot apply unary operation to this element"
))

let raise_invalid_binary_op node = raise(Semantic_error(node.loc, 
  "Invalid binary operation"
))

let raise_invalid_binary_comparison node left_typ right_typ = raise(Semantic_error(node.loc, 
  Printf.sprintf "Invalid binary operation. Cannot compare %s with %s" (descr_typ left_typ) (descr_typ right_typ)
))

let raise_invalid_function_body node = raise(Semantic_error(node.loc, 
  "Function body is not a block"
))

let raise_invalid_function_arg_type node req_typ given_typ = raise(Semantic_error(node.loc, 
  Printf.sprintf "Invalid argument, expected %s but found %s" (descr_typ req_typ) (descr_typ given_typ))
)

let raise_invalid_arguments_number node funname req_len given_len = raise(Semantic_error(node.loc, 
  Printf.sprintf "Function '%s' expects %d arguments but here %d arguments are passed to it" 
    funname req_len given_len
))

let raise_missing_fun_declaration node funname = raise(Semantic_error(node.loc, 
  Printf.sprintf "Missing declaration of function '%s'" funname
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

let raise_missing_multidimensional_array_size node = raise(Semantic_error(node.loc, 
  "Multidimensional array has incomplete element type"
))

let raise_reserved_for_rts ide loc = raise(Semantic_error(loc, 
  Printf.sprintf "Redeclaration of '%s' as a different symbol. It is reserved for run-time support library" ide
))