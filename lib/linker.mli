exception Linker_error of string

(** Given a list of abstract syntax trees, merge them into a single ast *)
val link_asts: Ast.program list -> Ast.program

(** Given a list of Llvm modules, links them into one module. All the
    given modules are destroyed *)
val link_llvm_modules: Llvm.llmodule list -> Llvm.llmodule

(** Link an object file to create an executable file. Link run-time support library too.
    @param obj_file the path to the object file to be linked
    @param exe_file the path to the executable file to be created
    @param rts_file the path to the sourcecode of the run-time support library
    @raise Linker_error if the linking process fails *)
val link : string -> string -> string -> unit