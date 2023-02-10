(** Generate LLVM of the given abstract syntax tree. It adds LLVM content into the
    given module and returns the module itself. *)
val to_llvm_module : Ast.program -> Llvm.llmodule -> Llvm.llmodule

(** Given an LLVM type, returns the equivalent AST type, if any. None otherwise. *)
val get_ast_typ : Llvm.lltype -> Ast.typ option

(** Sets data layout and target triple to the given module. Returns the target machine. *)
val set_target_info: Llvm.llmodule -> Llvm_target.TargetMachine.t