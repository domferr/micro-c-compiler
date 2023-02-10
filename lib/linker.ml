exception Linker_error of string

let link_asts programs = Ast.Prog(
  List.concat_map (fun (Ast.Prog(topdecls)) -> topdecls) programs
)

let link_llvm_modules llmodules = 
  match llmodules with
    [] -> raise(Linker_error("Nothing to be linked"))
  | llhd::lltail -> 
    List.fold_left (fun linkedmodule llm -> 
      try Llvm_linker.link_modules' linkedmodule llm; linkedmodule with
      Llvm_linker.Error(msg) -> raise(Parsing.Syntax_error(Location.dummy_lexeme_pos, msg))
    ) llhd lltail

let link obj_file exe_file rts_file =
  match Unix.fork () with
      -1 -> raise (Linker_error "Unable to fork another process to run 'clang'")
    | 0  -> (* Child process, run clang to link and build the executable. Link RTS also *) 
      let clang_args = [| "clang"; "-o"; exe_file; obj_file; rts_file; |] in
      Unix.execvp "clang" clang_args |> ignore;
      exit 1 
      (*
      let ld_args = [| "ld"; "-o"; exe_file; obj_file; "-pie"; "./bin/rt-support.o"; "-lc"; "-dynamic-linker"; "/lib64/ld-linux-x86-64.so.2"; |] in
      Unix.execvp "ld" ld_args |> ignore;*)
    | pid -> (* Compiler process, wait for ld execution *)
      let _, status = Unix.waitpid [] pid in
      if status = Unix.WEXITED 0 then ()
      else raise (Linker_error "Linking failed")