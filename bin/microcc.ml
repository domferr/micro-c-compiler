open Microc
type action = Parse | Type_check | Dump_llvm_ir | Compile | Executable

exception Fatal_error of string

let[@inline] ( >> ) f g x = g (f x)

(** [load_file filename] Read the file. If it is a bitcode file, raises Invalid_argument exception.
    Returns the file content as a string. *)
let load_file filename =
  if String.ends_with ~suffix:".bc" filename then
    raise(Invalid_argument("Cannot read bitcode file"))
  else
    try 
      let ic = open_in filename in
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      close_in ic;
      Bytes.to_string s
    with Sys_error msg -> raise(Fatal_error(msg))

(** [parse filename] Parse micro c source code or micro c bitcode. Returns the abstract 
    syntax tree and the llvm's module if the bitcode was parsed. *)
let parse_file filename =
  if String.ends_with ~suffix:".bc" filename then 
    try 
      let llmem = Llvm.MemoryBuffer.of_file filename in
      let llctx = Llvm.global_context () in
      let llmodule = Llvm_bitreader.parse_bitcode llctx llmem in
      Parsing.parse_llvm_module filename llmodule, Some llmodule
    with Llvm.IoError msg -> raise(Fatal_error(Printf.sprintf "%s. %s" filename msg))
  else
    let source = load_file filename in
    let lexbuf = Lexing.from_string ~with_positions:true source in
    Parsing.parse filename Scanner.next_token lexbuf, None

(** [print_warnings warnings_list] Print the given warning list to stderr *)
let print_warnings =
  (* cache source code already loaded instead of 
     loading the same thing multiple times*)
  let loadedsource = Hashtbl.create 25 in
  List.iter (fun wrng -> 
    let filename = wrng.Warning.loc.Location.filename in
    let source = match Hashtbl.find_opt loadedsource filename with
        Some content -> content
      | None -> try load_file wrng.Warning.loc.Location.filename with _ -> ""
    in
      Warning.fprintf stderr wrng source
  )

(** [parse_all list] Parse the list [list] of all the source code and bit code files *)
let parse_all =
  List.fold_left (fun (astlist, extastlist, llmlist) sf -> 
    match parse_file sf with
        ast, Some llm -> (astlist, ast::extastlist, llm::llmlist)
      | ast, None -> (ast::astlist, extastlist, llmlist)
  ) ([], [], [])

(** [compile verify_module requires_main link_rts optimize sources] *)
let compile ~requires_main ~detect_deadcode =
  parse_all
  >> (function (program, externals, llmodules) -> 
      let mergedprog = Linker.link_asts program in 
      let mergedext = Linker.link_asts externals in
      let astchecked = Semantic_analysis.type_check mergedprog mergedext requires_main in
      if detect_deadcode then (
        let warnings = Deadcode.detect astchecked mergedext in
        print_warnings warnings);
      (astchecked, llmodules))
  >> (function (program, llmodules) ->
      let microcmodule = Llvm.create_module (Llvm.global_context()) "microcc" in
      let llmodule = Linker.link_llvm_modules (microcmodule::llmodules) in
      Codegen.to_llvm_module program llmodule)

(** Just a simple handler that raises Fatal_error exception *)
let llvm_fatal_error_handler msg = raise(Fatal_error(msg))

(** Run module verification. If there is something wrong, raises Fatal_error 
    exception with the reason. Returns the given LLVM module otherwise *)
let llvm_verify_module llmodule = 
  match Llvm_analysis.verify_module llmodule with
    Some error -> raise(Fatal_error("Module validation failed. " ^ error))
  | None -> ();
  llmodule

let action_function outputfile optimize verify_module = function
  | Parse -> 
      (* Parse each given source code or bitcode and print the abstract syntax tree *)
      List.iter (fun sf ->
        let (ast, _) = parse_file sf in
        ast
        |> Ast.show_program
        |> Printf.printf "File: %s\n%s\n\n" sf
      )
  | Type_check -> (* parsing phase, then merge all the ASTs and run typechecker *)
      parse_all
      >> (function (program, externals, _) -> 
          let mergedprog = Linker.link_asts program in 
          let mergedext = Linker.link_asts externals in
          Semantic_analysis.type_check mergedprog mergedext false)
      >> Ast.show_program
      >> Printf.printf "%s"
  | Dump_llvm_ir ->
      compile ~requires_main:false ~detect_deadcode:false
      >> (if verify_module then llvm_verify_module 
          else Fun.id)
      >> (if optimize then Optimizer.optimize_module else Fun.id)
      >> Llvm.dump_module
  | Compile -> (* Build object code if the output file ends with .o, bitcode otherwise *)
      compile ~requires_main:false ~detect_deadcode:false
      >> llvm_verify_module
      >> (if optimize then Optimizer.optimize_module else Fun.id)
      >> (fun llmodule ->
          if String.ends_with ~suffix:".o" outputfile then
            let machine = Codegen.set_target_info llmodule in
            let filetype = Llvm_target.CodeGenFileType.ObjectFile in
            Llvm_target.TargetMachine.emit_to_file llmodule filetype outputfile machine
          else 
            let _ = Codegen.set_target_info llmodule in
            assert (Llvm_bitwriter.write_bitcode_file llmodule outputfile))
  | Executable -> (* Build object file and then call clang to link everything (and the rts) to create the executable *)
      compile ~requires_main:true ~detect_deadcode:true
      >> llvm_verify_module
      >> (if optimize then Optimizer.optimize_module else Fun.id)
      >> (fun llmodule ->
          let executable = Filename.remove_extension outputfile in
          let temp_obj_file = Filename.temp_file executable ".o" in
          try
            let machine = Codegen.set_target_info llmodule in
            let filetype = Llvm_target.CodeGenFileType.ObjectFile in
            (* build temporary object file *)
            Llvm_target.TargetMachine.emit_to_file llmodule filetype temp_obj_file machine;
            Linker.link temp_obj_file executable Rtsupport.sourcepath;
            Sys.remove temp_obj_file;
          with exn -> 
            Sys.remove temp_obj_file; 
            raise exn)

let () =
  try
    let action = ref Executable in
    let sourcefiles = ref [] in
    let outputfile = ref "a.bc" in
    let optimize = ref false in
    let verify = ref false in
    let spec_list =
      [
        ("-p", 
          Arg.Unit (fun () -> action := Parse), "Parse and print AST");
        ( "-t",
          Arg.Unit (fun () -> action := Type_check),
          "Type checks and print the result" );
        ( "-d",
          Arg.Unit (fun () -> action := Dump_llvm_ir),
          "Compile and print the generated LLVM IR" );
        ( "-c",
          Arg.Unit (fun () -> action := Compile),
          "Compile the source file" );
        ( "-o",
          Arg.Set_string outputfile,
          "Place the output into file (default: a.bc)" );
        ( "-O",
          Arg.Set optimize,
          "Optimize the generated LLVM IR (default: false)" );
        ( "-verify",
          Arg.Set verify,
          "Verify the generated LLVM module (default: false)" );
      ]
    in
    let usage =
      Printf.sprintf "Usage:\t%s [options] <source_file>\n" Sys.argv.(0)
    in
    Arg.parse spec_list (fun file -> sourcefiles := file::!sourcefiles) usage;
    if !sourcefiles = [] then (Arg.usage spec_list usage; exit 0 |> ignore)
    else
      Llvm.install_fatal_error_handler llvm_fatal_error_handler;
      try action_function !outputfile !optimize !verify !action !sourcefiles with
			| Microc.Scanner.Lexing_error (pos, msg) | Microc.Parsing.Syntax_error (pos, msg) ->
        let source = try load_file pos.Location.filename with _ -> "" in
        Microc.Errors.report_singleline "Error" source pos msg
			| Microc.Sem_error.Semantic_error (pos, msg) ->
        let source = try load_file pos.Location.filename with _ -> "" in
        Microc.Errors.report_multiline "Syntax error" source pos msg
  with 
    Fatal_error msg
  | Failure msg -> Printf.eprintf "\027[1;31mFatal error:\027[0m %s\n" msg
  | _ -> Printf.eprintf "\027[1;31mUnexpected error\027[0m\n"