open Microc
type action = Parse | Type_check | Dump_llvm_ir | Compile

let[@inline] ( >> ) f g x = g (f x)

(* let action_function outputfile optimize verify_module = function *)
let action_function = function
  | Parse ->
      Parsing.parse Scanner.next_token
      >> Ast.show_program
      >> Printf.printf "%s\n"
  | Type_check ->
      Parsing.parse Scanner.next_token
      >> Semantic_analysis.type_check 
      >> Ast.show_program
      >> Printf.printf "%s\n"
	| _ -> failwith("Not supported yet")
(*   | Dump_llvm_ir ->
      Parsing.parse Scanner.next_token
      >> Semantic_analysis.type_check >> Codegen.to_llvm_module
      >> (fun llmodule ->
           if verify_module then Llvm_analysis.assert_valid_module llmodule;
           llmodule)
      >> (if optimize then Optimizer.optimize_module else Fun.id)
      >> Llvm.dump_module
  | Compile ->
      (Parsing.parse Scanner.next_token
      >> Semantic_analysis.type_check >> Codegen.to_llvm_module
      >> (fun llmodule ->
           if verify_module then Llvm_analysis.assert_valid_module llmodule;
           Llvm_analysis.assert_valid_module llmodule;
           llmodule)
      >> if optimize then Optimizer.optimize_module else Fun.id)
      >> fun llmodule ->
      assert (Llvm_bitwriter.write_bitcode_file llmodule outputfile) *)

let load_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let () =
  try
		let action = ref Type_check in
    (* let action = ref Compile in *)
    let filename = ref "" in
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
          "Compile the source file (default)" );
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
    Arg.parse spec_list (fun file -> filename := file) usage;
    if String.equal !filename "" then Arg.usage spec_list usage
    else
      let source = load_file !filename in
      let lexbuf = Lexing.from_string ~with_positions:true source in
      (* try action_function !outputfile !optimize !verify !action lexbuf with *)
      try action_function !action lexbuf with
			| Microc.Scanner.Lexing_error (pos, msg) | Microc.Parsing.Syntax_error (pos, msg) -> 
				Microc.Errors.report_singleline "Error" source pos msg
			| Microc.Sem_error.Semantic_error (pos, msg) -> 
				Microc.Errors.report_multiline "Syntax error" source pos msg
  with Sys_error msg -> Printf.eprintf "*** Error %s ***\n" msg

(* let () =
	let input_channel =
		if Array.length Sys.argv > 1 then
			open_in Sys.argv.(1)
		else
			stdin
	in
	let lexbuf = Lexing.from_channel ~with_positions:true input_channel in
	try
		let parsingResult = Microc.Parsing.parse Microc.Scanner.next_token lexbuf in
		let semAnalysResult = Microc.Semantic_analysis.type_check parsingResult in
		Printf.fprintf stdout "\n%s\n" (Microc.Ast.show_program semAnalysResult);
		close_in input_channel;
	with
		| Microc.Scanner.Lexing_error (pos, msg) | Microc.Parsing.Syntax_error (pos, msg) -> 
				let error_pos = Microc.Errors.SingleLine(pos) in
				Microc.Errors.report_error "Error" input_channel stdout error_pos msg;
				close_in input_channel;
		| Microc.Sem_error.Semantic_error (pos, msg) -> 
				let error_pos = Microc.Errors.Multiline(pos) in
				Microc.Errors.report_error "Syntax error" input_channel stdout error_pos msg;
				close_in input_channel;
 *)