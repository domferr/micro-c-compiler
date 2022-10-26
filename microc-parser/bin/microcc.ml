let () =
	let input_channel =
		if Array.length Sys.argv > 1 then
			open_in Sys.argv.(1)
		else
			stdin
  	in
  	let lexbuf = Lexing.from_channel ~with_positions:true input_channel in
  	try
    	let result = Microc.Parsing.parse Microc.Scanner.next_token lexbuf in
      	Printf.fprintf stdout "\n%s\n" (Microc.Ast.show_program result);
				close_in input_channel;
  	with
    	| Microc.Scanner.Lexing_error (pos, msg) -> 
					Microc.Errors.report_error "Error" input_channel stdout pos msg;
					close_in input_channel;
			| Microc.Parsing.Syntax_error (pos, msg) -> 
					Microc.Errors.report_error "Syntax error" input_channel stdout pos msg;
					close_in input_channel;
	
