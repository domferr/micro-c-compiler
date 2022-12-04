let () =
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
		| Microc.Scanner.Lexing_error (pos, msg) -> 
				let error_pos = Microc.Errors.SingleLine(pos) in
				Microc.Errors.report_error "Error" input_channel stdout error_pos msg;
				close_in input_channel;
		| Microc.Parsing.Syntax_error (pos, msg) -> 
				let error_pos = Microc.Errors.SingleLine(pos) in
				Microc.Errors.report_error "Syntax error" input_channel stdout error_pos msg;
				close_in input_channel;
		| Microc.Semantic_analysis.Semantic_error (pos, msg) -> 
				let error_pos = Microc.Errors.Multiline(pos) in
				Microc.Errors.report_error "Syntax error" input_channel stdout error_pos msg;
				close_in input_channel;
