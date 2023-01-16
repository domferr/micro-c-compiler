open Location

type error_location = SingleLine of Location.lexeme_pos | Multiline of Location.code_pos

let print_sourcelines outchan sourcefile_chan start_line error_lines add_arrows =
	let pre_line = if add_arrows then "  " else "" in
	let pre_error_line = if add_arrows then "> " else "" in
	try
		for _ = 0 to start_line - 1 do
			ignore (input_line sourcefile_chan)
		done;
		for _ = start_line to error_lines.start_line - 2 do
			Printf.fprintf outchan "%s%s\n" pre_line (input_line sourcefile_chan)
		done;
		for _ = error_lines.start_line to error_lines.end_line do
			Printf.fprintf outchan "\027[1;31m%s\027[0m%s\n" pre_error_line (input_line sourcefile_chan)
		done
	with _ -> ()

let max_number_of_lines = 6 (* How many lines of the sourcecode to print before printing the error *)

let print_arrows outchan start_column end_column = 
	(* Create a string with number of spaces from the beginning of the line to the error column *)
	let space_before_arrows = String.make (start_column - 1) ' ' in
	(* Create a string with number of '^' arrows from the error's beginning column to the error's end column *)
	let arrows_quantity = if start_column > end_column then 1 
												else end_column - start_column + 1 in
	let error_arrows = String.make arrows_quantity '^' in
	Printf.fprintf outchan "%s\027[1;31m%s\027[0m\n" space_before_arrows error_arrows

let report_error header_str file_channel outchan error_loc msg =
	(* Lines of the source code that will be printed before printing the error message *)
	let error_lines = match error_loc with
			SingleLine(lex_pos) -> {
				start_line 		= lex_pos.line;
				start_column 	= lex_pos.start_column;
				end_line 			= lex_pos.line;
				end_column 		= lex_pos.end_column;
			}
		| Multiline(code_pos) -> code_pos
	in
	let is_multline = match error_loc with
			SingleLine(_) -> false
		| Multiline { start_line = startl; end_line = endl; _} -> startl != endl
	in
	(* Start reading the channel again from the beginning *)
	seek_in file_channel 0;
	
	(* Finally print error lines *)
	let start_line = if (error_lines.start_line - max_number_of_lines + 1) < 0 then 0
									 else error_lines.start_line - max_number_of_lines + 1 in
	print_sourcelines outchan file_channel start_line error_lines is_multline;

	(* Print error arrows *)
	if is_multline then Printf.fprintf outchan "\n" else
	print_arrows outchan error_lines.start_column error_lines.end_column;

	(* Print error location info and header *)
	if error_lines.start_line == error_lines.end_line then
		Printf.fprintf outchan "\027[1;31m%s:\027[0m line %d, position %d\n" 
		header_str error_lines.end_line error_lines.start_column
	else
		Printf.fprintf outchan "\027[1;31m%s:\027[0m lines %d:%d, position %d\n" 
		header_str error_lines.start_line error_lines.end_line error_lines.start_column;

	(* And then print the error message *)
	let bottom_header_space = String.make ((String.length header_str) + 2) ' ' in
	Printf.fprintf outchan "%s%s\n" bottom_header_space msg;
	flush outchan
