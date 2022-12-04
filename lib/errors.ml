open Location

type error_location = SingleLine of Location.lexeme_pos | Multiline of Location.code_pos

let print_error_lines error_channel sourcecode_channel from_line to_line =
	try
		for _ = 1 to from_line-1 do
			ignore (input_line sourcecode_channel)
		done;
		for _ = from_line to to_line do
			Printf.fprintf error_channel "%s\n" (input_line sourcecode_channel)
		done
	with _ -> ()

let max_number_of_lines = 6 (* How many lines of the sourcecode to print before printing the error *)

let get_start_line errorline =
	if (errorline - max_number_of_lines + 1) <= 0 then 1
	else errorline - max_number_of_lines + 1

let report_error header_str file_channel outchan error_loc msg =
	(* Lines of the source code that will be printed before printing the error message *)
	let error_lines = match error_loc with
			SingleLine(lex_pos) -> {
				start_line 		= get_start_line lex_pos.line;
				start_column 	= lex_pos.start_column;
				end_line 			= lex_pos.line;
				end_column 		= lex_pos.end_column;
			}
		| Multiline(code_pos) -> {
				start_line 		= get_start_line code_pos.start_line;
				start_column 	= code_pos.start_column;
				end_line 			= code_pos.end_line;
				end_column 		= code_pos.end_column;
			}
	in
	(* Start reading the channel again from the beginning *)
	seek_in file_channel 0;

	(* Finally print error lines *)
	print_error_lines outchan file_channel error_lines.start_line error_lines.end_line;

	(* Print error arrows *)
	(* Create a string with number of spaces from the beginning of the line to the error column *)
	let space_before_arrows = String.make (error_lines.start_column - 1) ' ' in
	(* Create a string with number of '^' arrows from the error's beginning column to the error's end column *)
	let arrows_quantity = if error_lines.start_column > error_lines.end_column then 1 
												else error_lines.end_column - error_lines.start_column + 1 in
	let error_arrows = String.make arrows_quantity '^' in
	Printf.fprintf outchan "%s\027[1;31m%s\027[0m\n" space_before_arrows error_arrows;

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
