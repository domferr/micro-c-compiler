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

let report_error header_str file_channel outchan pos msg =
	let _ = Location.show_lexeme_pos pos in
	(* Line in which the error occurred *)
	let error_line = pos.Location.line in
	let from_line = if (error_line - max_number_of_lines + 1) <= 0 then 1 else error_line - max_number_of_lines + 1 in
	(* Start reading the channel again from the beginning *)
	seek_in file_channel 0;
	(* Finally print error lines *)
	print_error_lines outchan file_channel from_line error_line;
	(* Create a string with number of spaces from the beginning of the line to the error column *)
	let space_before_arrows = String.make (pos.Location.start_column - 1) ' ' in
	(* Create a string with number of '^' arrows from the error's beginning column to the error's end column *)
	let arrows_quantity = if pos.Location.start_column > pos.Location.end_column then 1 
												else pos.Location.end_column - pos.Location.start_column + 1 in
	let error_arrows = String.make arrows_quantity '^' in
	let bottom_header_space = String.make ((String.length header_str) + 2) ' ' in
	(* Finally print the error to the output channel *)
	Printf.fprintf outchan "%s\027[1;31m%s\027[0m\n" space_before_arrows error_arrows;
	Printf.fprintf outchan "\027[1;31m%s:\027[0m line %d, position %d\n" header_str error_line pos.Location.start_column;
	Printf.fprintf outchan "%s%s\n" bottom_header_space msg;
	flush outchan
