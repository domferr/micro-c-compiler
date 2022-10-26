
let print_error_lines error_channel sourcecode_channel from_line to_line =
	try
		for _ = 1 to from_line - 1 do
			ignore (input_line sourcecode_channel)
		done;
		for _ = from_line to to_line do
			Printf.fprintf error_channel "%s\n" (input_line sourcecode_channel)
		done
	with _ -> ()

let max_number_of_lines = 3 (* How many lines to print before the error line *)

let report_error file_channel outchan pos msg =
	(* Line in which the error occurred *)
	let error_line = pos.Location.line in
	let number_of_lines = if error_line - max_number_of_lines > 0 then error_line - max_number_of_lines else 1 in
	let from_line = error_line - number_of_lines in
	(* Start reading the channel again from the beginning *)
	let _ = seek_in file_channel 0 in
	print_error_lines outchan file_channel from_line error_line;
	(* Create a string with number of spaces from the beginning of the line to the error column *)
	let left_spaces = String.make (pos.Location.start_column - 1) ' ' in
	(* Create a string with number of '^' arrows from the error's beginning column to the error's end column *)
	let error_arrows = String.make (pos.Location.end_column - pos.Location.start_column + 1) '^' in
	(* Finally print the error to the output channel *)
	Printf.fprintf outchan "%s\027[1;31m%s\027[0m\n\027[1;31mError:\027[0m line %d, position %d\n       %s\n" 
													left_spaces error_arrows error_line pos.Location.start_column msg