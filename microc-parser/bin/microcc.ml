open Microc

let print_error file_channel outchan (pos :Location.lexeme_pos) msg =
	let _ = seek_in file_channel 0 in
	let rec skip_lines channel lines =
		if lines <= 1 
		then () 
		else let _ = input_line channel in skip_lines channel (lines-1)
	in
	let rec read_lines channel lines acc = 
		if lines <= 0
		then acc
		else let last = input_line channel 
			in let new_acc = Printf.sprintf "%s%s\n" acc last 
				in read_lines channel (lines-1) new_acc
	in
	let error_line = pos.Location.line in
	let max_number_of_lines = 3 in
	let number_of_lines = if error_line - max_number_of_lines > 0 then error_line - max_number_of_lines else 1 in
	let first_line = error_line - number_of_lines in
	let _ = skip_lines file_channel first_line in
	let file_sourcecode_line = read_lines file_channel number_of_lines "" in
	let left_spaces = String.make (pos.Location.start_column - 1) ' ' in 
	let error_arrows = String.make (pos.Location.end_column - pos.Location.start_column + 1) '^' in
	Printf.fprintf outchan "%s%s%s\n\027[1;31mError:\027[0m line %d, position %d\n       %s\n" 
	file_sourcecode_line left_spaces error_arrows error_line pos.Location.start_column msg

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
      	Printf.fprintf stdout "\n%s\n" (Microc.Ast.show_program result)
  	with
    	| Microc.Scanner.Lexing_error (pos, msg) -> print_error input_channel stdout pos msg
	
