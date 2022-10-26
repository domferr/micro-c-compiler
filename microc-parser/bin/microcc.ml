open Microc

(*
	Print the error to outchan. The error follows this template:
	<number_of_lines> lines of the source code
	some spaces and then arrows pointing to the error
	Then Error: line <error_line>, position <pos.Location.start_column>
	Some spaces to align with "Error:" and then error message <msg>
	'Error:' and the arrows '^' are written in bold red.

	Error example:
	int main() {
		int test
				^^^^       
	Error: line 2, position 7
				Unexpected character
*)
let print_error file_channel outchan (pos :Location.lexeme_pos) msg =
	(* Start reading the channel again from the beginning *)
	let _ = seek_in file_channel 0 in
	(* Function to skip <lines> lines from channel *)
	let rec skip_lines channel lines = 
		if lines <= 1 then () 
		else let _ = input_line channel in skip_lines channel (lines-1)
	in
	(* Function to read <lines> lines from channel *)
	let rec read_lines channel lines acc =  
		if lines <= 0 then acc
		else let last = input_line channel 
			in let new_acc = Printf.sprintf "%s%s\n" acc last 
				in read_lines channel (lines-1) new_acc
	in
	(* Line in which the error occurred *)
	let error_line = pos.Location.line in
	(* How many lines to read before the error line *)
	let max_number_of_lines = 3 in
	let number_of_lines = if error_line - max_number_of_lines > 0 then error_line - max_number_of_lines else 1 in
	let first_line = error_line - number_of_lines in
	(* Skip the lines not needed *)
	let _ = skip_lines file_channel first_line in
	(* Finally read <number_of_lines> lines *)
	let file_sourcecode_lines = read_lines file_channel number_of_lines "" in
	(* Create a string with number of spaces from the beginning of the line to the error column *)
	let left_spaces = String.make (pos.Location.start_column - 1) ' ' in
	(* Create a string with number of '^' arrows from the error's beginning column to the error's end column *)
	let error_arrows = String.make (pos.Location.end_column - pos.Location.start_column + 1) '^' in
	(* Finally print the error to the output channel *)
	Printf.fprintf outchan "%s%s\027[1;31m%s\027[0m\n\027[1;31mError:\027[0m line %d, position %d\n       %s\n" 
	file_sourcecode_lines left_spaces error_arrows error_line pos.Location.start_column msg

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
	
