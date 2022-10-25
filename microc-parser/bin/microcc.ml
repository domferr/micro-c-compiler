open Microc

let print_error outchan (pos :Location.lexeme_pos) msg = 
	let left_spaces = String.make (pos.Location.start_column - 1) ' ' in 
	let error_indication = String.make (pos.Location.end_column - pos.Location.start_column + 1) '^' in 
	Printf.fprintf outchan "\n%s%s\n\027[1;31mError:\027[0m line %d, position %d\n       %s\n" left_spaces error_indication pos.Location.line pos.Location.start_column msg

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
      	Printf.fprintf stdout "Show: %s\n" (Microc.Ast.show_program result)
  	with
    	| Microc.Scanner.Lexing_error (pos, msg) -> print_error stderr pos msg
