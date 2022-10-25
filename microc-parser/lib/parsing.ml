open Location
open Parser

exception Syntax_error of Location.lexeme_pos * string

let token_to_string = function
    INT(value)  ->	string_of_int value
  | ADD        ->  " piu "
	| SUB       ->  " meno "
	| MULT       ->  " per "
	| DIV         ->  " diviso "
	| LPAREN      ->  "("
	| RPAREN      ->  ")"
  | EOF         ->  ""

let print_error outchan pos msg = Printf.fprintf outchan "\027[31m\nerror:\027[0m line %d, position %d\n       %s\n" pos.line pos.start_column msg

let rec parse scanner lexbuf =
  let token = scanner lexbuf in
    let token_str = token_to_string token in
      Printf.printf "%s" token_str;
  parse scanner lexbuf

let () =
  let input_channel =
    if Array.length Sys.argv > 1 then
    open_in Sys.argv.(1)
    else
    stdin
  in
  let lexbuf = Lexing.from_channel input_channel in
  try
    parse Scanner.next_token lexbuf
  with
  | Scanner.Lexing_error (pos, msg) -> print_error stderr pos msg
  | End_of_file -> ()
