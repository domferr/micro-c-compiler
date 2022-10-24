open Location
open Parser

exception Syntax_error of Location.lexeme_pos * string

let token_to_string = function
    INT(value)  ->	string_of_int value
  | PLUS        ->  " piu "
	| MINUS       ->  " meno "
	| TIMES       ->  " per "
	| DIV         ->  " diviso "
	| LPAREN      ->  "("
	| RPAREN      ->  ")"
  | EOF         ->  ""

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
  | Scanner.Lexing_error (pos, msg) ->
    Printf.fprintf stderr "ERROR: line %d, position %d\n       %s\n" pos.line pos.start_column msg
  | End_of_file -> ()
