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

let rec parse scanner lexbuf =
  let token = scanner lexbuf in
    let token_str = token_to_string token in
      Printf.printf "%s" token_str;
  parse scanner lexbuf
