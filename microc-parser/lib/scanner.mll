{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

	(*open Printf
	let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
		tbl
	
	let keywords_table =
		create_hashtable 1 [
			("if", IF)
		]

	let string_of_token = function
		| PLUS   -> "PLUS"
		| IF     -> "IF"
		| ID(s)  -> Printf.sprintf "ID(%s)" s
		| NUM(i) -> Printf.sprintf "INT_NUM(%d)" i
		| EOF    -> "eof"
	
	let rec iterate scanner =
		match scanner () with
		| EOF -> ()
		| tok -> Printf.printf "%s\n" (string_of_token tok); iterate scanner
	*)
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let identifier = letter (letter | digit | '_')*

(* Scanner specification *)
rule next_token = parse
	  [' ' '\t' '\n']   { next_token lexbuf }	(* ignore and skip whitespace *)
	| digit+ as lit 	{ INT(int_of_string lit) }
	(*| identifier as word{ try					(* identifier or keyword *)
						  let token = Hashtbl.find keywords_table word in
							printf "keyword: %s\n" word;
							token
						  with Not_found ->
							printf "identifier: %s\n" word;
							ID word
						}*)
	| '+'            	{ ADD }
	| '-'            	{ SUB }
	| '*'            	{ MULT }
	| '/'            	{ DIV }
	| '('            	{ LPAREN }
	| ')'            	{ RPAREN }
	| eof            	{ raise End_of_file }
	| _ 				{ raise (Lexing_error((Location.to_lexeme_position lexbuf), "Unexpected character")) }
