{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

	let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, value) -> Hashtbl.add tbl key value) init;
		tbl
	
	let keywords_table =
		create_hashtable 2 [
			("int", INT);
			("return", RETURN)
		]
}

let newline = '\n' | '\r' '\n'
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let identifier = letter (letter | digit | '_')*

(* Scanner specification *)
rule next_token = parse
	  [' ' '\t']+   	{ next_token lexbuf }	(* ignore and skip whitespace *)
	| newline			{ Lexing.new_line lexbuf; next_token lexbuf }
	| digit+ as lit 	{ INTEGER(int_of_string lit) }
	| identifier as word (* identifier or keyword *)
	{					
		match Hashtbl.find_opt keywords_table word with 
		| Some token 	-> token 
		| None 			-> ID(word)
	}
	| '+'            	{ ADD }
	| '-'            	{ SUB }
	| '*'            	{ MULT }
	| '/'            	{ DIV }
	| '='            	{ EQ }
	| '('            	{ LPAREN }
	| ')'            	{ RPAREN }
	| '['            	{ LBRACKET }
	| ']'            	{ RBRACKET }
	| '{'            	{ LBRACE }
	| '}'            	{ RBRACE }
	| ';'            	{ SEMICOL }
	| eof            	{ EOF }
	| _ 				{ raise (Lexing_error((Location.to_lexeme_position lexbuf), "Unexpected character")) }
