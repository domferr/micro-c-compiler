{
	open Parser

	(* Auxiliary definitions *)
	exception Lexing_error of Location.lexeme_pos * string

	let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, value) -> Hashtbl.add tbl key value) init;
		tbl
	
	let keywords_table =
		create_hashtable 9 [
			("int", INT);
			("char", CHAR);
			("bool", BOOL);
			("void", VOID);
			("if", IF);
			("else", ELSE);
			("for", FOR);
			("while", WHILE);
			("return", RETURN)
		]
}

let newline = '\n' | '\r' '\n'
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let identifier = (letter | '_') (letter | digit | '_')*

(* Scanner specification *)
rule next_token = parse
	  [' ' '\t']+   	{ next_token lexbuf }	(* ignore and skip whitespace *)
	| newline			{ Lexing.new_line lexbuf; next_token lexbuf }
	| digit+ as lit 	{ INTEGER(int_of_string lit) }
	| identifier as word (* identifier or keyword *)
	{					
		match Hashtbl.find_opt keywords_table word with 
		| Some token 	-> token 
		| None 				-> ID(word)
	}
	| "true"	{ BOOLEAN(true) }
	| "false"	{ BOOLEAN(false) } (* todo maybe true and alse are keywords *)
	| '+'       { ADD }
	| '-'       { SUB }
	| '*'       { MULT }
	| '/'       { DIV }
	| '%'       { MOD }
	| '='       { ASSIGN }
	| '>'       { GT }
	| '<'       { LT }
	| "=="      { EQ }
	| ">="      { GEQ }
	| "<="      { LEQ }
	| "!="      { NEQ }
	| "&&"      { AND }
	| "||"      { OR }
	| "!"      	{ NOT }
	| '('       { LPAREN }
	| ')'       { RPAREN }
	| '['       { LBRACKET }
	| ']'       { RBRACKET }
	| '{'       { LBRACE }
	| '}'       { RBRACE }
	| ';'       { SEMICOL }
	| ','       { COMMA }
	| eof		{ EOF }
	| _ 		{ raise (Lexing_error((Location.to_lexeme_position lexbuf), "Unexpected character")) }
