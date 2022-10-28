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
	
	(*let getchar = match c with
		| None		-> None
		| Some val	-> *)
}

let newline = '\n' | '\r' '\n'
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let identifier = (letter | '_') (letter | digit | '_')*

(* Scanner specification *)
rule next_token = parse
	  [' ' '\t']+		{ next_token lexbuf }	(* ignore and skip whitespace *)
	| newline			{ Lexing.new_line lexbuf; next_token lexbuf }
	| digit+ as lit		{ INTEGER(int_of_string lit) }
	| identifier as word 
	{	(* identifier or keyword *)
		match Hashtbl.find_opt keywords_table word with 
		| Some token	-> token 
		| None			-> ID(word)
	}
	| "/*"		{ multilinecomment lexbuf }
	| "//"		{ singlelinecomment lexbuf }
	| '\'' [^ '\''] '\''		{ CHARACTER('t') }
	| "true"	{ BOOLEAN(true) }
	| "false"	{ BOOLEAN(false) } (* todo maybe true and alse are keywords *)
	| '+'		{ ADD }
	| '-'		{ SUB }
	| '*'		{ MULT }
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
	| _			{ raise (Lexing_error((Location.to_lexeme_position lexbuf), "Unexpected character")) }

and multilinecomment = parse
	| "*/"			{ next_token lexbuf }
	| newline		{ Lexing.new_line lexbuf; multilinecomment lexbuf }
	| _				{ multilinecomment lexbuf }
	| eof			{ raise (Lexing_error((Location.to_lexeme_position lexbuf), "Multiline comment not closed")) }

and singlelinecomment = parse
	| newline		{ Lexing.new_line lexbuf; next_token lexbuf }
	| _				{ singlelinecomment lexbuf }
	| eof			{ EOF }

(* special characters are \', \b, \f, \t, \\, \r, and \n *)
(*and readchar = parse
	  "'"			{ raise (Lexing_error((Location.to_lexeme_position lexbuf), "Missing character between single quotes")) }
	| [^ '\''] as c	{ CHARACTER(c) }
	| eof			{ raise (Lexing_error((Location.to_lexeme_position lexbuf), "Not finished char")) }
*)