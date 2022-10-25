{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

	(* let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
		tbl
	
	let keywords_table =
		create_hashtable 1 [
			("if", IF)
		] *)
}

let newline = '\n' | '\r' '\n'
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let identifier = letter (letter | digit | '_')*

(* Scanner specification *)
rule next_token = parse
	  [' ' '\t']+   	{ next_token lexbuf }	(* ignore and skip whitespace *)
	| newline			{ Lexing.new_line lexbuf; next_token lexbuf }
	| digit+ as lit 	{ INT(int_of_string lit) }
	(* | identifier as word{ try					(* identifier or keyword *)
						  let token = Hashtbl.find keywords_table word in
							token
						  with Not_found ->
							ID word
						} *)
	| '+'            	{ ADD }
	| '-'            	{ SUB }
	| '*'            	{ MULT }
	| '/'            	{ DIV }
	| '('            	{ LPAREN }
	| ')'            	{ RPAREN }
	| eof            	{ EOF }
	| _ 				{ raise (Lexing_error((Location.to_lexeme_position lexbuf), "Unexpected character")) }
