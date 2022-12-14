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
type error_location = SingleLine of Location.lexeme_pos | Multiline of Location.code_pos

val report_error: string -> in_channel -> out_channel -> error_location -> string -> unit