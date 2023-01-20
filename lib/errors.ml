let report_line header line (lexeme_pos: Location.lexeme_pos) msg =
	let prefix = String.make (lexeme_pos.Location.start_column - 1) ' ' in
  let arrows = String.make
		(lexeme_pos.Location.end_column - lexeme_pos.Location.start_column + 1)
		'^'
	in
	Printf.eprintf "\027[1;31m%s\027[0m, line %d: \027[1m%s\027[0m\n%s\n%s\027[1;31m%s\027[0m\n"
		header lexeme_pos.Location.line msg line prefix arrows

let report_singleline header source lexeme_pos msg =
  let lines = String.split_on_char '\n' source in
  let line = List.nth lines (lexeme_pos.Location.line - 1) in
  report_line header line lexeme_pos msg

let report_multiline header source code_pos msg =
  let lines =
    String.split_on_char '\n' source
    |> List.filteri (fun line _ ->
           code_pos.Location.start_line - 1 <= line
           && line <= code_pos.Location.end_line - 1)
  in
  let length = List.length lines in
  if length = 1 then
    let line = List.hd lines in
		let lexeme_pos = Location.{ 
			line = code_pos.Location.start_line; 
			start_column = code_pos.Location.start_column; 
			end_column = code_pos.Location.end_column 
		} in
		report_line header line lexeme_pos msg
  else
    let text = lines |> List.filteri (fun i _ -> i < 5) |> String.concat "\n" in
    Printf.eprintf "\027[1;31m%s\027[0m, lines %d-%d: \027[1m%s\027[0m\n%s\n"
      header
			code_pos.Location.start_line
      (code_pos.Location.start_line + 5)
			msg
      text