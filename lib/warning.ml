open Location

type warning = {
    msg: string;
    loc: Location.code_pos;
}

let create msg code_pos = 
  { msg; loc = code_pos }

let printf warn source = 
  let lines = String.split_on_char '\n' source in
  let line = List.nth lines (warn.loc.start_line - 1) in
  let prefix = String.make (warn.loc.start_column - 1) ' ' in
  let arrows = String.make (warn.loc.end_column - warn.loc.start_column + 1)
		'^'
	in
  Printf.printf "\027[1;33mWarning\027[0m at file %s, line %d: \027[1;33m%s\027[0m\n%s\n%s\027[1;33m%s\027[0m\n" 
  warn.loc.filename warn.loc.start_line warn.msg 
  line
  prefix arrows