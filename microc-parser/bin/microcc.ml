open Microc.Location

let print_error outchan pos msg = Printf.fprintf outchan "\027[1;31m\nError:\027[0m line %d, position %d\n       %s\n" pos.line pos.start_column msg

let _ =
  let input_channel =
    if Array.length Sys.argv > 1 then
    open_in Sys.argv.(1)
    else
    stdin
  in
  let lexbuf = Lexing.from_channel input_channel in
  try
    let _ = Microc.Parsing.parse Microc.Scanner.next_token lexbuf in
    ()
  with
  | Microc.Scanner.Lexing_error (pos, msg) -> print_error stderr pos msg
  | End_of_file -> ()
