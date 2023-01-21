type test = {
  func: unit -> bool;
  name: string;
  reqResult: bool;
}

type test_result = {
  name: string;
  pass: bool;
}

let load_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let print_test_failed testname =
  let fail_color="\027[1;91m" in
  let reset_color="\027[0m" in 
  Printf.printf "%s[ FAIL ]%s %s\n" fail_color reset_color testname

let print_test_passed testname =
  let pass_color="\027[1;92m" in
  let reset_color="\027[0m" in 
  Printf.printf "%s[ PASS ]%s %s\n" pass_color reset_color testname

let print_test_result test_res = 
  if test_res.pass then print_test_passed test_res.name
  else print_test_failed test_res.name

let testerFun test =
  try
  let res = test.func() in
  let pass = res = test.reqResult in
    { name = test.name; pass }
  with e -> 
    print_test_failed test.name;
    let msg = Printexc.to_string e in
    let stack = Printexc.get_backtrace () in
      Printf.printf "There was an unexpected error: %s%s\n\n" msg stack;
      { name = test.name; pass = false }
