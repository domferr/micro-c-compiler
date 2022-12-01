open Microc

(* ---------- UTILITY FUNCTIONS ---------- *)
let print_test_failed testname =
  let fail_color="\027[1;91m" in
  let reset_color="\027[0m" in 
  Printf.fprintf stdout "%s[ FAIL ]%s %s\n" fail_color reset_color testname

let print_test_passed testname =
  let pass_color="\027[1;92m" in
  let reset_color="\027[0m" in 
  Printf.fprintf stdout "%s[ PASS ]%s %s\n" pass_color reset_color testname

(* Calls the given function and returns true if the function raises an 
   exception, false otherwise *)
let assert_exception func =
  try func(); false
  with _ -> true

(* ------------- UNIT TESTS ------------- *)

(* Test function 'empty_table' *)
let empty_table_test () =
  let _ = Symbol_table.empty_table() in 
  true

(* Test function 'of_alist' with a simple list *)
let of_alist_test () =
  let _ = Symbol_table.of_alist [
    ("x", 1);
    ("y", 5);
  ] in
  true

(* Test function 'of_alist' with a list that has duplicated entries *)
let of_alist_duplicate_entry_test () =
  let res = assert_exception (fun _ -> 
    let _ = Symbol_table.of_alist [
      ("same", 1);
      ("same", 5);
    ] in ()
  ) in
  res

(* Test function 'of_alist' with empty list *)
let of_alist_empty_test () =
  let _ = Symbol_table.of_alist [] in
  true

(* ------------- MAIN ENTRY POINT ------------- *)
type test = {
  func: unit -> bool;
  name: string;
}
let () =
  let testerFun test =
    let res = test.func() in
    if not res then 
      print_test_failed test.name 
    else 
      print_test_passed test.name
  in
  (* Run all the tests *)
  List.iter testerFun [
    { name = "Empty table"; func = empty_table_test };
    { name = "Table from list"; func = of_alist_test };
    { name = "Table from list with duplicate entries"; func = of_alist_duplicate_entry_test };
    { name = "Table from empty list"; func = of_alist_empty_test };
  ]