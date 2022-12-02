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

let assert_lookup key data tbl =
  match Symbol_table.lookup key tbl with
    None   -> false
  | Some x -> if x == data then true else false

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

(* Test function 'add_entry' with one element and an empty table *)
let add_entry_simple_test () =
  let tbl = Symbol_table.empty_table() in
  let _ = Symbol_table.add_entry "x" 1 tbl in
  true

(* Test function 'add_entry' with one element that is already inside the table *)
let add_entry_duplicate_test () =
  let tbl = Symbol_table.empty_table() in
  let _ = Symbol_table.add_entry "x" 1 tbl in
  let res = assert_exception (fun _ -> 
    Symbol_table.add_entry "x" 1 tbl
  ) in
  res

(* Test function 'begin_block' *)
let begin_block_simple_test () =
  let tbl = Symbol_table.empty_table() in
  let _ = Symbol_table.add_entry "x" 1 tbl in
  let _ = Symbol_table.begin_block tbl in
  let _ = Symbol_table.add_entry "x" 1 tbl in
  true

(* Test function 'lookup' *)
let lookup_simple_test () =
  let tbl = Symbol_table.empty_table() in
  let _ = Symbol_table.add_entry "x" 6 tbl in
  let _ = Symbol_table.add_entry "z" 9 tbl in
  let _ = Symbol_table.add_entry "y" 2 tbl in
  let _ = Symbol_table.begin_block tbl in
  match Symbol_table.lookup "x" tbl with
      None   -> false
    | Some x -> if x == 6 then true else false
  
(* Test function 'lookup' into empty table *)
let lookup_empty_test () =
  let tbl = Symbol_table.empty_table() in
  match Symbol_table.lookup "x" tbl with
      None   -> true
    | Some _ -> false

(* Test function 'lookup' into a table with multiple blocks *)
let lookup_with_blocks_test () =
  let tbl = Symbol_table.empty_table() in
  let _ = Symbol_table.add_entry "x" 6 tbl in
  let _ = Symbol_table.add_entry "z" 9 tbl in
  let _ = Symbol_table.add_entry "y" 2 tbl in
  let _ = Symbol_table.begin_block tbl in
  let _ = Symbol_table.add_entry "x" 392 tbl in
  let okX = assert_lookup "x" 392 tbl in
  let okZ = assert_lookup "z" 9 tbl in
  let okY = assert_lookup "y" 2 tbl in
  okX && okZ && okY

(* Test function 'end_block' *)
let end_block_lookup_test () =
  let tbl = Symbol_table.empty_table() in
  let _ = Symbol_table.add_entry "x" 6 tbl in
  let _ = Symbol_table.begin_block tbl in
  let _ = Symbol_table.add_entry "x" 392 tbl in
  let _ = Symbol_table.end_block tbl in
  assert_lookup "x" 6 tbl

(* Test function 'end_block' with multiple values and blocks *)
let end_block_lookup_multiple_blocks_test () =
  let tbl = Symbol_table.empty_table() in
  let _ = Symbol_table.add_entry "x1" 1 tbl in
  let _ = Symbol_table.begin_block tbl in
  let _ = Symbol_table.add_entry "x1" 2 tbl in
  let _ = Symbol_table.begin_block tbl in
  let _ = Symbol_table.add_entry "x1" 3 tbl in
  let _ = Symbol_table.begin_block tbl in
  let _ = Symbol_table.add_entry "x1" 4 tbl in
  let _ = Symbol_table.begin_block tbl in
  let _ = Symbol_table.add_entry "x1" 5 tbl in
  let _ = Symbol_table.begin_block tbl in
  let _ = Symbol_table.add_entry "x1" 6 tbl in
  let rec check value =
    let _ = Symbol_table.end_block tbl in
    if value = 0 then 
      true
    else
      if assert_lookup "x1" value tbl then 
        check (value - 1)
      else
        false  
  in
  if assert_lookup "x1" 6 tbl then
    check 5
  else
    false

(* Test function 'end_block' with empty table *)
let end_block_empty_table_test () =
  let tbl = Symbol_table.empty_table() in
  let _ = Symbol_table.end_block tbl in
  true

(* Test function 'end_block' with empty table *)
let end_block_one_level_table_test () =
  let tbl = Symbol_table.empty_table() in
  let _ = Symbol_table.add_entry "x" 1 tbl in
  let _ = Symbol_table.add_entry "y" 2 tbl in
  let _ = Symbol_table.add_entry "z" 3 tbl in
  let okX = assert_lookup "x" 1 tbl in
  let okZ = assert_lookup "y" 2 tbl in
  let okY = assert_lookup "z" 3 tbl in
  if okX && okZ && okY then
    let _ = Symbol_table.end_block tbl in
    let okX = assert_lookup "x" 1 tbl in
    let okZ = assert_lookup "y" 2 tbl in
    let okY = assert_lookup "z" 3 tbl in
    okX && okZ && okY
  else
    false

(* ------------- MAIN ENTRY POINT ------------- *)
type test = {
  func: unit -> bool;
  name: string;
}
let () =
  let testerFun test =
    let res = test.func() in    if not res then 
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
    { name = "Add entry"; func = add_entry_simple_test };
    { name = "Add duplicate entry"; func = add_entry_duplicate_test };
    { name = "Begin block"; func = begin_block_simple_test };
    { name = "Lookup"; func = lookup_simple_test };
    { name = "Lookup into empty table"; func = lookup_empty_test };
    { name = "Lookup into a table with some blocks"; func = lookup_with_blocks_test };
    { name = "End block"; func = end_block_lookup_test };
    { name = "End block and lookup with multiple blocks"; func = end_block_lookup_multiple_blocks_test };
    { name = "End block with empty table"; func = end_block_empty_table_test };
    { name = "End block with a table of one level"; func = end_block_one_level_table_test };
  ]