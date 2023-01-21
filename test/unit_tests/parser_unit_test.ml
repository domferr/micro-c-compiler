open Utils
open Microc

let tests = [
(
"Missing closing '}'",
"int fun() {
  return;

"
);
(
"Cannot use negative size of array",
"void main() {
  int a[-2]; /* cannot use negative size of array */
}"
);
(
"Cannot declare array of voids",
"void main() {
  void a[2]; /* cannot declare array of voids */
}"
);
]

let run_test sourcecode =
  let lexbuf = Lexing.from_string ~with_positions:true sourcecode in 
  try
    lexbuf |>
    Parsing.parse Scanner.next_token |>
    ignore; true
  with 
  | Scanner.Lexing_error _ -> false
  | Parsing.Syntax_error _ -> true

  let run_tests =
    (* Run all the tests *)
    tests |>
    List.map (fun test -> 
      { name = fst test; func = (fun () -> run_test (snd test)); reqResult = true }
      |> Utils.testerFun
    )