open Utils
open Microc

let tests = [
(
"Function declared in another file returns int but not char",
["void main() {
  char c = takeChar();
}";
"int takeChar() {
  return 10;
}"
]
);
]

let run_test sourcecodes =
  try
    let programs = List.map (fun sourcecode -> 
      let lexbuf = Lexing.from_string ~with_positions:true sourcecode in
      Parsing.parse "" Scanner.next_token lexbuf
    ) sourcecodes in
      programs |> Linker.link |>
      Semantic_analysis.type_check |>
      ignore; false
  with 
  | Sem_error.Semantic_error _ -> true
  | _ -> false

let run_tests =
  (* Run all the tests *)
  List.map (fun test -> 
    { name = fst test; func = (fun () -> run_test (snd test)); reqResult = true }
    |> Utils.testerFun
  ) tests