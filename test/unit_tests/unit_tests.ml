open Utils

let () =
  let parser_results = Parser_unit_test.run_tests in
  let symtbl_results = Symbol_table_unit_test.run_tests in
  let semanalisys_results = Semantic_analysis_unit_test.run_tests in
  Printf.printf "--- Parser\n";
  let parser_count = List.fold_left (fun count res -> 
    Utils.print_test_result res;
    if res.pass then ((fst count)+1, snd count)
    else (fst count, (snd count)+1)
  ) (0, 0) parser_results in
  Printf.printf "\n--- Symbol Table\n";
  let symtbl_count = List.fold_left (fun count res -> 
    Utils.print_test_result res;
    if res.pass then ((fst count)+1, snd count)
    else (fst count, (snd count)+1)
  ) (0, 0) symtbl_results in
  Printf.printf "\n--- Semantic Analysis\n";
  let sem_count = List.fold_left (fun count res -> 
    Utils.print_test_result res;
    if res.pass then ((fst count)+1, snd count)
    else (fst count, (snd count)+1)
  ) (0, 0) semanalisys_results in
  let total_tests_pass = fst parser_count + fst symtbl_count + fst sem_count in
  let total_tests_fail = snd parser_count + snd symtbl_count + snd sem_count in
  Printf.printf "\n\n[ Summary ]--------------------------\n";
  Printf.printf "Total tested sources: %d\n" (total_tests_pass + total_tests_fail);
  Printf.printf "Pass: %d\tFail: %d\n" total_tests_pass total_tests_fail;
  Printf.printf "\n[ Summary | Parser ]-----------------\n";
  Printf.printf "Total tested sources: %d\n" (fst parser_count + snd parser_count);
  Printf.printf "Pass: %d\t\tFail: %d\n" (fst parser_count) (snd parser_count);
  Printf.printf "\n[ Summary | Symbol table ]-----------\n";
  Printf.printf "Total tested sources: %d\n" (fst symtbl_count + snd symtbl_count);
  Printf.printf "Pass: %d\tFail: %d\n" (fst symtbl_count) (snd symtbl_count);
  Printf.printf "\n[ Summary | Semantic Analysis ]------\n";
  Printf.printf "Total tested sources: %d\n" (fst sem_count + snd sem_count);
  Printf.printf "Pass: %d\tFail: %d\n" (fst sem_count) (snd sem_count)