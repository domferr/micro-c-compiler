open Utils

let iter_results header = 
  Printf.printf "\n----- %s\n" header;
  List.fold_left (fun count res -> 
    Utils.print_test_result res;
    if res.pass then ((fst count)+1, snd count)
    else (fst count, (snd count)+1)
  ) (0, 0)

let print_summary header count = 
  Printf.printf "\n[ Summary | %s ]\n" header;
  Printf.printf "Total tested sources: %d\n" (fst count + snd count);
  Printf.printf "Pass: %d\t\tFail: %d\n" (fst count) (snd count)

let () =
  let parser_results = Parser_unit_test.run_tests in
  let symtbl_results = Symbol_table_unit_test.run_tests in
  let semanalisys_results = Semantic_analysis_unit_test.run_tests in
  let sepcomp_results = Separate_compilation_unit_test.run_tests in
  let parser_count = iter_results "Parser" parser_results in
  let symtbl_count = iter_results "Symbol Table" symtbl_results in
  let semant_count = iter_results "Semantic Analysis" semanalisys_results in
  let sepcom_count = iter_results "Separate Compilation" sepcomp_results in
  let total_tests_pass = fst parser_count + fst symtbl_count + fst semant_count + fst sepcom_count in
  let total_tests_fail = snd parser_count + snd symtbl_count + snd semant_count + snd sepcom_count in
  Printf.printf "\n\n[ Summary ]--------------------------\n";
  Printf.printf "Total tested sources: %d\n" (total_tests_pass + total_tests_fail);
  Printf.printf "Pass: %d\tFail: %d\n" total_tests_pass total_tests_fail;
  print_summary "Parser" parser_count;
  print_summary "Symbol table" symtbl_count;
  print_summary "Semantic Analysis" semant_count;
  print_summary "Separate Compilation" sepcom_count