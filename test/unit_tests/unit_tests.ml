let () =
  let parser_results = Parser_unit_test.run_tests in
  let symtbl_results = Symbol_table_unit_test.run_tests in
  let semanalisys_results = Semantic_analysis_unit_test.run_tests in
  Printf.printf "--- Parser\n";
  List.iter Utils.print_test_result parser_results;
  Printf.printf "\n--- Symbol Table\n";
  List.iter Utils.print_test_result symtbl_results;
  Printf.printf "\n--- Semantic Analysis\n";
  List.iter Utils.print_test_result semanalisys_results;