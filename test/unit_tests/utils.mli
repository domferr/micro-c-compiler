type test = {
  func: unit -> bool;
  name: string;
  reqResult: bool;
}

type test_result = {
  name: string;
  pass: bool;
}

val load_file: string -> string
val testerFun: test -> test_result
val print_test_result: test_result -> unit