open Utils
open Microc

let tests = [
(
"Invalid post-increment of char",
"void main() {
  char i;
  i++; /* post-increment a char */
}"
);
(
"Invalid post-increment of bool",
"void main() {
  bool i;
  i++; /* post-increment a bool */
}"
);
(
"Invalid pre-increment of bool",
"void main() {
  bool i;
  ++i; /* pre-increment a bool */
}"
);
(
"Invalid pre-increment of char",
"void main() {
  char i;
  ++i; /* pre-increment a char */
}"
);
(
"Cannot declare array without size",
"void main() {
  int a[]; /* cannot declare array without size */
}"
);
(
"Cannot use char as index of array",
"void main() {
  int a[2];
  int b;
  char i;
  b = a[i]; /* cannot use char as index of array */
}"
);
(
"Cannot use boolean as index of array",
"void main() {
  int a[2];
  int b;
  bool i;
  b = a[i]; /* cannot use boolean as index of array */
}"
);
(
"Cannot use indexing on non-array",
"void main() {
  int a;
  int b;
  b = a[0]; /* cannot use indexing on non-array */
}"
);
(
"Cannot assign bool constant to int",
"void main() {
  int a;
  a = false; /* cannot assign bool constant to int */
}"
);
(
"Cannot assign char to int",
"void main() {
  int a;
  a = 'c'; /* cannot assign char to int */
}"
);
(
"Cannot assign char to bool",
"void main() {
  bool b;
  b = 'c'; /* cannot assign char to bool */
}"
);
(
"Cannot assign int constant to bool",
"void main() {
  bool b;
  b = 10; /* cannot assign int constant to bool */
}"
);
(
"Cannot assign bool to int",
"void main() {
  bool b;
  int i;
  b = i; /* cannot assign bool to int */
}"
);
(
"Cannot assign char to bool",
"void main() {
  bool b;
  char c;
  b = c; /* cannot assign char to bool */
}"
);
(
"Cannot assign char to int",
"void main() {
  int i;
  char c;
  i = c; /* cannot assign char to int */
}"
);
(
"Cannot assign bool to int",
"void main() {
  int i;
  bool b;
  i = b; /* cannot assign bool to int */
}"
);
(
"Cannot assign char to bool",
"void main() {
  bool b;
  char c;
  b = c; /* cannot assign char to bool */
}"
);
(
"Cannot assign pointer of int to bool",
"void main() {
  int *p;
  bool b;
  b = *p; /* cannot assign pointer of int to bool */
}"
);
(
"Cannot assign array to int",
"void main() {
  int a[5];
  int b[6];
  a[4] = b; /* cannot assign array to int */
}"
);
(
"Cannot assign array to array",
"void main() {
  int a[5];
  int b[6];
  a = b; /* cannot assign array to array */
}"
);
(
"Cannot assign int to array",
"void main() {
  int a[5];
  int b[6];
  a = b[2]; /* cannot assign int to array */
}"
);
(
"Cannot assign array to array",
"void main() {
  int a[5];
  int b[5];
  a = b; /* cannot assign array to array */
}"
);
(
"Cannot assign void to int",
"void foo() {
    
}

void main() {
    int a;
    a = foo(); /* cannot assign void to int */
}"
);
(
"Cannot compare int with bool",
"void main() {
  bool res;
  int a;
  bool b;
  res = a < b; /* cannot compare int with bool */
}"
);
(
"Cannot compare int with bool",
"void main() {
  bool res;
  bool a;
  int b;
  res = a < b; /* cannot compare int with bool */
}"
);
(
"Cannot compare char with int",
"void main() {
  bool res;
  int a;
  char b;
  res = a < b; /* cannot compare char with int */
}"
);
(
"Cannot assign char to bool",
"void main() {
  bool b;
  char c;
  b = c; /* cannot assign char to bool */
}"
);
(
"Cannot compare char with int",
"void main() {
  bool res;
  char a;
  int b;
  res = a < b; /* cannot compare char with int */
}"
);
(
"Cannot compare char with bool",
"void main() {
  bool res;
  bool a;
  char b;
  res = a < b; /* cannot compare char with bool */
}"
);
(
"Cannot sum int with bool",
"void main() {
  int res;
  int a;
  bool b;
  res = a + b; /* cannot sum int with bool */
}"
);
(
"Cannot subtract int with bool",
"void main() {
  int res;
  int a;
  bool b;
  res = a - b; /* cannot subtract int with bool */
}"
);
(
"Cannot divide int with bool",
"void main() {
  int res;
  int a;
  bool b;
  res = a / b; /* cannot divide int with bool */
}"
);
(
"Cannot multiply int with bool",
"void main() {
  int res;
  int a;
  bool b;
  res = a * b; /* cannot multiply int with bool */
}"
);
(
"Cannot multiply int with char",
"void main() {
  int res;
  int a;
  char b;
  res = a * b; /* cannot multiply int with char */
}"
);
(
"Cannot divide int with char",
"void main() {
  int res;
  int a;
  char b;
  res = a / b; /* cannot divide int with char */
}"
);
(
"Cannot subtract int with char",
"void main() {
  int res;
  int a;
  char b;
  res = a - b; /* cannot subtract int with char */
}"
);
(
"Cannot sum int with char",
"void main() {
  int res;
  int a;
  char b;
  res = a + b; /* cannot sum int with char */
}"
);
(
"Function not declared",
"int main() {
  foo(); /* foo not declared */
}"
);
(
"Function requires one int argument but nothing is given",
"void foo(int a) {

}

int main() {
    foo(); /* foo requires one int argument but nothing is given */
}"
);
(
"Function requires one int argument but two integers are given",
"void foo(int a) {

}

int main() {
    int a;
    int b;
    foo(a, b); /* foo requires one int argument but two integers are given */
}"
);
(
"Function requires int and char but int and bool is given",
"void foo(int a, char c) {

}

int main() { 
    int a;
    bool b;
    foo(a, b); /* foo requires int and char but int and bool is given */
}"
);
(
"Assignment of int to a bool",
"void main() {
  int a;
  bool b = a; /* assignment of int to a bool */
}"
);
(
"Assignment of char to a bool",
"void main() {
  char a;
  bool b = a; /* assignment of char to a bool */
}"
);
(
"Assignment of int to a bool",
"void main() {
  int a[10];
  bool b = a[1]; /* assignment of int to a bool */
}"
);
(
"Assignment of int to a bool",
"void main() {
  int a[10];
  int *b = a; /* assignment of int to a bool */
}"
);
(
"Assignment of bool to a int",
"bool f = false;

void main() {
    int b = f; /* assignment of bool to a int */
}"
);
(
"Assignment of int with a variable not declared",
"void main() {
  int b = a; /* variable 'a' not declared */
  int a;
}"
);
(
"Assignment of bool to int",
"bool isBoolean() {
  return true;
}

void main() {
  int b = isBoolean(); /* assignment of bool to int */
}"
);
(
"Variable not declared",
"void main() {
  int a, b = c; /* c not declared */
}"
);
(
"Variable not declared",
"void main() {
  int a, b = c, c; /* c not declared */
}"
);
(
"Function not declared",
"void main() {
  int a, b = foo(), c; /* foo not declared */
}"
);
(
"Invalid for condition",
"void main() {
  int i;
  for( ; i == false; ) {

  }
}"
);
(
"Main function cannot return bool",
"bool main() { /* main cannot return bool */
    
}"
);
(
"Main function has a not valid parameter",
"int main(int a) { /* not valid parameter */
  bool b;
  b = false;
  int a[];
  return a;
}"
);
(
"Redefinition of 'main' as different kind of symbol",
"int main; /* redefinition of 'main' as different kind of symbol */

int main() {
    return 0;
}"
);
(
"Assignment of NULL to a int",
"void main() {
  int a;
  a = NULL;
}"
);
(
"Assignment of NULL to a char",
"void main() {
  char a;
  a = NULL;
}"
);
(
"Assignment of NULL to a bool",
"void main() {
  bool a;
  a = NULL;
}"
);
(
"Assignment of pointer to a int",
"void main() {
  int p;
  int b;
  b = *p;  /* p is not a pointer */
}"
);
(
"Missing return value",
"int main() {
  return;  /* missing return value */
}"
);
(
"Missing return",
"int main() {
  /* missing return */
}"
);
(
"Missing return value",
"int main() {
  bool b;
  if (b) { 
      return; /* missing return value */
  } else {
      return 1;
  }
}"
);
(
"Missing return",
"int main() {
  if (true) {
      /* missing return */
  } else {
      return 1;
  }
}"
);
(
"Function not declared used in return statement",
"int main() {
  return foo(); /* Foo not declared */
}"
);
(
"Invalid return type",
"bool foo() { 
  return 1; /* foo returns boolean, not int */
}

int main() {
  foo();
  return 0;
}"
);
(
"Invalid return type",
"bool foo() { 
  return false;
}

int main() {
  return foo(); /* foo returns boolean, not int */
}"
);
(
"Use of '-' on boolean",
"void main() {
  bool a;
  int c;
  c = -a; /* Use of '-' on boolean */
}"
);
(
"Use of '-' on char",
"void main() {
  char a;
  int c;
  c = -a; /* Use of '-' on char */
}"
);
(
"Cannot negate int",
"void main() {
  int a;
  bool c;
  c = !a; /* cannot negate int */
}"
);
(
"Cannot negate char",
"void main() {
  char a;
  bool c;
  c = !a; /* cannot negate char */
}"
);
(
"Missing previous declaration",
"void main() {
  b = true; /* missing previous declaration of 'b' */
}"
);
(
"Missing previous declaration",
"void main() {
  bool b;
  b = var; /* missing previous declaration of 'var' */
}"
)
]

let run_test sourcecode =
  let lexbuf = Lexing.from_string ~with_positions:true sourcecode in 
  try
    lexbuf |>
    Parsing.parse Scanner.next_token |>
    Semantic_analysis.type_check |>
    ignore; false
  with 
  | Scanner.Lexing_error _ | Parsing.Syntax_error _ -> 
    false
  | Sem_error.Semantic_error _ -> true

let run_tests =
  (* Run all the tests *)
  List.map (fun test -> 
    { name = fst test; func = (fun () -> run_test (snd test)); reqResult = true }
    |> Utils.testerFun
  ) tests