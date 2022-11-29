# MicroC Compiler

MicroC is a subset of the language C with the following simplification:

* It supports only integers (`int`), characters (`char`) and booleans (`bool`) as scalar values, array and pointers as compound data types;
* There are no structures, unions, doubles, function pointer;
* No dynamic allocation of memory;
* No multi-dimensional arrays;
* No shorthand for initialize variable during declaration;
* Functions can only return `void`, `int`, `bool`, `char`;
* No pointer arithmetic;
* Pointers and arrays are not interchangeable;
* no separate compilation, all the code of a program must stay in a unique compilation unit;
* there are only two library functions
```C
void print(int)  // it outputs an integer to standard output
int getint()     // it inputs an integer from standard input 
```

As part of the exam students will be asked to implement a compiler for MicroC using the OCaml language, the tools and the techniques presented during the course. 
The project is made of four assignments that are released incrementally during the course, so that students can start working on the project before the end of classes (actually, the last assignment is released in the last lecture).


## Setup your development environment

A description of the provided project kit is [here](SETUP.md).

## MicroC assignments

The project is split in the following assignments:

* **Parsing**: this assignment asks students to implement a parser for MicroC. The material is available [here](microc-parser/); 

* **Semantic analysis**: this assignment mainly concerns the implementation of a static analysis for check that a given program obeys the scoping rule and the type system of the language. The material is available here;

* **Code generation**: this assignment asks to use the LLVM toolchain to compile a MicroC program to low level code (LLVM bitcode) and to perform some simple optimizations. The material is available here; 

* **Language extensions**: this assignment asks to extend the MicroC language by considering further constructs. In particular, students are required to implement **at least three** of the following items: 
    * pre/post increment/decrement operators, i.e., `++` and `--`, and  abbreviation for assignment operators, i.e., `+=`, `-=`, `*=`, `/=` and `%=`;
    * `do-while` loop, variable declaration with initialization and multiple declarations, e.g., `int i = 0, *j = &z;`;
    * pointers, arrays & multi-dimensional arrays as in C;
    * floating point arithmetic and strings as in C, i.e. null-terminated arrays of characters;
    * structs, `sizeof`, bitwise and comma operators;
    * a new semantic analysis pass to detect deadcode;
    * seperate compilation. 

To take the exam students must submit their solutions to the teacher via email by the deadline reported on the exam registration portal.
The submitted solution must include the OCaml code, the documentation of the code, and a report describing the design and the implementation choices adopted.


# MicroC Parser

The goal of this assignment is to implement a parser for MicroC. 
You must use `ocamllex` for implementing the scanner and `menhir` for the parser.

## MicroC syntax
In the following there is the specification of the syntax of MicroC.
Note that the syntax is a small variant of the syntax of the C language.

### Lexical elements

* Identifiers starts with a letter or an underscore and then can contain letters, underscore and numbers, e.g., `i`, `_local_var`, `string_of_int32`;

* Integer literals are sequence of digits in base 10 or digits in base 16 prefixed with `0x` (integers are 32bit values), e.g., `32`, `1024`, `3232`, `0xFF`, `0x10`;

* Character literals have the form `'c'` where c is a character, e.g., `a`, `A`, `1`, special characters are `\'`,`\b`, `\f`, `\t`, `\\`, `\r`, and `\n`;

* Boolean literals are `true` and `false`;

* Keywords are: `if`, `return`, `else`, `for`, `while`, `int`, `char`, `void`, `NULL`, `bool`;

* Operators are: &,  +, -, *, /, %,  =, ==, !=, <, <=, >, >=, &&, ||, !

* Other symbols: (, ), {, }, [, ], &, ;

* Comments:
    * `//...` single line comments;
    * `/* ... */` multi line comments.

The operators have the following precedence and associativity:

    right    =             /* lowest precedence */
    left     ||
    left     &&
    left     ==  != 
    nonassoc >  <  >=  <=
    left     +  - 
    left     *  /  %
    nonassoc !  &
    nonassoc [             /* highest precedence  */



### Syntax

Here is an ambiguous grammar for MicroC where tokens with no semantic values are enclosed between quotes, e.g., `"("`, whereas tokens with semantic values are capitalized, e.g., `INT`. 
As usual in EBNF notation, the operator `*` means zero or more occurrences, `+` means one or more occurrences, and `?` means at most one.

    Program ::= Topdecl* EOF
    
    Topdecl ::= Vardecl ";"  | Fundecl
    
    Vardecl ::= Typ Vardesc
    
    Vardesc ::= ID | "*" Vardesc | "(" Vardesc ")" | Vardesc "[" "]" | Vardesc "[" INT "]" 
    
    Fundecl ::= Typ ID "("((Vardecl ",")* Vardecl)? ")" Block
    
    Block ::= "{" (Stmt | Vardecl ";")* "}"
    
    Typ ::= "int" | "char" | "void" | "bool" 
    
    Stmt ::= "return" Expr? ";" | Expr? ";" | Block | "while" "(" Expr ")" Stmt 
        |    "for" "(" Expr? ";" Expr? ";" Expr? ")" Stmt
        |    "if" "(" Expr ")" Stmt "else" Stmt  | "if" "(" Expr ")" Stmt

    Expr ::= RExpr | LExpr

    LExpr ::= ID | "(" LExpr ")" | "*" LExpr | "*" AExpr | LExpr "[" Expr "]"

    RExpr ::= AExpr | ID "(" ((Expr ",")* Expr)? ")" | LExpr "=" Expr | "!" Expr 
        |  "-" Expr | Expr BinOp Expr 

    BinOp ::= "+" | "-" | "*" | "%" | "/" | "&&" | "||" | "<" | ">" | "<=" | ">=" | "==" | "!="

    AExpr ::= INT | CHAR | BOOL | "NULL" | "(" RExpr ")" | "&" LExpr

## Result of the parsing
The parsing phase produces an AST whose node are annotated with a location in the code, if the program is syntactically correct, 
otherwise it raises an error.

## Requirement to build the code
The code requires:
* OCaml >= 4.12.0
* Menhir >= 20210419
* ppx_deriving >= 5.2 

You can install the required dependencies via `opam`
```sh
$ opam install menhir ppx_deriving
```
[Here](https://github.com/ocaml-ppx/ppx_deriving), you can the documentation of `ppx_deriving`.

## Building the code and running the tests
Typing `make` will generate a dummy `bin/microcc.exe` executable and a testing `test/parser_test.exe`:
```
$ make
```

To clean-up the folder, run:
```
$ make clean
```

To test your parser you can use the files in `test/samples` directory, for example
```
$ dune exec test/parser_test.exe -- samples/test-hello.mc
```

## Directory structure #

Read about the organization of the project [here](../SETUP.md#project-structure).

## The source code

The `lib/` directory contains the modules for each phase of the compiler. 
Your code will stay there.

More precisely, the `lib/` directory provides:

    ast.ml                       <-- Definition of the abstract syntax tree of MicroC 
    location.ml                  <-- The module Location provides two data types to represent code locations
    location.mli                 <-- The interface of the module Location   
    parser.mly                   <-- Menhir specification of the grammar
    parsing.ml                   <-- The module Parsing implements the parser 
    parsing.mli                  <-- The interface of the module Parsing  
    scanner.mll                  <-- ocamllex specification of the scanner 
    scanner.mli                  <-- The interface of the module Scanner

**The assignment requires you to complete the implementation of `scanner.mll`, `parser.mly` and `parsing.ml`.**

# MicroC semantic analysis

The goal of this assignment is to implement a semantic checker, i.e., types and name usage, for MicroC.

## The static semantics of MicroC

MicroC mainly follows the same rules of language C with some exceptions described below.

### Semantic rules
MicroC adopts a static scoping: 
* Block can be nested and the declaration of a variable `x` in a inner block hides possible declarations in outer blocks;
* There is a global scope that contains variables and function declarations;
* Functions cannot be nested;
* No function overloading.

MicroC does not support separate compilation, thus, each source file must provide the definition of the `main` function.
The signature of the `main` can be:
* `int main()` when it returns an integer;
* `void main()` when it returns no value.

We assume there exist two library functions to perform I/O operations:
* `void print(int)` to print an integer on the standard output;
* `int getint()` to read an integer from the standard input.

### Typing rules

MicroC types are defined by the following grammar

    T ::= int | bool | char | void | T* | T[]

where `T[]` denote the type of an array of `T` values and `T*` the type of pointers to `T`.

Note that there are no type coercion:
* booleans and characters cannot be converted to integers;
* arrays and pointers are not interchangeable but are different types;
* arithmetic operators expect only integer values;
* logical operators expect only boolean values;
* dereference operator expects a pointer;
* in `a[i]` we expect `a` to be an array and `i` to be an integer value;
* only functions can be invoked;
* a function call must provides a number of arguments equals to the parameters of the function;
* conditional guards in `if` and `while` statements expect boolean values.

Finally, observe that:
* array should have a size of at least 1 element;
* array cannot be assigned, i.e., `array1 = array2` is not allowed;
* variables of type `void` are not allowed;
* function pointers are not supported yet;
* currently, functions only return `int`, `bool`, `char` and `void`;
* multi-dimensional arrays are not supported, yet. 

## Result of the semantic analysis
The semantic analysis phase returns the AST of the program, if the program is well typed, otherwise it raises an error.

## Requirement to build the code
The code requires:
* OCaml >= 4.12.0
* Menhir >= 20210419
* ppx_deriving >= 5.2  

You can install the required dependencies via `opam`
```sh
$ opam install menhir ppx_deriving
```
[Here](https://github.com/ocaml-ppx/ppx_deriving), you can the documentation of `ppx_deriving`.

## Building the code and running the tests
Typing `make` will generate a dummy `bin/microcc.exe` executable and a testing `test/semant_test.exe`:
```
$ make
```

To clean-up the folder, run:
```
$ make clean
```

To test your analyzer you can use the files in `test/samples` directory, for example
```
$ dune exec test/semant_test.exe -- samples/test-hello.mc
```

## Directory structure #

Read about the organization of the project [here](../SETUP.md#project-structure).

## The source code

The `lib/` directory contains the modules for each phase of the compiler. 
Your code will stay there.

More precisely, the `lib/` directory provides:

    ast.ml                       <-- Definition of the abstract syntax tree of MicroC 
    microcc.ml                   <-- The file from which build the executable 
    location.ml                  <-- The module Location provides two data types to represent code locations
    location.mli                 <-- The interface of the module Location   
    parser.mly                   <-- Menhir specification of the grammar
    parsing.ml                   <-- The module Parsing implements the parser
    parsing.mli                  <-- The interface of the module Parsing  
    scanner.mll                  <-- ocamllex specification of the scanner 
    scanner.mli                  <-- The interface of the module Scanner
    symbol_table.ml              <-- The module Symbol_table provides the implementation of a symbol table
    symbol_table.mli             <-- The interface of the module Symbol_table
    semantic_analysis.ml         <-- The module Semantic_analysis implements the semantic checker
    semantic_analysis.mli        <-- The interface of the module Semantic_analysis

**The assignment requires you to complete the implementation of `symbol_table.ml` and `semantic_analysis.ml`.**