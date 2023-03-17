![logo](./logo.png)

# MicroC Compiler

MicroC is a subset of the language C with the following simplification:

* It supports only integers (`int`), characters (`char`) and booleans (`bool`) as scalar values, array and pointers as compound data types;
* There are no structures, unions, doubles, function pointer;
* No dynamic allocation of memory;
* there are only four library functions
```C
void print(int)  // it outputs an integer to standard output
int getint()     // it inputs an integer from standard input 
void printchar(char)  // it outputs a char to standard output
void printbool(bool)  // it outputs a boolean to standard output
```

## Setup your development environment

A description of the provided project kit is [here](SETUP.md).

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
Typing `make` will generate a dummy `bin/microcc.exe` executable:
```
$ make
```

To clean-up the folder, run:
```
$ make clean
```

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

### Semantic rules
MicroC adopts a static scoping: 
* Block can be nested and the declaration of a variable `x` in a inner block hides possible declarations in outer blocks;
* There is a global scope that contains variables and function declarations;
* Functions cannot be nested;
* No function overloading.

The signature of the `main` can be:
* `int main()` when it returns an integer;
* `void main()` when it returns no value.

### Typing rules

MicroC types are defined by the following grammar

    T ::= int | bool | char | void | T* | T[]

where `T[]` denote the type of an array of `T` values and `T*` the type of pointers to `T`.

Note that there are no type coercion:
* booleans and characters cannot be converted to integers;
* logical operators expect only boolean values;
* in `a[i]` we expect `a` to be an array and `i` to be an integer value;
* only functions can be invoked;
* a function call must provides a number of arguments equals to the parameters of the function;
* conditional guards in `if` and `while` statements expect boolean values.
* variables of type `void` are not allowed;
* function pointers are not supported;
