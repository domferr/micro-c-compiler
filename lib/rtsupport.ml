open Ast

let sourcepath = "./bin/rt-support.c"

let ast =
  [Ast.Fundecl({
    typ = Ast.TypI;
    fname = "getint";
    formals = [];
    body = None;
  });
  Ast.Fundecl({
    typ = Ast.TypV;
    fname = "print";
    formals = [(Ast.TypI, "num")];
    body = None;
  });
  Ast.Fundecl({
    typ = Ast.TypV;
    fname = "printchar";
    formals = [(Ast.TypC, "ch")];
    body = None;
  });
  Ast.Fundecl({
    typ = Ast.TypV;
    fname = "printbool";
    formals = [(Ast.TypB, "b")];
    body = None;
  });]

let iter iterfn = List.iter iterfn ast