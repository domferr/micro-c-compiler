let link programs = 
  Ast.Prog(
    List.concat_map (fun (Ast.Prog(topdecls)) -> topdecls) programs
  )