open Ast

(** Code generation context. A code generation execution creates a new context which is used
    all the way while traversing the abstract syntax tree. *)
type cgcontext = {
  llcontext: Llvm.llcontext;
  symbtbl: Llvm.llvalue Symbol_table.t;
  mutable ibuilder: Llvm.llbuilder;
  llmodule: Llvm.llmodule;
}

(** Returns the equivalent llvm's type given the AST type *)
let rec llvm_type_of ctx = function
    Ast.TypI -> Llvm.i32_type ctx.llcontext
  | Ast.TypB -> Llvm.i1_type ctx.llcontext
  | Ast.TypC -> Llvm.i8_type ctx.llcontext
  | Ast.TypP(t) -> Llvm.pointer_type (llvm_type_of ctx t)
  | Ast.TypV -> Llvm.void_type ctx.llcontext
  | Ast.TypNull -> Llvm.pointer_type (Llvm.void_type ctx.llcontext)
  | Ast.TypA(t, Some size) -> Llvm.array_type (llvm_type_of ctx t) size
  | Ast.TypA(t, None) -> llvm_type_of ctx (Ast.TypP(t))

let rec get_ast_typ lltyp =
  match Llvm.classify_type lltyp with
    Llvm.TypeKind.Integer -> 
      (match Llvm.integer_bitwidth lltyp with
        32 -> Some Ast.TypI (* int has 32 bits *)
      | 1 -> Some Ast.TypB (* bool has 1 bit *)
      | 8 -> Some Ast.TypC (* char has 8 bits *)
      | _ -> None)
  | Llvm.TypeKind.Void -> Some Ast.TypV
  | Llvm.TypeKind.Pointer ->
    (* Get the type of the pointed element *)
    let ptrtyp = get_ast_typ (Llvm.element_type lltyp) in
    if Option.is_none ptrtyp then None 
    else Some(Ast.TypP(Option.get ptrtyp))
  | Llvm.TypeKind.Array ->
    (* Get the type of the array element *)
    let arrtyp = get_ast_typ (Llvm.element_type lltyp) in
    let length = Llvm.array_length lltyp in
    if Option.is_none arrtyp then None 
    else Some(Ast.TypA(Option.get arrtyp, Some(length)))
  | _ -> None

(** Returns an llvm constant of the given type *)
let llvm_const ctx t = Llvm.const_int (llvm_type_of ctx t)

let llvm_null ctx = Llvm.undef (llvm_type_of ctx Ast.TypNull)

(** Returns true if the given LLVM value is a null value *)
let llvm_isnull llval ctx =
  Llvm.is_undef llval && Llvm.type_of llval == Llvm.type_of (llvm_null ctx)

(** Returns the null pointer of the given LLVM value *)
let llvm_null_of llval = 
  llval |> Llvm.type_of |> Llvm.const_pointer_null

(** Same as Llvm.build_store but it handles the case in which it is needed to store a NULL
    to a variable. If that's the case, the stored value is the null pointer of the same 
    type of the variable.
    @param llval value to be stored. May be a generic NULL
    @param llacc variable to which store the value *)
let enhanced_build_store llval llacc ctx = 
  let llval = if llvm_isnull llval ctx then
    llacc |> Llvm.type_of |> Llvm.element_type |> Llvm.const_pointer_null
  else llval in
  Llvm.build_store llval llacc ctx.ibuilder

(** Returns the LLVM's binary op function equivalent to the given AST binary op *)
let llvm_binop_fun = function
    Ast.Add -> Llvm.build_add
  | Ast.Sub -> Llvm.build_sub
  | Ast.Mult -> Llvm.build_mul
  | Ast.Div -> Llvm.build_sdiv
  | Ast.Mod -> Llvm.build_srem
  | Ast.Equal -> Llvm.build_icmp Llvm.Icmp.Eq
  | Ast.Neq -> Llvm.build_icmp Llvm.Icmp.Ne
  | Ast.Less -> Llvm.build_icmp Llvm.Icmp.Slt
  | Ast.Leq -> Llvm.build_icmp Llvm.Icmp.Sle
  | Ast.Greater -> Llvm.build_icmp Llvm.Icmp.Sgt
  | Ast.Geq -> Llvm.build_icmp Llvm.Icmp.Sge
  | Ast.And -> Llvm.build_and
  | Ast.Or -> Llvm.build_or

let llvm_ensure_block_terminator builder after =
  let terminator = Llvm.block_terminator (Llvm.insertion_block builder) in
  if Option.is_none terminator then after builder |> ignore else ()
  
let rec codegen_expr ctx expr =
  match expr.node with
      Ast.ILiteral(lit) -> llvm_const ctx Ast.TypI lit
    | Ast.BLiteral(b) -> llvm_const ctx Ast.TypB (if b then 1 else 0)
    | Ast.CLiteral(ch) -> llvm_const ctx Ast.TypC (Char.code ch)
    | Ast.Null -> llvm_null ctx
    | Ast.UnaryOp(uop, uopexp) -> codegen_uop ctx uop uopexp
    | Ast.BinaryOp(binop, left, right) ->
        (* Generate code for left side of binary op *)
        let llleft = codegen_expr ctx left in
        let fundef, doshortc = try (* get the current function, if any *)
          Llvm.block_parent (Llvm.insertion_block ctx.ibuilder), true
        with _ -> llvm_null ctx, false in (* it is a binop in a global variable *)
        (match binop, doshortc with
          Ast.Or, true | Ast.And, true -> (* Do short circuiting *)
            let bcurr = Llvm.insertion_block ctx.ibuilder in (* current block *)
            let bisneq = Llvm.append_block ctx.llcontext "" fundef in (* block to go if left != false *)
            let isneq_builder = Llvm.builder_at_end ctx.llcontext bisneq in
            let llfalse = llvm_const ctx Ast.TypB 0 in
            let leftneqicmp = if get_ast_typ (Llvm.type_of llleft) == Some(Ast.TypB) then
              llleft (* it is already an icmp, do not create another one *)
            else
              (llvm_binop_fun Ast.Neq) llleft llfalse "" ctx.ibuilder 
            in 
            let current_builder = ctx.ibuilder in (* cache current block builder *)
            ctx.ibuilder <- isneq_builder;
            (* Generate code for right side of binary op *)
            let llright = codegen_expr ctx right in
            let rightneqicmp = if get_ast_typ (Llvm.type_of llright) == Some(Ast.TypB) then
              llright (* it is already an icmp, do not create another one *)
            else
              (llvm_binop_fun Ast.Neq) llright llfalse "" ctx.ibuilder
            in
            (* result if left was true *)
            let llbool = llvm_const ctx Ast.TypB (if binop == Ast.Or then 1 else 0) in
            let bcont = Llvm.append_block ctx.llcontext "" fundef in (* block to continue the execution *)
            let _ = Llvm.build_br bcont ctx.ibuilder in
            (* get the last block. May be changed because of code generation of right statements *)
            let lastblock = Llvm.insertion_block ctx.ibuilder in
            (* come back to the beginning and add br *)
            ctx.ibuilder <- current_builder;
            let _ = if binop == Ast.Or then 
              Llvm.build_cond_br leftneqicmp bcont bisneq ctx.ibuilder 
            else
              Llvm.build_cond_br leftneqicmp bisneq bcont ctx.ibuilder 
            in
            (* go to the end of all the blocks and add phi *)
            Llvm.position_at_end bcont ctx.ibuilder;
            Llvm.build_phi [(llbool, bcurr); (rightneqicmp, lastblock)] "" ctx.ibuilder
        | _ -> (* Do not do short circuiting *)
          (* Generate code for right side of binary op *)
          let llright = codegen_expr ctx right in
          (* Handle the case in which there is NULL on the right or on the left (but not on both sides).
            If NULL bop rightexpr then NULL has to be of the same type of rightexpr. When NULL is on 
            the right, it is specular. *)
          let (llleft, llright) = match llvm_isnull llleft ctx, llvm_isnull llright ctx with
              true, false -> (llvm_null_of llright, llright)
            | false, true -> (llleft, llvm_null_of llleft)
            | _ -> (llleft, llright)
          in
          (llvm_binop_fun binop) llleft llright "binop" ctx.ibuilder)
    | Ast.Access(acc) -> 
        let llacc = codegen_access ctx acc in
        (* Check if accessing an array. Note: llacc is always a pointer *)
        (match Llvm.classify_type(Llvm.element_type (Llvm.type_of llacc)) with
            Llvm.TypeKind.Array -> (* Access(array) means accessing to array's first element *)
              let llzero = llvm_const ctx Ast.TypI 0 in
              Llvm.build_in_bounds_gep llacc [| llzero; llzero |] "firstelement" ctx.ibuilder
          | _ -> Llvm.build_load llacc "access" ctx.ibuilder)
    | Ast.Assign(acc, ex) ->
        let llval = codegen_expr ctx ex in
        let llacc = codegen_access ctx acc in
        (* Build store and handle the case of assignment of NULL*)
        enhanced_build_store llval llacc ctx |> ignore;
        llval
    | Ast.Addr acc -> 
      (* Codegen_access always returns a pointer *)
      codegen_access ctx acc
    | Ast.Call(fname, params) ->
      (* Get the function from the symbol table *)
      let fundeftocall = match Symbol_table.lookup fname ctx.symbtbl with
          None -> Sem_error.raise_missing_fun_declaration expr fname
        | Some fd -> fd
      in
      (* Map each given parameter to LLVM code *)
      let llparams = List.mapi (fun i param ->
        let llgivenparam = codegen_expr ctx param in
        let requiredparam = Llvm.param fundeftocall i in
        (* if null is given as parameter, get the equivalent null pointer *)
        if llvm_isnull llgivenparam ctx then llvm_null_of requiredparam
        else llgivenparam
      ) params in
      (* Build function call *)
      Llvm.build_call fundeftocall (Array.of_list llparams) "" ctx.ibuilder
and codegen_uop ctx uop uopexp =
  match uop, uopexp.node with 
      Ast.Not, _ -> 
        let llex = codegen_expr ctx uopexp in
        Llvm.build_not llex "not" ctx.ibuilder
    | Ast.Neg, _ -> 
        let llex = codegen_expr ctx uopexp in
        Llvm.build_neg llex "neg" ctx.ibuilder
    | incrordecr, Ast.Access(acc) ->  (* Post or pre increment/decrement*)
        let llacc = codegen_access ctx acc in
        let llload = Llvm.build_load llacc "access" ctx.ibuilder in
        let llop = if incrordecr = Ast.PreIncr || incrordecr = Ast.PostIncr then
          (* increment case: add +1 *)
          let llconst_one = llvm_const ctx Ast.TypI 1 in
          Llvm.build_add llload llconst_one "incr" ctx.ibuilder
        else
          (* decrement case: add -1. Simpler to subtracting 1 *)
          let llconst_minus_one = llvm_const ctx Ast.TypI (-1) in
          Llvm.build_add llload llconst_minus_one "decr" ctx.ibuilder 
        in
        (* Store the add operation *)
        Llvm.build_store llop llacc ctx.ibuilder |> ignore;
        (* Return previous or updated value *)
        if incrordecr = Ast.PreDecr || incrordecr = Ast.PreIncr then 
          llop (* updated *)
        else 
          llload (* previous *)
    | _ -> Sem_error.raise_invalid_unary_op_access uopexp
and codegen_access ctx access = (* always returns a pointer *)
  match access.node with
      Ast.AccVar ide -> Option.get(Symbol_table.lookup ide ctx.symbtbl)
    | Ast.AccDeref e -> codegen_expr ctx e
    | Ast.AccIndex(acc, indexpr) -> 
      let llacc = codegen_access ctx acc in
      let llind = codegen_expr ctx indexpr in
      let (llp, indices) = (* codegen_access returns a pointer, check pointed element's type *)
        match Llvm.classify_type(Llvm.element_type (Llvm.type_of llacc)) with
            Llvm.TypeKind.Array -> llacc, [| llvm_const ctx Ast.TypI 0 ; llind |]
          | _ -> (Llvm.build_load llacc "accessptr" ctx.ibuilder, [| llind |]) (* access of index of a pointer *)
      in Llvm.build_in_bounds_gep llp indices "accindex" ctx.ibuilder
  
let rec codegen_stmt ctx retyp stmt =
  match stmt.node with
    Ast.Block stmtordeclist -> 
      Symbol_table.begin_block ctx.symbtbl;
      let has_return = codegen_stmtordeclist ctx retyp stmtordeclist in
      Symbol_table.end_block ctx.symbtbl;
      Option.is_some has_return
  | Ast.Return None -> 
      Llvm.build_ret_void ctx.ibuilder |> ignore;
      true
  | Ast.Return Some(expr) -> (* if return NULL; then return the right NULL pointer type *)
      let genexpr = codegen_expr ctx expr in
      let toberet = if llvm_isnull genexpr ctx then
        Llvm.const_pointer_null retyp
      else genexpr in
        Llvm.build_ret toberet ctx.ibuilder |> ignore;
      true
  | Ast.Expr e -> codegen_expr ctx e |> ignore; false
  | Ast.If(guard, thbr, elbr) -> 
    let icmp = codegen_expr ctx guard in
    let fundef = Llvm.block_parent (Llvm.insertion_block ctx.ibuilder) in
    let bthen = Llvm.append_block ctx.llcontext "then" fundef in
    let belse = Llvm.append_block ctx.llcontext "else" fundef in
    let bcont = Llvm.append_block ctx.llcontext "cont" fundef in
    let then_builder = Llvm.builder_at_end ctx.llcontext bthen in
    let else_builder = Llvm.builder_at_end ctx.llcontext belse in
    let current_builder = ctx.ibuilder in
    let _ = Llvm.build_cond_br icmp bthen belse current_builder in
    ctx.ibuilder <- then_builder;
    let _ = codegen_stmt ctx retyp thbr in
    llvm_ensure_block_terminator then_builder (Llvm.build_br bcont);
    ctx.ibuilder <- else_builder;
    let _ = codegen_stmt ctx retyp elbr in
    llvm_ensure_block_terminator else_builder (Llvm.build_br bcont);
    ctx.ibuilder <- current_builder;
    Llvm.position_at_end bcont current_builder;
    false
  | Ast.While(guard, body) ->
    let fundef = Llvm.block_parent (Llvm.insertion_block ctx.ibuilder) in
    let bcond = Llvm.append_block ctx.llcontext "condition" fundef in
    let bbody = Llvm.append_block ctx.llcontext "whilebody" fundef in
    let bcont = Llvm.append_block ctx.llcontext "cont" fundef in
    let cond_builder = Llvm.builder_at_end ctx.llcontext bcond in
    let body_builder = Llvm.builder_at_end ctx.llcontext bbody in
    let current_builder = ctx.ibuilder in
    let _ = bcond
    |> Llvm.build_br 
    |> llvm_ensure_block_terminator current_builder in
    ctx.ibuilder <- cond_builder;
    let icmp = codegen_expr ctx guard in
    let _ = Llvm.build_cond_br icmp bbody bcont cond_builder in
    ctx.ibuilder <- body_builder;
    let _ = codegen_stmt ctx retyp body in
    Llvm.build_br bcond |> llvm_ensure_block_terminator body_builder;
    ctx.ibuilder <- current_builder;
    Llvm.position_at_end bcont current_builder;
    false
and codegen_stmtordeclist ctx retyp stmtordeclist = List.find_opt (function
    { node = Ast.Dec(vd); _ } -> 
      let llinit = match vd.init with 
        None -> None  
      | Some ex -> Some(codegen_expr ctx ex)
      in 
      codegen_local_vardecl ctx vd.typ vd.vname llinit;
      false
  | { node = Ast.Stmt(stmt); _ } -> 
      codegen_stmt ctx retyp stmt
  ) stmtordeclist
and get_default_value ctx typ = (* Return the default llvalue of the given type *)
  match typ with
  | Ast.TypI -> llvm_const ctx Ast.TypI 0 (* integer's default value is 0 *)
  | Ast.TypB -> llvm_const ctx Ast.TypB 0 (* boolean's default value is false (0) *)
  | Ast.TypC -> llvm_const ctx Ast.TypC 0 (* char's default value is '\0' (0) *)
  | Ast.TypP(t) -> Llvm.const_pointer_null (llvm_type_of ctx t) (* pointer's default value is NULL pointer of pointer's type *)
  | Ast.TypA(t, Some size) -> (* array of size s has s default values *)
    let default_values = Array.init size (fun _ -> get_default_value ctx t) in
    Llvm.const_array (Llvm.type_of(get_default_value ctx t)) default_values
  | _ -> Llvm.undef (llvm_type_of ctx typ)
and codegen_local_vardecl ctx typ ide init = 
    let llval = Llvm.build_alloca (llvm_type_of ctx typ) ide ctx.ibuilder in
    Symbol_table.add_entry ide llval ctx.symbtbl;
    match init with 
        None -> () (* to give default values local variables, call get_default_value ctx typ *)
      | Some llinit -> (* may happen that the variable init type is NULL *)
        enhanced_build_store llinit llval ctx |> ignore   

(** [codegen_global_vardecl vd ctx]
    Define global variable [vd]. If an initial value is not specified,
    the variable is initialized with a default value. *)
let codegen_global_vardecl vd ctx =
  let llinit = match vd.init with 
    None -> get_default_value ctx vd.typ
  | Some ex -> codegen_expr ctx ex
  in
  let llval = Llvm.define_global vd.vname llinit ctx.llmodule in
  Symbol_table.add_entry vd.vname llval ctx.symbtbl

(** [codegen_fundecl fundecl body ctx] 
    Do code generation of function's body [body] *)
let codegen_fundecl fundecl body ctx =
  let fundef = Option.get(Symbol_table.lookup fundecl.fname ctx.symbtbl) in (* safe because it is always declared before *)
  Symbol_table.begin_block ctx.symbtbl;
  Llvm.position_at_end (Llvm.entry_block fundef) ctx.ibuilder;
  (* build each parameter as local variable *)
  List.iteri (fun i (typ, ide) -> 
    let param = Llvm.param fundef i in
    codegen_local_vardecl ctx typ ide (Some param)
  ) fundecl.formals;
  (* get the function body *)
  let stmtdeclis = match body.node with
    Ast.Block(stmtordeclist) -> stmtordeclist
  | _ -> Sem_error.raise_invalid_function_body body in
  let retyp = llvm_type_of ctx fundecl.typ in
  (* generate function body *)
  codegen_stmtordeclist ctx retyp stmtdeclis |> ignore;
  Symbol_table.end_block ctx.symbtbl;
  (* Check if the function has a return as last element in the block.
     If the function returns void, the source code may not have a final 
     return statement. *)
  llvm_ensure_block_terminator ctx.ibuilder ( 
    if fundecl.typ = Ast.TypV then Llvm.build_ret_void
    else Llvm.build_ret (Llvm.undef retyp) (* will never happen with a semantically checked ast *)
  )

(** [llvm_function_of ctx fundecl] 
    Get the function type of [fundecl] *)
let llvm_function_of ctx fundecl =
  let params = List.map (fun (f, _) -> llvm_type_of ctx f) fundecl.formals in
  let retyp = llvm_type_of ctx fundecl.typ in
  Llvm.function_type retyp (Array.of_list params)

(** [codegen_define_function ctx fundecl]
    Define function but not generate code for its body [fundecl] *)
let codegen_define_function ctx fundecl = 
  let funtyp = llvm_function_of ctx fundecl in
  let fundef = Llvm.define_function fundecl.fname funtyp ctx.llmodule in
  Symbol_table.add_entry fundecl.fname fundef ctx.symbtbl
  |> ignore

let to_llvm_module (Ast.Prog(topdecls)) llmodule =
  let llcontext = Llvm.module_context llmodule in
  let ctx = {
    llcontext; 
    llmodule; 
    symbtbl = Symbol_table.empty_table();
    ibuilder = Llvm.builder llcontext;
  } in
  (* Utility function to loop through global variables and functions of a LLVM module *)
  let llvm_iter_topdecls iterator llm =
    Llvm.iter_globals iterator llm;
    Llvm.iter_functions iterator llm
  in
  (* Add the already generated llvm content to the symbol table *)
  llvm_iter_topdecls (fun llval ->
    let name = Llvm.value_name llval in
    Symbol_table.add_entry name llval ctx.symbtbl
  ) ctx.llmodule;
  (* Declare run-time support functions and global variables. Do not fail if the RTS is 
     already declared. If it is already declared then it is always a correct declaration 
     because it is checked during type checking phase *)
  Rtsupport.iter (fun td ->  
    try
      match td with
        Ast.Fundecl(fundecl) -> 
          let funtyp = llvm_function_of ctx fundecl in
          let fundef = Llvm.declare_function fundecl.fname funtyp ctx.llmodule in
            Symbol_table.add_entry fundecl.fname fundef ctx.symbtbl 
      | Ast.Vardec(v) -> 
        codegen_global_vardecl v ctx
    with Symbol_table.DuplicateEntry(_) -> ()
    (* Ignored because it means run time support is already declared *)
  );
  (* Do not generate functions body yet. Just declare all the global top declarations *)
  List.iter (function 
      { node = Ast.Fundecl fundecl; _ } -> codegen_define_function ctx fundecl
    | { node = Ast.Vardec v; _ } -> codegen_global_vardecl v ctx
  ) topdecls;
  (* Finally run code generation of functions body *)
  List.iter (function
      { node = Ast.Fundecl { typ; fname; formals; body = Some(blk) }; _ } -> 
        codegen_fundecl { typ; fname; formals; body = Some(blk) } blk ctx
    | _ -> ()
  ) topdecls;
  llmodule

let set_target_info llmodule =
  Llvm_all_backends.initialize (); (* needed to find the right target via by_triple function *)
  let triple = Llvm_target.Target.default_triple () in
  let target = Llvm_target.Target.by_triple triple in
  let machine = Llvm_target.TargetMachine.create ~triple:triple target in
  let data_layout = Llvm_target.TargetMachine.data_layout machine 
  |> Llvm_target.DataLayout.as_string in
  Llvm.set_data_layout data_layout llmodule;
  Llvm.set_target_triple triple llmodule;
  machine