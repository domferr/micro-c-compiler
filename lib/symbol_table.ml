exception DuplicateEntry of Ast.identifier

type 'a t = {
  (* key -> list of values, one for each level, if any *)
  tbl: (Ast.identifier, 'a table_entry) Hashtbl.t;
  (* list of identifiers for each block, sorted by scope level *)
  mutable identifiers: (Ast.identifier list) list;
  (* current block level *)
  mutable current_level: int;
}
and 'a table_entry = {
  value: 'a;
  lvl: int;
}

let empty_table () = 
  { tbl = Hashtbl.create 256; current_level = 0; identifiers = [] }

let show symbol_table =
  let print_identifiers idx lis =
    if idx > 0 then Printf.printf "::["
    else Printf.printf "[";
    
    let elem_printer idx elem =
      if idx == 0 then Printf.printf "%s" elem
      else Printf.printf "; %s" elem
    in
      List.iteri elem_printer lis; 
      Printf.printf "]"
  in
  Printf.printf "lvl: %d, identifiers: " symbol_table.current_level;
  List.iteri print_identifiers symbol_table.identifiers;
  Printf.printf "\n"
  
(* Just increase the current level. There isn't any overhead because any hashtable is created *)
let begin_block symbol_table = 
  symbol_table.current_level <- symbol_table.current_level + 1;
  symbol_table.identifiers   <- []::symbol_table.identifiers

(* Just decrease the current level and remove each identifier in this block.
   There isn't any overhead given by any hashtable destruction. *)
let end_block symbol_table = 
  if symbol_table.current_level = 0 then 
    () 
  else (
    symbol_table.current_level <- symbol_table.current_level - 1;
    match symbol_table.identifiers with
      []    -> ()
    | x::xs -> List.iter (Hashtbl.remove symbol_table.tbl) x;
              symbol_table.identifiers <- xs
  )

(* Return the data entry value associated to the given key, if any *)
let lookup key symbol_table =
  match Hashtbl.find_opt symbol_table.tbl key with
    None        -> None
  | Some entry  -> Some entry.value

(* Lookup key and if there is an element, throw DuplicateEntry if element's level
   is equal to current_level, otherwise add key into hashmap *)
let add_entry key data symbol_table =
  (match Hashtbl.find_opt symbol_table.tbl key with
    None        ->  ()
  | Some entry when entry.lvl = symbol_table.current_level
                ->  raise(DuplicateEntry(key))
  | _           ->  ()
  );
  let new_entry = { value = data; lvl = symbol_table.current_level; } in
  Hashtbl.add symbol_table.tbl key new_entry;
  symbol_table.identifiers <- match symbol_table.identifiers with
    [] -> [[key]]
    | x::xs -> let newLis = key::x in newLis::xs

let of_alist (lis: (Ast.identifier * 'a) list) =
  let newSymbolTable = empty_table () in
  List.iter (fun (k, d) -> add_entry k d newSymbolTable) lis;
  newSymbolTable