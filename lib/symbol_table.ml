exception DuplicateEntry of Ast.identifier

type 'a t = {
  (* key -> list of values, one for each level, if any *)
  tbl: (Ast.identifier, 'a table_entry) Hashtbl.t;
  (* current block level *)
  mutable current_level: int;
}
and 'a table_entry = {
  value: 'a;
  lvl: int;
}
(*and TableBlock = None | Table of TableBlock * (Ast.identifier, 'a) Hashtbl.t;*)

let rec clean_old_entries key symbol_table = 
  match Hashtbl.find_all symbol_table.tbl key with
    []   -> ()
  | x::_ -> if x.lvl > symbol_table.current_level then
              let _ = Hashtbl.remove symbol_table.tbl key in
              clean_old_entries key symbol_table
            else
              ()

let lookup_table_entry key symbol_table = 
  clean_old_entries key symbol_table;
  Hashtbl.find_opt symbol_table.tbl key

let empty_table () = 
  { tbl = Hashtbl.create 256; current_level = 0; }

(* Just increase the current level. There isn't any overhead because any hashtable is created *)
let begin_block symbol_table = 
  symbol_table.current_level <- symbol_table.current_level + 1

(* Just decrease the current level. There isn't any overhead because any hashtable is destroyed *)
let end_block symbol_table = 
  if symbol_table.current_level > 0 then
    symbol_table.current_level <- symbol_table.current_level - 1
  else
    ()

(* Get list of values associated to key and drop the once with level > current_level.
   Return the next element, which has level <= current_level *)
let lookup key symbol_table =
  match lookup_table_entry key symbol_table with
    None        -> None
  | Some entry  -> Some entry.value

(* Lookup key and if there is an element, throw DuplicateEntry if element's level
   is equal to current_level, otherwise add key into hashmap *)
let add_entry key data symbol_table = 
  let new_entry = { value = data; lvl = symbol_table.current_level; } in
  match lookup_table_entry key symbol_table with
    None    ->  Hashtbl.add symbol_table.tbl key new_entry
  | Some x  ->  if x.lvl = symbol_table.current_level then
                  raise(DuplicateEntry(key))
                else
                  Hashtbl.add symbol_table.tbl key new_entry

let of_alist (lis: (Ast.identifier * 'a) list) =
  let newSymbolTable = empty_table () in
  List.iter (fun (k, d) -> add_entry k d newSymbolTable) lis;
  newSymbolTable