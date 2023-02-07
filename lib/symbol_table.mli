(** Exception trown when trying to add into the symbol table an identifier that
    has already a binding in the current block/scope *)
exception DuplicateEntry of Ast.identifier

(** Type of the element stored in the symbol table *)
type 'a t

(** Creates an empty symbol table *)
val empty_table : unit -> 'a t

(** Prints the content of the symbol table *)
val show : 'a t -> unit

(** Starts a new block *)
val begin_block : 'a t -> unit

(** Retrieves the content of the current block/scope *)
val get_current_block : 'a t -> (Ast.identifier * 'a) list

(** Ends the current block *)
val end_block : 'a t -> unit

(** Adds a new symbol into the current block/scope *)
val add_entry : Ast.identifier -> 'a -> 'a t -> unit

(** Retrieves the current binding of the given identifier inside the current block/scope *)
val lookup : Ast.identifier -> 'a t -> 'a option

(** Creates a new list representing a copy of the symbol table *)
val of_alist : (Ast.identifier * 'a) list -> 'a t