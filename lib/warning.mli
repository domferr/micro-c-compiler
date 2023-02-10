(** The type of a warning. It has a message string and the position in the source code *)
type warning = {
    msg: string;
    loc: Location.code_pos;
}

(** Creates a new warning with the given message string and code position *)
val create: string -> Location.code_pos -> warning

(** Prints the given warning into the given output channel *)
val fprintf: out_channel -> warning -> string -> unit