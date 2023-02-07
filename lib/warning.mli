(** The type of a warning. It has a message string and the position in the source code *)
type warning = {
    msg: string;
    loc: Location.code_pos;
}

(** Creates a new warning with the given message string and code position *)
val create: string -> Location.code_pos -> warning

(** Prints the given warning into the standard output (stdout) *)
val printf: warning -> string -> unit