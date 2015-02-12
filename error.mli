(* raise and print errors *)

val get_location: Lexing.lexbuf -> string * int * int

val print_location_error: int -> int -> string -> unit

exception Not_implemented
