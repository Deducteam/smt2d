val get_location: Lexing.lexbuf -> string * int * int

val print_location_error: int -> int -> string -> unit

exception Lexer_error of string * int * int
exception Parser_error of string * int * int
exception Script_error of int * int
exception Logic_error of string * int * int

	    
