(* Assertion-set stack *)

exception Set_stack_error

type set_stack

val create: Signature.signature -> set_stack

val all: set_stack -> Signature.signature * Abstract.term list

val push: set_stack -> int -> unit
			    
val pop: set_stack -> int -> unit

val add_sort: set_stack -> Abstract.sort_symbol -> Signature.sort_data -> unit

val add_fun: set_stack -> Abstract.fun_symbol -> Signature.fun_data -> unit

val add_assertion: set_stack -> Abstract.term -> unit
