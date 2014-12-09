(* Assertion-set stack *)

exception Set_stack_error

(* non empty list *)
type set_stack = (Signature.signature * Abstract.term list) list
	    
val push: int -> set_stack -> set_stack
			    
val pop: int -> set_stack -> set_stack

val add_sort: Abstract.sort_symbol -> Signature.sort_data
	      -> set_stack -> set_stack

val add_fun: Abstract.fun_symbol -> Signature.fun_data
	      -> set_stack -> set_stack
