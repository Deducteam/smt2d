(* Translate smtlib2 to dedukti *)

exception Translate_error

val tr_string: string -> Dedukti.ident

val tr_term: Signature.signature -> Abstract.term -> Dedukti.term

val tr_sort_context: Signature.signature -> Dedukti.line list

val tr_fun_context: Signature.signature -> Dedukti.line list

val tr_assertion_bindings: Signature.signature -> (Abstract.term * Dedukti.term) list -> 
			   Dedukti.line list
