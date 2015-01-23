(* Translate smtlib2 to dedukti *)

exception Translate_error

val tr_string: string -> Dedukti.ident

val tr_term: Signature.signature -> Expand.term -> Dedukti.term

val tr_sort_context: Signature.signature -> Dedukti.line list

val tr_fun_context: Signature.signature -> Dedukti.line list

val tr_assertions: Signature.signature -> Abstract.term list -> (Dedukti.line * Dedukti.term) list
