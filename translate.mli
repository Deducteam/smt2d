(* Translate smtlib2 to dedukti *)

exception Translate_error

val tr_string: string -> Dedukti.ident

val tr_term: Signature.signature -> Expand.term -> Dedukti.term

val tr_prelude: string -> Dedukti.line

val tr_sort_context: Signature.signature -> Dedukti.line list

val tr_fun_context: Signature.signature -> Dedukti.line list

val tr_assertions: Signature.signature -> Abstract.term list ->  Dedukti.line list

val print_context: out_channel -> Signature.signature -> Abstract.term list -> string -> unit
