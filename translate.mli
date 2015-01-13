(* Translate smtlib2 to dedukti *)

val translate_string: string -> Dedukti.ident

val translate_prelude: string -> Dedukti.line

val translate_sort_context: Signature.signature -> Dedukti.line list

val translate_fun_context: Signature.signature -> Dedukti.line list

val translate_assertions: Signature.signature -> Abstract.term list ->  Dedukti.line list

val print_context: out_channel -> Signature.signature -> Abstract.term list -> string -> unit
