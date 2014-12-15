(* Print Dedukti terms *)

val print_var : out_channel -> Dedukti.var -> unit
val print_term : out_channel -> Dedukti.term -> unit
val print_terms : out_channel -> Dedukti.term list -> unit
val print_line : out_channel -> Dedukti.line -> unit
