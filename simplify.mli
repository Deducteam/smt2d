(* Removes chainable, left-assoc, right-assoc, and pairwise syntactic sugar *)

val simplify: Signature.signature -> Abstract.term -> Abstract.term
