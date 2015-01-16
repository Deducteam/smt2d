(* Removes chainable, left-assoc, right-assoc, and pairwise syntactic sugar *)

type term = private Abstract.term

val expand: Signature.signature -> Abstract.term -> term
