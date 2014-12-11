(* remove syntaxic sugar of binary function symbol annoted with
   :chainable, :left-assoc, :right-assoc, and :pairwise *)

val unfold: Signature.signature -> Abstract.term -> Abstract.term
