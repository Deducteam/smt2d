(* find sorts of terms *)

exception Sort_error

val get_sort: Signature.signature -> Abstract.term -> Abstract.sort
