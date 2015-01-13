(* find sorts of terms *)

exception Sort_error

val get_sort: Signature.signature -> Abstract.term -> Abstract.sort

val get_par_sort: Signature.signature -> Abstract.sort_parameter ->
		  Abstract.parametric_sort list -> Abstract.term list -> Abstract.sort
