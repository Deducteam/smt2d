(* Signatures *)

type sort_data =
  | Sort_declaration of int
  | Sort_definition of
      Abstract.sort_parameter list * Abstract.parametric_sort

type fun_data =
  | Fun_declaration of
      (Abstract.sort_parameter list *
	 Abstract.parametric_sort list * Abstract.parametric_sort) list
  | Fun_definition of
      (Abstract.variable * Abstract.sort) list *
	Abstract.sort * Abstract.term
		      
type signature

val empty: signature
val add_sort: Abstract.sort_symbol -> sort_data -> signature -> signature
val add_fun: Abstract.fun_symbol -> fun_data -> signature -> signature
val overload_fun: Abstract.fun_symbol -> fun_data -> signature -> signature
