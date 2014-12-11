(* Signatures *)

exception Signature_error

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

(* no shadowing/overloading accepting *)
val add_sort:
  Abstract.sort_symbol -> sort_data -> signature -> signature
val add_fun:
  Abstract.fun_symbol -> fun_data -> signature -> signature

val find_sort:
  Abstract.sort_symbol -> signature -> sort_data
val find_fun:
  Abstract.fun_symbol -> signature -> fun_data 
						    
val logic_signature: Abstract.logic_name -> signature
