(* smtlib2 abstract syntax *)

type sort_symbol
type fun_symbol
type attribute_name
type sort_parameter
type variable
type attribute_value

(* Sorts *)

type sort =
  | Sort of sort_symbol * sort list

type parametric_sort =
  | Par of sort_parameter
  | P_sort of sort_symbol * parametric_sort list

(* Terms *)

type attribute = attribute_name * attribute_value option

type term =
  | Var of variable
  | App of fun_symbol * sort option * term list
  | Let of (variable * term) list * term
  | Forall of (variable * sort) list * term
  | Exists of (variable * sort) list * term
  | Attributed of term * attribute list

(* *** FUNCTIONS *** *)

val sort_sym: string -> sort_symbol
val fun_sym: string -> fun_symbol
				   
val par: string -> parametric_sort
val p_sort: sort_symbol -> parametric_sort list -> parametric_sort
