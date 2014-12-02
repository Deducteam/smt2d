(* smtlib2 abstract syntax *)

module Old = Concrete

type sort_symbol = Old.identifier
type fun_symbol =
  | Spec_constant_fun of Old.spec_constant
  | Identifier_fun of Old.identifier
type attribute_name = Old.keyword
type sort_parameter = Old.symbol
type variable = Old.symbol
type attribute_value = Old.attribute_value
    
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

let sort_sym s = s, []				   
let fun_sym s = Identifier_fun (s, [])

(* Sorts *)
			       
let par s = Par s
let p_sort sort_sym p_sorts = P_sort (sort_sym, p_sorts)
