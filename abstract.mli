(* smtlib2 abstract syntax *)

type number = int
type sort_symbol
type fun_symbol
type attribute_name
type theory_name
type sort_parameter
type variable
type attribute_value
type logic_name

(* Sorts *)

type sort =
  | Sort of sort_symbol * sort list

type parametric_sort =
  | Par of sort_parameter
  | Par_sort of sort_symbol * parametric_sort list

(* Terms *)

type attribute = attribute_name * attribute_value option

type term =
  | Var of variable
  | App of fun_symbol * sort option * term list
  | Let of (variable * term) list * term
  | Forall of (variable * sort) list * term
  | Exists of (variable * sort) list * term
  | Attributed of term * attribute list

(* Theories *)

type sort_declaration = sort_symbol * number * attribute list

type par_fun_declaration = 
    sort_parameter list * fun_symbol * parametric_sort list * parametric_sort * attribute list

type theory_declaration = theory_name * sort_declaration list * par_fun_declaration list

(* Logics *)

type logic_declaration = logic_name * theory_name list

(* *** CONSTANTS *** *)

val core_declaration: theory_declaration
val qf_uf_declaration: logic_declaration  

(* *** SIGNATURES *** *)

(* type sort_data *)
(* type fun_data *)
type signature

(* val add_sort: sort_symbol -> sort_data -> signature -> signature *)
(* val add_fun: fun_symbol -> fun_data -> signature -> signature *)

(* *** SCOPING *** *)

val logic_signature: Concrete.symbol -> signature

(* val declare_sort: Concrete.symbol -> Concrete.numeral -> sort_symbol * sort_data *)

(* val define_sort: Concrete.symbol -> Concrete.symbol list -> Concrete.sort -> environment ->  *)
(* 		 sort_symbol * sort_data *)

(* val declare_fun: Concrete.symbol -> Concrete.sort list -> Concrete.sort -> environment ->  *)
(* 		 fun_symbol * fun_data *)

(* val define_fun: Concrete.symbol -> Concrete.sorted_var list -> Concrete.sort ->  *)
(* 			Concrete.term -> environment -> (fun_symbol * fun_data) list *)
							
(* val in_line_assert: Concrete.term -> environment -> (fun_symbol * fun_data) list * term *)

(* val in_line_get_value: Concrete.term list -> environment -> (fun_symbol * fun_data) list *)
