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

(* *** CONCRETE TO ABSTRACT *** *)

val logic_name: Concrete.symbol -> logic_name

(* *** ASSERTION SETS *** *)

type assertion_set = signature * term list

(* *** LOGIC SIGNATURES *** *)

val logic_signature: logic_name -> signature

(* *** RUN COMMANDS *** *)

val run_command: Concrete.command -> assertion_set list -> assertion_set list
