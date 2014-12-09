(* smtlib2 abstract syntax *)

type number = int
type sort_symbol
type fun_symbol
type attribute_name = string
type theory_name = string
type sort_parameter
type variable
type attribute_value
type logic_name = string

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

(* Command options and info names *)

type command_option = attribute

type info_flag = attribute_name
			
(* Commands *)

type command =
  | Set_logic of logic_name
  | Set_option of command_option
  | Set_info of attribute
  | Declare_sort of sort_symbol * number
  | Define_sort of sort_symbol * sort_parameter list * parametric_sort
  | Declare_fun of fun_symbol * sort list * sort
  | Define_fun of fun_symbol * (variable * sort) list * sort * term
  | Push of number
  | Pop of number
  | Assert of term
  | Check_sat
  | Get_assertions
  | Get_value of term list
  | Get_assignment
  | Get_proof
  | Get_unsat_core
  | Get_info of info_flag
  | Get_option of attribute_name
  | Exit

type script = command list
      
(* *** CONCRETE TO ABSTRACT *** *)

type varset
		      
val term: varset -> Concrete.term -> term
		      
val command: Concrete.command -> command

(* Sorts are parametric sorts *)
val par_sort_of_sort: sort -> parametric_sort

(* *** CONSTANTS *** *)

val core_declaration: theory_declaration
val qf_uf_declaration: logic_declaration  
