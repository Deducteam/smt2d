(* smtlib2 abstract syntax *)

exception Abstract_error

(* STRUCTURE and building functions *)

(* Basic types *)

type number = int
type sort_symbol = Concrete.identifier
type fun_symbol = private
  | Spec_constant_fun of Concrete.spec_constant
  | Identifier_fun of Concrete.identifier
val spec_constant_fun: Concrete.spec_constant -> fun_symbol
val identifier_fun: Concrete.identifier -> fun_symbol
type attribute_name = Concrete.keyword
type theory_name = string
type sort_parameter = Concrete.symbol
type variable = Concrete.symbol
type attribute_value = Concrete.attribute_value
type logic_name = string

(* Sorts *)

type sort = private
  | Sort of sort_symbol * sort list
  | Bool
val sort: sort_symbol -> sort list -> sort
val bool: sort

type parametric_sort = private
  | Param of sort_parameter
  | Par_sort of sort_symbol * parametric_sort list
  | Par_bool
val param: sort_parameter -> parametric_sort
val par_sort: sort_symbol -> parametric_sort list -> parametric_sort
val par_bool: parametric_sort

(* Terms *)

type attribute = attribute_name * attribute_value option

type core_app = private
  | True
  | False
  | Not of term
  | Imply of term * term
  | And of term * term
  | Or of term * term
  | Xor of term * term
  | Equal of term * term
  | Distinct of term * term
  | Ite of term * term * term

and term = private
  | Var of variable
  | App of fun_symbol * sort option * term list
  | Core of core_app
  | Let of (variable * term) list * term
  | Forall of (variable * sort) list * term
  | Exists of (variable * sort) list * term
  | Attributed of term * attribute list
val t_var: variable -> term
val t_app: fun_symbol -> sort option -> term list -> term
val t_let: (variable * term) list -> term -> term
val t_forall: (variable * sort) list -> term -> term
val t_exists: (variable * sort) list -> term -> term
val t_attributed: term -> attribute list -> term
val t_true: term
val t_false: term
val t_not: term -> term
val t_imply: term -> term -> term
val t_and: term -> term -> term
val t_or: term -> term -> term
val t_xor: term -> term -> term
val t_equal: term -> term -> term
val t_distinct: term -> term -> term
val t_ite: term -> term -> term -> term

(* theories *)

type sort_declaration = private
    { sort_symbol: sort_symbol; number: number; attributes: attribute list; }
val sort_declaration: sort_symbol -> number -> attribute list -> sort_declaration

type par_fun_declaration = private
    { sort_parameters: sort_parameter list; fun_symbol: fun_symbol;
      parametric_sorts: parametric_sort list; parametric_sort: parametric_sort;
      attributes: attribute list; }
val par_fun_declaration: sort_parameter list -> fun_symbol -> parametric_sort list ->
			 parametric_sort -> attribute list -> par_fun_declaration

type theory_declaration = private
    { theory_name: theory_name; sort_declarations: sort_declaration list;
      par_fun_declarations: par_fun_declaration list; }
val theory_declaration: theory_name -> sort_declaration list ->
			par_fun_declaration list -> theory_declaration

(* Logics *)

type logic_declaration = logic_name * theory_name list

(* Command options and info names *)

type command_option = attribute

type info_flag = attribute_name
			
(* Commands *)

type command = private
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
val c_set_logic: logic_name -> command
val c_set_option: command_option -> command
val c_set_info: attribute -> command
val c_declare_sort: sort_symbol -> number -> command
val c_define_sort: sort_symbol -> sort_parameter list -> parametric_sort -> command
val c_declare_fun: fun_symbol -> sort list -> sort -> command
val c_define_fun: fun_symbol -> (variable * sort) list -> sort -> term -> command
val c_push: number -> command
val c_pop: number -> command
val c_assert: term -> command
val c_check_sat: command
val c_get_assertions: command
val c_get_value: term list -> command
val c_get_assignment: command
val c_get_proof: command
val c_get_unsat_core: command
val c_get_info: info_flag -> command
val c_get_option: attribute_name -> command
val c_exit: command

type script = command list
      
(* *** CONCRETE TO ABSTRACT *** *)

type varset

val empty_vars: varset
       
val tr_term: varset -> Concrete.term -> term
		      
val tr_command: Concrete.command -> command

(* *** UTILS *** *)

val substitute_par_sort: (sort_parameter * sort) list -> parametric_sort -> sort

(* *** CONSTANTS *** *)

val bool_sym: sort_symbol
val true_sym: fun_symbol
val false_sym: fun_symbol
val not_sym: fun_symbol
val imply_sym: fun_symbol
val and_sym: fun_symbol
val or_sym: fun_symbol
val xor_sym: fun_symbol
val equal_sym: fun_symbol
val distinct_sym: fun_symbol
val ite_sym: fun_symbol

val core_declaration: theory_declaration
val qf_uf_declaration: logic_declaration  

(* Get theories and logics declarations from their names *)

val get_theory_declaration:
  theory_name -> theory_declaration
val get_logic_declaration:
  logic_name -> logic_declaration
