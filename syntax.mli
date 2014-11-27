(* AST corresponding to veriT proof traces, using smtlib2 terms *)

(* Tokens *)

type numeral = string
type decimal = string
type hexadecimal = string
type binary = string
type symbol = string
type keyword = string

(* S-expressions *)

type spec_constant =
  | Numeral of numeral
  | Decimal of decimal
  | Hexadecimal of hexadecimal
  | Binary of binary
  | String of string

type s_expr =
  | Spec_constant_expr of spec_constant
  | Symbol_expr of symbol
  | Keywork_expr of keyword
  | List_expr of s_expr list

(* Identifier *)

type identifier = symbol * numeral list

(* Sorts - including parametric sorts *)

type sort = Sort of identifier * sort list

(* Attributes *)

type attribute_value =
  | Spec_constant_value of spec_constant
  | Symbol_value of symbol
  | S_expr_list_value of s_expr list 

type attribute = keyword * attribute_value option

(* Terms *)

type qual_identifier = identifier * sort option

type sorted_var = symbol * sort

type var_binding = symbol * term

and term =
  | Spec_constant_term of spec_constant
  | Qual_identifier_term of qual_identifier
  | App_term of qual_identifier * term list
  | Let_term of var_binding list * term
  | Forall_term of sorted_var list * term
  | Exists_term of sorted_var list * term
  | Attributed_term of term * attribute list

(* Theories and logics not implemented *)

(* Command options *)

type b_value = string

type command_option = attribute

(* Info flags *)

type info_flag = keyword

(* Commands *)

type command =
  | Set_logic of symbol
  | Set_option of command_option
  | Set_info of attribute
  | Declare_sort of symbol * numeral
  | Define_sort of symbol * symbol list * sort
  | Declare_fun of symbol * sort list * sort
  | Define_fun of symbol * sorted_var list * sort * term
  | Push of numeral
  | Pop of numeral
  | Assert of term
  | Check_sat
  | Get_assertions
  | Get_proof
  | Get_unsat_core
  | Get_value of term list
  | Get_assignment
  | Get_option of keyword
  | Get_info of info_flag
  | Exit

type script = command list
