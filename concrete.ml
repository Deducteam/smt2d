(* smtlib2 concrete syntax *)

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
  | Keyword_expr of keyword
  | List_expr of s_expr list

(* Identifier *)

type identifier = symbol * numeral list

(* Sorts *)

type core_sort = CBool

type sort = 
  | Sort of identifier * sort list
  | Core_sort of core_sort * sort list

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

and core_fun =
  | CTrue
  | CFalse
  | CNot
  | CImply
  | CAnd
  | COr
  | CXor
  | CEqual
  | CDistinct
  | CIte

and term =
  | Spec_constant_term of spec_constant
  | App_term of qual_identifier * term list (* can be empty *)
  | Core_app_term of core_fun * term list
  | Let_term of var_binding list * term
  | Forall_term of sorted_var list * term
  | Exists_term of sorted_var list * term
  | Attributed_term of term * attribute list

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

(* Parsing functions *)

let parse_sort ((sym, nums) as id) sorts =
  match sym, nums with
  | "Bool", [] -> Core_sort (CBool, sorts)
  | _, _ -> Sort (id, sorts)

let parse_fun (((sym, nums), _) as qual_id) terms =
  match sym, nums with
  | "true", [] -> Core_app_term (CTrue, terms)
  | "false", [] -> Core_app_term (CFalse, terms)
  | "not", [] -> Core_app_term (CNot, terms)
  | "=>", [] -> Core_app_term (CImply, terms)
  | "and", [] -> Core_app_term (CAnd, terms)
  | "or", [] -> Core_app_term (COr, terms)
  | "xor", [] -> Core_app_term (CXor, terms)
  | "=", [] -> Core_app_term (CEqual, terms)
  | "distinct", [] -> Core_app_term (CDistinct, terms)
  | "ite", [] -> Core_app_term (CIte, terms)
  | _, _ -> App_term (qual_id, terms)

