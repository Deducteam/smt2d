(* smtlib2 terms abstract syntax *)

(* *** STRUCTURES KEPT FROM CONCRETE SYNTAX *** *)

(* TOKENS *)

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

(* Attributes *)

type attribute_value =
  | Spec_constant_value of spec_constant
  | Symbol_value of symbol
  | S_expr_list_value of s_expr list 

(* *** NEW STRUCTURES *** *)

type sort_symbol =
  | Bool
  | Identifier_sort of identifier
type fun_symbol =
  | True
  | False
  | Not
  | Imply
  | And
  | Or
  | Xor
  | Eq
  | Distinct
  | Ite
  | Spec_constant_fun of spec_constant
  | Identifier_fun of identifier
type attribute_name = keyword
type sort_parameter = symbol
type variable = symbol

(* Sorts *)

type sort = 
  | Sort of sort_symbol * sort list

type parametric_sort =
  | Parameter of sort_parameter
  | Parametric_sort of parametric_sort

(* Terms *)

type attribute = attribute_name * attribute_value option

type term =
  | Var of variable
  | App of fun_symbol * sort option * term list
  | Let of (variable * term) list * term
  | Forall of (variable * sort) list * term
  | Exists of (variable * sort) list * term
  | Attributed of term * attribute list
