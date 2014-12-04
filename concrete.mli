(* smtlib2 terms concrete syntax *)

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
