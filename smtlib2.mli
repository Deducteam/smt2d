(* AST corresponding to veriT proof traces, using smtlib2 terms *)


type smtterm =
  | Var of symbol
  | Fun of symbol * smtterm list
  | Let of smtvarbinding list * smtterm
  | Core of smtcore

and smtvarbinding =
  | Varbinding of symbol * smtterm

and smtcore =
  | True
  | False
  | Not of smtterm
  | Imply of smtterm list
  | And of smtterm list
  | Or of smtterm list
  | Xor of smtterm list
  | Eq of smtterm list
  | Distinct of smtterm list
  | Ite of smtterm * smtterm * smtterm

----

(* Tokens *)

type numeral = int
type decimal = string
type hexadecimal = string
type binary = string
type string = string_token
type symbol = string
type keyword = string

(* S-expressions *)

type spec_constant =
  | Numeral of numeral
  | Decimal of decimal
  | Hexadecimal of hexadecimal
  | Binary of binary
  | String of string_token

type s_expr =
  | Spec_constant_expr of spec_constant
  | Symbol_expr of symbol
  | Keywork_expr of keyword
  | List_expr of s_expr list

(* Identifier *)

type identifier = symbol * numeral list

(* Sorts *)

type sort = identifier * sort list

(* Attributes *)

type attribute_value =
  | Spec_constant_value of spec_constant
  | Symbol_value of symbol
  | S_expr_list_value of s_expr list 

type attribute =
  | Keyword of keyword
  | Set of keyword * attribute_value

(* Command options *)

type b_value = True | False

type option =
  | Print_success of b_value
  | Expand_definitions of b_value
  | Interactive_mode of b_value
  | Produce_proofs of b_value
  | Produce_unsat_cores of b_value
  | Produce_models of b_value
  | Produce_assignments of b_value
  | Regular_output_channel of string_token
  | Diagnostic_output_channel of string_token
  | Random_seed of numeral
  | Verbosity of numeral
  | Attibute of attribute

(* Commands *)

type command =
  | Set_logic of symbol
  | Set_option of option
  | Set_info of attribute
  | Declare_sort of symbol * numeral
  | Define_sort of symbol * symbol list * sort
  | Declare_fun (* continuer ici *)
  | Define_fun
  | Push
  | Pop
  | Assert
  | Check_sat
  | Get_assertions
  | Get_values
  | Get_proof
  | Get_unsat_core
  | Get_info
  | Get_option
  | Exit
