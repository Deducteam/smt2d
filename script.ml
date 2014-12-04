(* smtlib2 scripts concrete syntax *)

(* Command options *)

type b_value = string

type command_option = Concrete.attribute

(* Info flags *)

type info_flag = Concrete.keyword

(* Commands *)

type command =
  | Set_logic of Concrete.symbol
  | Set_option of command_option
  | Set_info of Concrete.attribute
  | Declare_sort of Concrete.symbol * Concrete.numeral
  | Define_sort of Concrete.symbol * Concrete.symbol list * Concrete.sort
  | Declare_fun of Concrete.symbol * Concrete.sort list * Concrete.sort
  | Define_fun of Concrete.symbol * Concrete.sorted_var list * Concrete.sort * Concrete.term
  | Push of Concrete.numeral
  | Pop of Concrete.numeral
  | Assert of Concrete.term
  | Check_sat
  | Get_assertions
  | Get_proof
  | Get_unsat_core
  | Get_value of Concrete.term list
  | Get_assignment
  | Get_option of Concrete.keyword
  | Get_info of info_flag
  | Exit

type script = command list
