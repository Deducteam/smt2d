(* Dedukti variable *)

type ident = string

type var = string

type const = private
  | Lsort
  | Lterm
  | Lbool
  | Ltrue
  | Lfalse
  | Lnot
  | Limply
  | Land
  | Lor
  | Lxor
  | Leq
  | Lneq
  | Lite

(* Dedukti term *)
type term = private
  | Var of var
  | Lam of var * term * term
  | App of term list                (* at least two arguments, the first is not an App *)
  | Arrow of term * term
  | Const of const

(* Dedukti line (e.g. definition) *)
type line = private
  | Declaration of term * term
  | Definition of term * term * term
  | Prelude of string

(* Building Dedukti terms *)
val var : var -> term
val lam : var -> term -> term -> term
val lams : var list -> term list -> term -> term
val app : term -> term list -> term
val app2 : term -> term -> term
val app3 : term -> term -> term -> term
val arrow : term -> term -> term

(* Building Dedukti terms in the logic.dk context *)
val l_sort : term
val l_term : term -> term
val l_bool : term
val l_true : term
val l_false : term
val l_not : term -> term
val l_imply : term -> term -> term
val l_and : term -> term -> term
val l_or : term -> term -> term
val l_xor : term -> term -> term
val l_eq : term -> term -> term -> term
val l_neq : term -> term -> term -> term
val l_ite : term -> term -> term -> term -> term

(* Building Dedukti lines *)
val declaration : term -> term -> line
val definition : term -> term -> term -> line
val prelude : string -> line

(* Print Dedukti terms *)
val print_var : out_channel -> var -> unit
val print_term : out_channel -> term -> unit
val print_terms : out_channel -> term list -> unit
val print_line : out_channel -> line -> unit
