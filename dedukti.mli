(* Dedukti variable *)

type ident = string

type var = string

type const = private
  | Lterm
  | Lprop
  | Ltrue
  | Lfalse
  | Lnot
  | Limply
  | Land
  | Lor
  | Leq
  | Lprf

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
val l_term : term
val l_prop : term
val l_true : term
val l_false : term
val l_not : term -> term
val l_imply : term -> term -> term
val l_and : term -> term -> term
val l_or : term -> term -> term
val l_eq : term -> term -> term
val l_prf : term -> term

(* Building Dedukti lines *)
val declaration : term -> term -> line
val definition : term -> term -> term -> line
val prelude : string -> line
