(* Dedukti ast and printing functions *)

open Printf

(* Dedukti identifiers *)
type ident = string

(* Dedukti constants *)
type const =
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
type term =
  | Var of ident
  | Lam of ident * term * term
  | App of term list                (* at least two arguments, the first is not an App *)
  | Arrow of term * term
  | Const of const

(* Dedukti line (e.g. definition) *)
type line =
  | Declaration of term * term
  | Definition of term * term * term
  | Prelude of string

(* Building Dedukti terms *)
let var ident = Var ident
let lam ident t term = Lam (ident, t, term)
let lams idents types e = 
  List.fold_left2 (fun term ident t -> lam ident t term) e (List.rev idents) (List.rev types)
let app t ts = 
  match t, ts with
  | _, [] -> t
  | App (us), _ -> App (us @ ts)
  | Var _ , _ -> App (t :: ts)
  | Lam _ , _ -> App (t :: ts)
  | Arrow _ , _ -> App (t :: ts)
  | Const _ , _ -> App (t :: ts)
let app2 t1 t2 = app t1 [t2]
let app3 t1 t2 t3 = app t1 [t2; t3]
let arrow t1 t2 = Arrow (t1, t2)

(* Building Dedukti terms in the logic.dk context *)
let l_sort = Const Lsort
let l_term t = app2 (Const Lterm) t
let l_bool = Const Lbool
let l_true = Const Ltrue
let l_false = Const Lfalse
let l_not p = app2 (Const Lnot) p
let l_imply p q = app3 (Const Limply) p q
let l_and p q = app3 (Const Land) p q
let l_or p q = app3 (Const Lor) p q
let l_xor p q = app3 (Const Lxor) p q
let l_eq s t1 t2 = app (Const Leq) [s; t1; t2]
let l_neq s t1 t2 = app (Const Lneq) [s; t1; t2]
let l_ite s b t1 t2 = app (Const Lite) [s; b; t1; t2]

let declaration t term = Declaration (t, term)
let definition t termtype term = Definition (t, termtype, term)
let prelude name = Prelude (name)

(* Print dedukti terms *)
let print_ident out ident = fprintf out "%s" ident

let print_const out const =
  match const with
  | Lsort -> output_string out "logic.Sort"
  | Lterm -> output_string out "logic.Term"
  | Lbool -> output_string out "logic.Bool"
  | Ltrue -> output_string out "logic.true"
  | Lfalse -> output_string out "logic.false"
  | Lnot -> output_string out "logic.not"
  | Limply -> output_string out "logic.imply"
  | Land -> output_string out "logic.and"
  | Lor -> output_string out "logic.or"
  | Lxor -> output_string out "logic.xor"
  | Leq -> output_string out "logic.equal"
  | Lneq -> output_string out "logic.distinct"
  | Lite -> output_string out "logic.ite"

let rec print_term out term =
  match term with
  | Var (ident) -> print_ident out ident
  | Lam (v, t1, t2) ->
    fprintf out "%a: %a => %a"
      print_ident v print_term_p t1 print_term_p t2
  | App (ts) -> print_terms out ts
  | Arrow (t1, t2) ->
    fprintf out "%a -> %a"
      print_term_p t1 print_term_p t2
  | Const c -> print_const out c

and print_term_p out term = 
  match term with
  | Lam _ | App _ | Arrow _ ->
    fprintf out "(%a)" print_term term
  | Var _ | Const _ -> print_term out term

and print_terms out terms = 
  match terms with
  | [] -> ()
  | [t] -> print_term_p out t
  | t :: q -> 
    fprintf out "%a %a"
      print_term_p t print_terms q

let print_line out line =
  match line with
  | Declaration (t, term) -> 
    fprintf out "%a: %a.\n" 
      print_term t
      print_term term
  | Definition (t, typeterm, term) ->
    fprintf out "%a: %a:= %a.\n"
      print_term t
      print_term typeterm
      print_term term
  | Prelude (name) -> fprintf out "#NAME %s.\n" name;
