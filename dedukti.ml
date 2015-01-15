type ident = string

type var = string

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

type term = 
  | Var of var
  | Lam of var * term * term
  | App of term list                (* at least two arguments, the first is not an App *)
  | Arrow of term * term
  | Const of const

type line =
  | Declaration of term * term
  | Definition of term * term * term
  | Prelude of string

let var var = Var var
let lam var t term = Lam (var, t, term)
let lams vars types e = 
  List.fold_left2 (fun term var t -> lam var t term) e (List.rev vars) (List.rev types)
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
