type ident = string

type var = string

type const = 
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
  | Sort
  | Term
  | Bool

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
  | _, _ -> App (t :: ts)
let app2 t1 t2 = app t1 [t2]
let app3 t1 t2 t3 = app t1 [t2; t3]
let arrow t1 t2 = Arrow (t1, t2)

let l_term = Const Lterm
let l_prop = Const Lprop
let l_true = Const Ltrue
let l_false = Const Lfalse
let l_not p = app2 (Const Lnot) p
let l_imply p q = app3 (Const Limply) p q
let l_and p q = app3 (Const Land) p q
let l_or p q = app3 (Const Lor) p q
let l_eq t1 t2 = app3 (Const Leq) t1 t2
let l_prf t = app2 (Const Lprf) t
let sort = Const Sort
let term t = app2 (Const Term) t
let bool = Const Bool

let declaration t term = Declaration (t, term)
let definition t termtype term = Definition (t, termtype, term)
let prelude name = Prelude (name)
