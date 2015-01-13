(* Print dedukti terms *)

open Printf

let print_var out var = fprintf out "%s" var

let rec print_const out const =
  match const with
  | Dedukti.Lsort -> output_string out "logic.Sort"
  | Dedukti.Lterm -> output_string out "logic.Term"
  | Dedukti.Lbool -> output_string out "logic.Bool"
  | Dedukti.Ltrue -> output_string out "logic.true"
  | Dedukti.Lfalse -> output_string out "logic.false"
  | Dedukti.Lnot -> output_string out "logic.not"
  | Dedukti.Limply -> output_string out "logic.imply"
  | Dedukti.Land -> output_string out "logic.and"
  | Dedukti.Lor -> output_string out "logic.or"
  | Dedukti.Lxor -> output_string out "logic.xor"
  | Dedukti.Leq -> output_string out "logic.equal"
  | Dedukti.Lneq -> output_string out "logic.distinct"
  | Dedukti.Lite -> output_string out "logic.ite"

let rec print_term out term =
  match term with
  | Dedukti.Var (var) -> print_var out var
  | Dedukti.Lam (v, t1, t2) ->
    fprintf out "%a: %a => %a"
      print_var v print_term_p t1 print_term_p t2
  | Dedukti.App (ts) -> print_terms out ts
  | Dedukti.Arrow (t1, t2) ->
    fprintf out "%a -> %a"
      print_term_p t1 print_term_p t2
  | Dedukti.Const c -> print_const out c

and print_term_p out term = 
  match term with
  | Dedukti.Lam _ | Dedukti.App _ | Dedukti.Arrow _ ->
    fprintf out "(%a)" print_term term
  | _ -> print_term out term

and print_terms out terms = 
  match terms with
  | [] -> ()
  | [t] -> print_term_p out t
  | t :: q -> 
    fprintf out "%a %a"
      print_term_p t print_terms q

let print_line out line =
  match line with
  | Dedukti.Declaration (t, term) -> 
    fprintf out "%a: %a.\n" 
      print_term t
      print_term term
  | Dedukti.Definition (t, typeterm, term) ->
    fprintf out "%a: %a:= %a.\n"
      print_term t
      print_term typeterm
      print_term term
  | Dedukti.Prelude (name) -> fprintf out "#NAME %s.\n" name;
