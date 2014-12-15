(* Print dedukti terms *)

open Printf

let print_var out var = fprintf out "%s" var

let rec print_const out const =
  match const with
  | Dedukti.Lterm -> output_string out "logic.Term"
  | Dedukti.Lprop -> output_string out "logic.Prop"
  | Dedukti.Lnot -> output_string out "logic.not"
  | Dedukti.Land -> output_string out "logic.and"
  | Dedukti.Lor -> output_string out "logic.or"
  | Dedukti.Limply -> output_string out "logic.imply"
  | Dedukti.Ltrue -> output_string out "logic.True"
  | Dedukti.Lfalse -> output_string out "logic.False"
  | Dedukti.Leq -> output_string out "logic.equal"
  | Dedukti.Lprf -> output_string out "logic.prf"

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
