(* Translate smtlib2 to dedukti *)

module Abs = Abstract
module Dk = Dedukti

exception Translate_error

(* any special caracter c becomes 
     "_"^(string_of_int (int_of_char c))^"_"
   and "_"^(string_of_char c) with c in ['0' - '9'] becomes "_0"^(string_of_char c) *)
let tr_string s = 
  let buf = Buffer.create (2*String.length s) in
  let escape = ref false in
  String.iter
    (fun c ->
     match c with
     | 'a'..'z' | 'A'..'Z' -> Buffer.add_char buf c
     | '0'..'9' ->
	if !escape
	then 
	  (Buffer.add_char buf '0'; Buffer.add_char buf c; escape := false) 
	else 
	  Buffer.add_char buf c
     | '_' -> 
	escape := true; Buffer.add_char buf c
     | _ -> 
	Buffer.add_string 
	  buf ("_"^(string_of_int (int_of_char c))^"_")) s;
  Buffer.contents buf

let tr_identifier (sym, nums) =
  match nums with
  | [] -> 
     "_" ^ tr_string sym
  | _ -> 
     let strings = 
       string_of_int (List.length nums) :: (tr_string sym) :: nums in
     String.concat "_" strings

let tr_sort_symbol sort_sym =
  Dk.var ("S"^(tr_identifier sort_sym))

let tr_user_fun_symbol fun_sym =
  match fun_sym with
  | Abs.Identifier_fun ident ->
     Dk.var ("T"^(tr_identifier ident))
  | Abs.Spec_constant_fun _ -> raise Error.Not_implemented

let tr_sort_par par =
  "S"^(tr_identifier (par, []))

let tr_variable var =
  "T"^(tr_identifier (var, []))

let rec tr_sort sort =
  match sort with
  | Abs.Sort (sort_sym, sorts) -> 
     Dk.app (tr_sort_symbol sort_sym) (List.map tr_sort sorts)
  | Abs.Bool -> Dk.l_bool

let rec tr_par_sort par_sort =
  match par_sort with
  | Abs.Param par ->
     Dk.var (tr_sort_par par)
  | Abs.Par_sort (sort_sym, par_sorts) -> 
     Dk.app 
       (tr_sort_symbol sort_sym) 
       (List.map tr_par_sort par_sorts)
  | Abs.Par_bool -> Dk.l_bool

let rec tr_core signature core =
  match core with
  | Abstract.True -> Dk.l_true
  | Abstract.False -> Dk.l_false
  | Abstract.Not t -> Dk.l_not (tr_term signature t)
  | Abstract.Imply (t1, t2) -> Dk.l_imply (tr_term signature t1) (tr_term signature t2)
  | Abstract.And (t1, t2) -> Dk.l_and (tr_term signature t1) (tr_term signature t2)
  | Abstract.Or (t1, t2) -> Dk.l_or (tr_term signature t1) (tr_term signature t2)
  | Abstract.Xor (t1, t2) -> Dk.l_xor (tr_term signature t1) (tr_term signature t2)
  | Abstract.Equal (t1, t2) -> 
     Dk.l_equal 
       (tr_sort (Get_sort.get_sort signature t1)) 
       (tr_term signature t1) (tr_term signature t2)
  | Abstract.Distinct (t1, t2) -> 
     Dk.l_distinct 
       (tr_sort (Get_sort.get_sort signature t1)) 
       (tr_term signature t1) 
       (tr_term signature t2)
  | Abstract.Ite (t1, t2, t3) -> 
     Dk.l_ite 
       (tr_sort (Get_sort.get_sort signature t1)) 
       (tr_term signature t1) 
       (tr_term signature t2) 
       (tr_term signature t3) 

and tr_term signature term =
  match term with
  | Abs.Var var -> Dk.var (tr_variable var)
  | Abs.App (fun_sym, _, terms) ->
     begin
       match Signature.find_fun_data fun_sym signature with
       | Signature.Fun_declaration _ 
       | Signature.Fun_definition _ ->
       	  Dk.app (tr_user_fun_symbol fun_sym)
       		 (List.map (tr_term signature) terms) end
  | Abs.Core core -> 
     tr_core signature core
  | Abs.Let (bindings, term) ->
     let sorted_vars = 
       List.map 
	 (fun (var, term) ->
	 var, Get_sort.get_sort signature term) bindings in
     let new_signature =
       List.fold_left 
	 (fun signature (var, sort) ->
	  Signature.add_var var sort signature) signature sorted_vars in
     let dk_term =
       List.fold_left 
	 (fun dk_term (var, sort) ->
	  Dk.lam (tr_variable var) (Dk.l_term (tr_sort sort)) dk_term) 
	 (tr_term new_signature term) (List.rev sorted_vars) in 
     Dk.app
       dk_term 
       (List.map (fun (_, term) -> tr_term signature term) bindings)
  | Abs.Forall (_, _)
  | Abs.Exists (_, _) ->
     raise Translate_error 
  | Abs.Attributed (term, _) -> 
     tr_term signature term

(* Dedukti lines *)

let tr_user_sort_declaration sort_sym n =
  let rec dk_sort i =
    match i with
    | 0 -> Dk.l_sort
    | _ -> Dk.arrow Dk.l_sort (dk_sort (i-1)) in
  Dk.declaration (tr_sort_symbol sort_sym) (dk_sort n)

let tr_sort_definition sort_sym pars par_sort =
  let dk_par_sort = tr_par_sort par_sort in
  let dk_sort, dk_term = 
    List.fold_left 
      (fun (sort, term) par -> Dk.arrow Dk.l_sort sort, Dk.lam par Dk.l_sort term) 
      (Dk.l_sort, dk_par_sort) (List.rev pars) in
  Dk.definition (tr_sort_symbol sort_sym) dk_sort dk_term

let tr_user_fun_declaration fun_sym sorts sort =
  Dk.declaration 
    (tr_user_fun_symbol fun_sym) 
    (List.fold_left 
       (fun dk_sort sort -> (Dk.arrow (Dk.l_term (tr_sort sort)) dk_sort)) 
       (Dk.l_term (tr_sort sort)) (List.rev sorts))

let tr_fun_definition signature fun_sym sorted_vars sort term =
  let dk_sort, new_signature =
    List.fold_left 
      (fun (dk_sort, signature) (var, sort) -> 
       Dk.arrow (Dk.l_term (tr_sort sort)) dk_sort, 
       Signature.add_var var sort signature)
      (Dk.l_term (tr_sort sort), signature) (List.rev sorted_vars) in
  let dk_term = tr_term new_signature term in
  Dk.definition (tr_user_fun_symbol fun_sym) dk_sort dk_term

let tr_sort_context signature =
  let lines =
    Signature.fold_sorts
      (fun sort_sym sort_data lines ->
       match sort_data with
       | Signature.Sort_declaration n ->
	  tr_user_sort_declaration sort_sym n :: lines
       | Signature.Sort_definition (pars, par_sort) ->
	  tr_sort_definition sort_sym pars par_sort :: lines) signature [] in
  List.rev lines

let tr_fun_context signature =
  let lines =
    Signature.fold_funs
      (fun fun_sym fun_data lines ->
       match fun_data with
       | Signature.Fun_declaration (sorts, sort) ->
	  tr_user_fun_declaration fun_sym sorts sort :: lines
       | Signature.Fun_definition (sorted_vars, sort, term) ->
	  tr_fun_definition signature fun_sym sorted_vars sort term :: lines) signature [] in
  List.rev lines

let tr_assertion_bindings signature assertion_bindings =
  List.map
    (fun (term, var) -> 
     Dk.definition var (Dk.l_term Dk.l_bool)
       (tr_term signature term)) assertion_bindings
