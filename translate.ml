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

let tr_theory_sort_symbol sort_sym =
  match sort_sym with
  | "Bool", [] -> Dk.l_bool
  | _ -> raise Error.Not_implemented

let tr_sort_symbol signature sort_sym =
  match Signature.find_sort_data sort_sym signature with
  | Signature.Theory_sort_declaration _ ->
     tr_theory_sort_symbol sort_sym
  | Signature.User_sort_declaration _ | Signature.Sort_definition _ -> 
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

let rec tr_sort signature sort =
  match sort with
  | Abs.Sort (sort_sym, sorts) -> 
     Dk.app (tr_sort_symbol signature sort_sym) (List.map (tr_sort signature) sorts)

let rec tr_par_sort signature par_sort =
  match par_sort with
  | Abs.Param par ->
     Dk.var (tr_sort_par par)
  | Abs.Par_sort (sort_sym, par_sorts) -> 
     Dk.app 
       (tr_sort_symbol signature sort_sym) 
       (List.map (tr_par_sort signature) par_sorts)

let tr_theory_app fun_sym sorts terms = 
  match sorts, terms with
  | [], [] when (fun_sym = Abs.true_sym) -> Dk.l_true
  | [], [] when (fun_sym = Abs.false_sym) -> Dk.l_false
  | [], [t] when (fun_sym = Abs.not_sym) -> Dk.l_not t
  | [], [t1; t2] when (fun_sym = Abs.imply_sym) -> Dk.l_imply t1 t2
  | [], [t1; t2] when (fun_sym = Abs.and_sym) -> Dk.l_and t1 t2
  | [], [t1; t2] when (fun_sym = Abs.or_sym) -> Dk.l_or t1 t2
  | [], [t1; t2] when (fun_sym = Abs.xor_sym) -> Dk.l_xor t1 t2
  | [s], [t1; t2] when (fun_sym = Abs.equal_sym) -> Dk.l_eq s t1 t2
  | [s], [t1; t2] when (fun_sym = Abs.distinct_sym) -> Dk.l_neq s t1 t2
  | [s], [b; t1; t2] when (fun_sym = Abs.ite_sym) -> Dk.l_ite s b t1 t2
  | _ -> raise Error.Not_implemented

let rec tr_term_aux signature term =
  match (term :> Abs.term) with
  | Abs.Var var -> Dk.var (tr_variable var)
  | Abs.App (fun_sym, _, terms) ->
     begin
       match Signature.find_fun_data fun_sym signature with
       | Signature.Theory_fun_declaration [pars, par_sorts, _, _] ->
	  let sorts = 
	    List.map 
	      (fun par -> 
	       Get_sort.get_par_sort 
		 signature par par_sorts (List.map (Get_sort.get_sort signature) terms)) 
	      pars in
	  tr_theory_app 
	    fun_sym (List.map (tr_sort signature) sorts) 
	    (List.map (tr_term_aux signature) terms)
       | Signature.Theory_fun_declaration _ -> 
	  raise Error.Not_implemented
       | Signature.User_fun_declaration _ 
       | Signature.Fun_definition _ ->
       	  Dk.app (tr_user_fun_symbol fun_sym)
       		 (List.map (tr_term_aux signature) terms) end
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
	  Dk.lam (tr_variable var) (Dk.l_term (tr_sort signature sort)) dk_term) 
	 (tr_term_aux new_signature term) (List.rev sorted_vars) in 
     Dk.app
       dk_term 
       (List.map (fun (_, term) -> tr_term_aux signature term) bindings)
  | Abs.Forall (_, _)
  | Abs.Exists (_, _) ->
     raise Translate_error 
  | Abs.Attributed (term, _) -> 
     tr_term_aux signature term

let tr_term signature term =
  tr_term_aux signature (term : Expand.term :> Abs.term)

(* Dedukti lines *)

let tr_user_sort_declaration signature sort_sym n =
  let rec dk_sort i =
    match i with
    | 0 -> Dk.l_sort
    | _ -> Dk.arrow Dk.l_sort (dk_sort (i-1)) in
  Dk.declaration (tr_sort_symbol signature sort_sym) (dk_sort n)

let tr_sort_definition signature sort_sym pars par_sort =
  let dk_par_sort = tr_par_sort signature par_sort in
  let dk_sort, dk_term = 
    List.fold_left 
      (fun (sort, term) par -> Dk.arrow Dk.l_sort sort, Dk.lam par Dk.l_sort term) 
      (Dk.l_sort, dk_par_sort) (List.rev pars) in
  Dk.definition (tr_sort_symbol signature sort_sym) dk_sort dk_term

let tr_user_fun_declaration signature fun_sym sorts sort =
  Dk.declaration 
    (tr_user_fun_symbol fun_sym) 
    (List.fold_left 
       (fun dk_sort sort -> (Dk.arrow (Dk.l_term (tr_sort signature sort)) dk_sort)) 
       (Dk.l_term (tr_sort signature sort)) (List.rev sorts))

let tr_fun_definition signature fun_sym sorted_vars sort term =
  let dk_sort, new_signature =
    List.fold_left 
      (fun (dk_sort, signature) (var, sort) -> 
       Dk.arrow (Dk.l_term (tr_sort signature sort)) dk_sort, 
       Signature.add_var var sort signature)
      (Dk.l_term (tr_sort signature sort), signature) (List.rev sorted_vars) in
  let dk_term = tr_term new_signature (Expand.expand signature term) in
  Dk.definition (tr_user_fun_symbol fun_sym) dk_sort dk_term

let tr_sort_context signature =
  let lines =
    Signature.fold_sorts
      (fun sort_sym sort_data lines ->
       match sort_data with
       | Signature.Theory_sort_declaration _ -> lines
       | Signature.User_sort_declaration n ->
	  tr_user_sort_declaration signature sort_sym n :: lines
       | Signature.Sort_definition (pars, par_sort) ->
	  tr_sort_definition signature sort_sym pars par_sort :: lines) signature [] in
  List.rev lines

let tr_fun_context signature =
  let lines =
    Signature.fold_funs
      (fun fun_sym fun_data lines ->
       match fun_data with
       | Signature.Theory_fun_declaration _ -> lines
       | Signature.User_fun_declaration (sorts, sort) ->
	  tr_user_fun_declaration signature fun_sym sorts sort :: lines
       | Signature.Fun_definition (sorted_vars, sort, term) ->
	  tr_fun_definition signature fun_sym sorted_vars sort term :: lines) signature [] in
  List.rev lines

let tr_assertion_bindings signature assertion_bindings =
  List.map
    (fun (term, var) -> 
     Dk.definition var (Dk.l_term Dk.l_bool)
       (tr_term signature (Expand.expand signature term))) assertion_bindings
