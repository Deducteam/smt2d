(* Translate smtlib2 to dedukti *)

module Abs = Abstract
module Dk = Dedukti

exception Translate_error

(* any special caracter c becomes 
     "_"^(string_of_int (int_of_char c))^"_"
   and "_"^(string_of_char c) with c in ['0' - '9'] becomes "_0"^(string_of_char c) *)
let translate_string s = 
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
		  
let translate_identifier (sym, nums) =
  match nums with
  | [] -> 
     "_" ^ translate_string sym
  | _ -> 
     let strings = 
       string_of_int (List.length nums) :: (translate_string sym) :: nums in
     String.concat "_" strings

let translate_theory_sort_symbol sort_sym =
  match sort_sym with
  | "Bool", [] -> Dk.l_bool
  | _ -> raise Error.Not_implemented

let translate_sort_symbol signature sort_sym =
  match Signature.find_sort_data sort_sym signature with
  | Signature.Theory_sort_declaration _ ->
     translate_theory_sort_symbol sort_sym
  | Signature.User_sort_declaration _ | Signature.Sort_definition _ -> 
     Dk.var ("S"^(translate_identifier sort_sym))

let translate_user_fun_symbol fun_sym =
  match fun_sym with
  | Abs.Identifier_fun ident ->
     Dk.var ("T"^(translate_identifier ident))
  | Abs.Spec_constant_fun _ -> raise Error.Not_implemented

let translate_sort_par par =
  "S"^(translate_identifier (par, []))

let translate_variable var =
  "T"^(translate_identifier (var, []))

let rec translate_sort signature sort =
  match sort with
  | Abs.Sort (sort_sym, sorts) -> 
     Dk.app (translate_sort_symbol signature sort_sym) (List.map (translate_sort signature) sorts)

let rec translate_par_sort signature par_sort =
  match par_sort with
  | Abs.Par par ->
     Dk.var (translate_sort_par par)
  | Abs.Par_sort (sort_sym, par_sorts) -> 
     Dk.app 
       (translate_sort_symbol signature sort_sym) 
       (List.map (translate_par_sort signature) par_sorts)

let translate_theory_app fun_sym sorts terms = 
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
  
let rec translate_simplified_term signature term =
  match term with
  | Abs.Var var -> Dk.var (translate_variable var)
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
	  translate_theory_app 
	    fun_sym (List.map (translate_sort signature) sorts) 
	    (List.map (translate_simplified_term signature) terms)
       | Signature.Theory_fun_declaration _ -> 
	  raise Error.Not_implemented
       | Signature.User_fun_declaration _ 
       | Signature.Fun_definition _ ->
       	  Dk.app (translate_user_fun_symbol fun_sym)
       		 (List.map (translate_simplified_term signature) terms) end
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
	  Dk.lam (translate_variable var) (Dk.l_term (translate_sort signature sort)) dk_term) 
	 (translate_simplified_term new_signature term) (List.rev sorted_vars) in 
     Dk.app
       dk_term 
       (List.map (fun (_, term) -> translate_simplified_term signature term) bindings)
  | Abs.Forall (_, _)
  | Abs.Exists (_, _) ->
     raise Translate_error 
  | Abs.Attributed (term, _) -> 
     translate_simplified_term signature term

let translate_term signature term =
  let s_term = Simplify.simplify signature term in
  translate_simplified_term signature s_term

(* Dedukti lines *)

let translate_prelude file = 
  let name =
    translate_string (Filename.chop_extension (Filename.basename file)) in
  Dk.prelude name

let translate_user_sort_declaration signature sort_sym n =
  let rec dk_sort i =
    match i with
    | 0 -> Dk.l_sort
    | _ -> Dk.arrow Dk.l_sort (dk_sort (i-1)) in
  Dk.declaration (translate_sort_symbol signature sort_sym) (dk_sort n)
		 
let translate_sort_definition signature sort_sym pars par_sort =
  let dk_par_sort = translate_par_sort signature par_sort in
  let dk_sort, dk_term = 
    List.fold_left 
      (fun (sort, term) par -> Dk.arrow Dk.l_sort sort, Dk.lam par Dk.l_sort term) 
      (Dk.l_sort, dk_par_sort) (List.rev pars) in
  Dk.definition (translate_sort_symbol signature sort_sym) dk_sort dk_term

let translate_user_fun_declaration signature fun_sym sorts sort =
  Dk.declaration 
    (translate_user_fun_symbol fun_sym) 
    (List.fold_left 
       (fun dk_sort sort -> (Dk.arrow (Dk.l_term (translate_sort signature sort)) dk_sort)) 
       (Dk.l_term (translate_sort signature sort)) (List.rev sorts))
	
let translate_fun_definition signature fun_sym sorted_vars sort term =
  let dk_sort, new_signature =
    List.fold_left 
      (fun (dk_sort, signature) (var, sort) -> 
       Dk.arrow (Dk.l_term (translate_sort signature sort)) dk_sort, 
       Signature.add_var var sort signature)
      (Dk.l_term (translate_sort signature sort), signature) (List.rev sorted_vars) in
  let dk_term = translate_term new_signature term in
  Dk.definition (translate_user_fun_symbol fun_sym) dk_sort dk_term

let translate_sort_context signature =
  let lines =
    Signature.fold_sorts
      (fun sort_sym sort_data lines ->
       match sort_data with
       | Signature.Theory_sort_declaration _ -> lines
       | Signature.User_sort_declaration n ->
	  translate_user_sort_declaration signature sort_sym n :: lines
       | Signature.Sort_definition (pars, par_sort) ->
	  translate_sort_definition signature sort_sym pars par_sort :: lines) signature [] in
  List.rev lines

let translate_fun_context signature =
  let lines =
    Signature.fold_funs
      (fun fun_sym fun_data lines ->
       match fun_data with
       | Signature.Theory_fun_declaration _ -> lines
       | Signature.User_fun_declaration (sorts, sort) ->
	  translate_user_fun_declaration signature fun_sym sorts sort :: lines
       | Signature.Fun_definition (sorted_vars, sort, term) ->
	  translate_fun_definition signature fun_sym sorted_vars sort term :: lines) signature [] in
  List.rev lines

let translate_assertions signature assertions =
  List.mapi
    (fun i term -> 
     Dk.definition 
       (Dk.var ("H_"^(string_of_int (i+1)))) 
       (Dk.l_term Dk.l_bool)
       (translate_term signature term)) assertions

let print_context out signature assertions file =
  let prelude = translate_prelude file in
  Print.print_line out prelude;
  let sort_context = translate_sort_context signature in
  List.iter (Print.print_line out) sort_context;
  let fun_context = translate_fun_context signature in
  List.iter (Print.print_line out) fun_context;
  let propositions = translate_assertions signature assertions in
  List.iter (Print.print_line out) propositions
