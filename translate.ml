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

let translate_sort_symbol ident =
  Dk.var ("S"^(translate_identifier ident))

let translate_fun_symbol fun_sym =
  match fun_sym with
  | Abs.Identifier_fun ident ->
     Dk.var ("T"^(translate_identifier ident))
  | _ -> raise Error.Not_implemented

let translate_sort_par par =
  translate_sort_symbol (par, [])

let translate_variable var =
  ("T"^(translate_identifier (var, [])))

let rec translate_sort sort =
  match sort with
  | Abs.Sort (("Bool", []), []) ->
     Dk.bool
  | Abs.Sort (sort_sym, sorts) -> 
     Dk.app (translate_sort_symbol sort_sym) (List.map translate_sort sorts)

let rec translate_par_sort par_sort =
  match par_sort with
  | Abs.Par par ->
     translate_sort_par par
  | Abs.Par_sort (sort_sym, par_sorts) -> 
     Dk.app (translate_sort_symbol sort_sym) (List.map translate_par_sort par_sorts)

let rec translate_term signature term =
  match term with
  | Abs.Var var -> Dk.var (translate_variable var)
  | Abs.App (fun_sym, _, terms) ->
     begin
       match Signature.find_fun_data fun_sym signature with
       | Signature.Theory_fun_declaration [_, _, _, attributes] ->
	  Dk.app (translate_fun_symbol fun_sym) (List.map (translate_term signature) terms)
       | _ ->
	  Dk.app (translate_fun_symbol fun_sym) (List.map (translate_term signature) terms) end
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
	  Dk.lam (translate_variable var) (translate_sort sort) dk_term) 
	 (translate_term new_signature term) (List.rev sorted_vars) in 
     Dk.app
       dk_term 
       (List.map (fun (_, term) -> translate_term signature term) bindings)
  | Abs.Forall (sorted_vars, term)
  | Abs.Exists (sorted_vars, term) ->
     raise Translate_error 
  | Abs.Attributed (term, _) -> 
     translate_term signature term

(* Dedukti lines *)

let translate_prelude file = 
  let name =
    translate_string (Filename.chop_extension (Filename.basename file)) in
  Dk.prelude name

let rec translate_sort_declaration sort_sym n =
  let rec dk_sort i =
    match i with
    | 0 -> Dk.sort
    | _ -> Dk.arrow Dk.sort (dk_sort (i-1)) in
  Dk.declaration (translate_sort_symbol sort_sym) (dk_sort n)
		 
let translate_sort_definition sort_sym pars par_sort =
  let dk_par_sort = translate_par_sort par_sort in
  let dk_sort, dk_term = 
    List.fold_left 
      (fun (sort, term) par -> Dk.arrow Dk.sort sort, Dk.lam par Dk.sort term) 
      (Dk.sort, dk_par_sort) (List.rev pars) in
  Dk.definition (translate_sort_symbol sort_sym) dk_sort dk_term

let translate_fun_declaration fun_sym sorts sort =
  Dk.declaration 
    (translate_fun_symbol fun_sym) 
    (List.fold_left 
       (fun dk_sort sort -> (Dk.arrow (Dk.term (translate_sort sort)) dk_sort)) 
       (Dk.term (translate_sort sort)) (List.rev sorts))
	
let translate_fun_definition signature fun_sym sorted_vars sort term =
  let dk_sort, new_signature =
    List.fold_left 
      (fun (dk_sort, signature) (var, sort) -> 
       Dk.arrow (translate_sort sort) dk_sort, 
       Signature.add_var var sort signature)
      (translate_sort sort, signature) (List.rev sorted_vars) in
  let dk_term = translate_term signature term in
  Dk.definition (translate_fun_symbol fun_sym) dk_sort dk_term

let translate_sort_context signature =
  let lines =
    Signature.fold_sorts
      (fun sort_sym sort_data lines ->
       match sort_data with
       | Signature.Theory_sort_declaration _ -> lines
       | Signature.User_sort_declaration n ->
	  translate_sort_declaration sort_sym n :: lines
       | Signature.Sort_definition (pars, par_sort) ->
	  translate_sort_definition sort_sym pars par_sort :: lines) signature [] in
  List.rev lines

let translate_fun_context signature =
  let lines =
    Signature.fold_funs
      (fun fun_sym fun_data lines ->
       match fun_data with
       | Signature.Theory_fun_declaration _ -> lines
       | Signature.User_fun_declaration (sorts, sort) ->
	  translate_fun_declaration fun_sym sorts sort :: lines
       | Signature.Fun_definition (sorted_vars, sort, term) ->
	  translate_fun_definition signature fun_sym sorted_vars sort term :: lines) signature [] in
  List.rev lines
