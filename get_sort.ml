(* find sorts of terms *)

exception Sort_error

exception No_match

module Abs = Abstract

let rec get_par_bindings bindings par_sort_assocs =
  let rec get_result bindings =
    match bindings with
    | [] -> []
    | ((par, None) :: bindings) -> raise No_match
    | ((par, Some sort) :: bindings) -> (par, sort) :: get_result bindings in
  let rec get_par_bindings_aux bindings par_sort sort =
    match par_sort, sort with
    | Abs.Par par, _ ->
       begin try
	   match List.assoc par bindings with
	   | None -> (par, Some sort) :: List.remove_assoc par bindings
	   | Some old_sort -> 
	      if old_sort = sort 
	      then bindings
	      else raise No_match
	 with Not_found -> raise Sort_error end
    | Abs.Par_sort (par_sort_sym, par_sorts), Abs.Sort (sort_sym, sorts) ->
       if par_sort_sym = sort_sym && (List.length par_sorts = List.length sorts)
       then 
	 List.fold_left2 get_par_bindings_aux bindings par_sorts sorts
       else raise No_match in
  match par_sort_assocs with
  | [] -> get_result bindings
  | (par_sort, sort) :: par_sort_assocs ->  
     let new_bindings = get_par_bindings_aux bindings par_sort sort in
     get_par_bindings new_bindings par_sort_assocs

let rec get_rank ranks sorts =
  match ranks with
  | [] -> raise Sort_error
  | (pars, par_sorts, par_sort) :: ranks ->
     if List.length par_sorts != List.length sorts 
     then get_rank ranks sorts
     else
       let opt_bindings = List.map (fun par -> (par, None)) pars in
       let par_sort_assocs = List.combine par_sorts sorts in
	 try 
	   let bindings = get_par_bindings opt_bindings par_sort_assocs in
	   Abstract.substitute_par_sort bindings par_sort 
	 with No_match -> get_rank ranks sorts

let rec get_sort signature term =
  match term with
  | Abs.Var var -> 
     Signature.find_var_sort var signature 
  | Abs.App (fun_sym, Some sort, terms) ->
     sort
  | Abs.App (fun_sym, None, terms) ->
     begin
       match Signature.find_fun_data fun_sym signature with
       | Signature.Theory_fun_declaration ranks ->
	  get_rank ranks (List.map (get_sort signature) terms)
       | Signature.User_fun_declaration (_, sort) -> 
	  sort
       | Signature.Fun_definition (_, sort, _) -> 
	  sort end
  | Abs.Let (bindings, term) ->
     let newsignature =
       List.fold_left 
	 (fun signature (var, term) -> 
	  let sort = get_sort signature term in
	  Signature.add_var var sort signature)
	 signature bindings in
     get_sort newsignature term
  | Abs.Forall (sorted_vars, term)
  | Abs.Exists (sorted_vars, term) ->
     let newsignature =
       List.fold_left 
	 (fun signature (var, sort) -> 
	  Signature.add_var var sort signature)
	 signature sorted_vars in
     get_sort newsignature term
  | Abs.Attributed (term, _) -> 
     get_sort signature term
