(* compute sorts of terms *)

module Abs = Abstract

let rec get_core_sort signature core =
  match core with
  | Abs.True -> Abs.bool
  | Abs.False -> Abs.bool
  | Abs.Not _ -> Abs.bool
  | Abs.Imply _-> Abs.bool
  | Abs.And _ -> Abs.bool
  | Abs.Or _ -> Abs.bool
  | Abs.Xor _ -> Abs.bool
  | Abs.Equal _ -> Abs.bool
  | Abs.Distinct _ -> Abs.bool
  | Abs.Ite (_, t, _) -> get_sort signature t

and get_sort signature term =
  match term with
  | Abs.Var var -> 
     Signature.find_var_sort var signature 
  | Abs.App (_, Some sort, _) ->
     sort
  | Abs.App (fun_sym, None, _) ->
     begin
       match Signature.find_fun_data fun_sym signature with
       | Signature.Fun_declaration (_, sort) -> 
	  sort
       | Signature.Fun_definition (_, sort, _) -> 
	  sort end
  | Abs.Core core ->
     get_core_sort signature core
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
