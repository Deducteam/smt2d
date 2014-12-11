(* remove syntaxic sugar of binary function symbol annoted with
   :chainable, :left-assoc, :right-assoc, and :pairwise *)

module Abs = Abstract

let rec unfold signature term =
  match term with
  | Abs.Var _ -> term
  | Abs.App (fun_sym, opt, terms) ->
     begin
       match Signature.find_fun fun_sym signature with
       | Signature.Fun_declaration (ranks) ->
	  
	  raise Error.Not_implemented
       | Signature.Fun_definition _ ->
	  Abs.App
	    (fun_sym, opt, List.map (unfold signature) terms) end
  | Abs.Let (bindings, term) ->
     Abs.Let (bindings, unfold signature term)
  | Abs.Forall (sorted_vars, term) ->
     Abs.Forall (sorted_vars, unfold signature term)
  | Abs.Exists (sorted_vars, term) ->
     Abs.Exists (sorted_vars, unfold signature term)
  | Abs.Attributed (term, attributes) ->
     Abs.Attributed (unfold signature term, attributes)
