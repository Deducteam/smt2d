(* Removes chainable, left-assoc, right-assoc, and pairwise syntactic sugar *)

exception Simplify_error

module Abs = Abstract

type sugar_rule =
  | Chainable
  | Left_assoc
  | Right_assoc
  | Pairwise

let rec get_sugar_rule attributes =
  match attributes with
  | (":chainable", None) :: attributes -> Some Chainable
  | (":left-assoc", None) :: attributes -> Some Left_assoc
  | (":right-assoc", None) :: attributes -> Some Right_assoc
  | (":pairwise", None) :: attributes -> Some Pairwise
  | _ :: attributes -> get_sugar_rule attributes
  | [] -> None

let rec simplify signature term =
  match term with
  | Abs.Var _ -> term
  | Abs.App (fun_sym, opt, []) -> Abs.App (fun_sym, opt, [])
  | Abs.App (fun_sym, opt, [t]) -> Abs.App (fun_sym, opt, [simplify signature t])
  | Abs.App (fun_sym, opt, [t1; t2]) -> 
     Abs.App (fun_sym, opt, [simplify signature t1; simplify signature t2])
  | Abs.App (fun_sym, opt, terms) ->
     begin
       match Signature.find_fun_data fun_sym signature with
       | Signature.Theory_fun_declaration [_, _, _, attributes] ->
	  begin
	    match get_sugar_rule attributes with
	    | Some Chainable ->
	       let rec get_chain l =
		 match l with
		 | [] | [_] -> [] 
		 | t1 :: t2 :: ts -> 
		    Abs.App (fun_sym, opt, [t1; t2]) :: get_chain (t2 :: ts) in
	       simplify 
		 signature 
		 (Abs.App (Abs.Identifier_fun Abs.and_id, None, get_chain terms))
	    | Some Left_assoc ->
	       let ts, t = Util.separate_last terms in
	       Abs.App 
		 (fun_sym, opt, 
		  [simplify signature (Abs.App (fun_sym, opt, ts));
		   simplify signature t])
	    | Some Right_assoc ->
	       let t, ts = List.hd terms, List.tl terms in
	       Abs.App 
		 (fun_sym, opt,
		  [simplify signature t; 
		   simplify signature (Abs.App (fun_sym, opt, ts))])
	    | Some Pairwise ->
	       let rec get_pairs l =
		 match l with
		 | [] | [_] -> [] 
		 | t :: ts -> 
		    List.map (fun term -> Abs.App (fun_sym, opt, [term; t])) ts @
		      [Abs.App (fun_sym, opt, ts)] in
	       simplify
		 signature 
		 (Abs.App (Abs.Identifier_fun Abs.and_id, None, get_pairs terms))
	    | None -> 
	       Abs.App (fun_sym, opt, List.map (simplify signature) terms) end
       | _ -> Abs.App (fun_sym, opt, List.map (simplify signature) terms) end
  | Abs.Let (bindings, term) -> 
     Abs.Let 
       (List.map (fun (var, term) -> var, simplify signature term) bindings, 
	simplify signature term)
  | Abs.Forall (sorted_vars, term) -> Abs.Forall (sorted_vars, simplify signature term)
  | Abs.Exists (sorted_vars, term) -> Abs.Exists (sorted_vars, simplify signature term)
  | Abs.Attributed (term, attributes) -> Abs.Attributed (simplify signature term, attributes)
