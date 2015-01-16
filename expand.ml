(* Removes chainable, left-assoc, right-assoc, and pairwise syntactic sugar *)

module Abs = Abstract

type term = Abstract.term

type sugar_rule =
  | Chainable
  | Left_assoc
  | Right_assoc
  | Pairwise

let rec get_sugar_rule attributes =
  match attributes with
  | (":chainable", None) :: _ -> Some Chainable
  | (":left-assoc", None) :: _ -> Some Left_assoc
  | (":right-assoc", None) :: _ -> Some Right_assoc
  | (":pairwise", None) :: _ -> Some Pairwise
  | _ :: attributes -> get_sugar_rule attributes
  | [] -> None
						   
let rec get_left_assoc fun_sym opt ts =
  match ts with
  | [] -> assert false 
  | [t] -> t
  | t1 :: t2 :: ts -> 
     let t = Abs.t_app fun_sym opt [t1; t2] in
     get_left_assoc fun_sym opt (t :: ts)

let rec get_right_assoc fun_sym opt ts =
  match ts with
  | [] -> assert false
  | [t] -> t
  | t1 :: t2 :: ts -> 
     let t = get_right_assoc fun_sym opt (t2 :: ts) in
     Abs.t_app fun_sym opt [t1; t]

let get_chainable fun_sym opt ts =
  let rec get_chain ts =
    match ts with
    | [] -> assert false
    | [_] -> [] 
    | t1 :: t2 :: ts -> Abs.t_app fun_sym opt [t1; t2] :: get_chain (t2 :: ts) in
  get_left_assoc Abs.and_sym None (get_chain ts)

let rec get_pairwise fun_sym opt ts = 
  let get_pairs ts =
    match ts with 
    | [] -> assert false
    | t :: ts -> 
       List.map (fun x -> Abs.t_app fun_sym opt [t; x]) ts @ [get_pairwise fun_sym opt ts] in
  match ts with
  | [] | [_] -> assert false
  | [t1; t2] -> Abs.t_app fun_sym opt [t1; t2]
  | _ -> get_left_assoc Abs.and_sym None (get_pairs ts)
			
let rec expand signature term =
  match term with
  | Abs.Var _ -> term
  | Abs.App (fun_sym, opt, []) -> Abs.t_app fun_sym opt []
  | Abs.App (fun_sym, opt, [t]) -> Abs.t_app fun_sym opt [expand signature t]
  | Abs.App (fun_sym, opt, [t1; t2]) -> 
     Abs.t_app fun_sym opt [expand signature t1; expand signature t2]
  | Abs.App (fun_sym, opt, terms) ->
     begin
       match Signature.find_fun_data fun_sym signature with
       | Signature.Theory_fun_declaration [_, _, _, attributes] ->
	  let ts = List.map (expand signature) terms in
	  begin
	    match get_sugar_rule attributes with
	    | Some Chainable ->
	       get_chainable fun_sym opt ts
	    | Some Left_assoc ->
	       get_left_assoc fun_sym opt ts
	    | Some Right_assoc ->
	       get_right_assoc fun_sym opt ts
	    | Some Pairwise ->	       
	       get_pairwise fun_sym opt ts
	    | None -> 
	       Abs.t_app fun_sym opt (List.map (expand signature) terms) end
       | Signature.Theory_fun_declaration _ -> raise Error.Not_implemented
       | Signature.User_fun_declaration _ | Signature.Fun_definition _ ->
	  Abs.t_app fun_sym opt (List.map (expand signature) terms) end
  | Abs.Let (bindings, term) -> 
     Abs.t_let 
       (List.map (fun (var, term) -> var, expand signature term) bindings) 
       (expand signature term)
  | Abs.Forall (sorted_vars, term) -> Abs.t_forall sorted_vars (expand signature term)
  | Abs.Exists (sorted_vars, term) -> Abs.t_exists sorted_vars (expand signature term)
  | Abs.Attributed (term, attributes) -> Abs.t_attributed (expand signature term) attributes
