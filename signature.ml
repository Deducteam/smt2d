(* Signatures *)

exception Signature_error

module Abs = Abstract

type sort_data =
  | Theory_sort_declaration of int
  | User_sort_declaration of int
  | Sort_definition of
      Abs.sort_parameter list * Abs.parametric_sort

type fun_data =
  | Theory_fun_declaration of
      (Abs.sort_parameter list *
	 Abs.parametric_sort list * Abs.parametric_sort * Abs.attribute list) list
  | User_fun_declaration of Abs.sort list * Abs.sort
  | Fun_definition of
      (Abs.variable * Abs.sort) list *
	Abs.sort * Abs.term

module SortMap =
  Map.Make
    (struct
      type t = Abs.sort_symbol
      let compare = Pervasives.compare
    end)

module FunMap =
  Map.Make
    (struct
      type t = Abs.fun_symbol
      let compare = Pervasives.compare
    end)

module VarMap =
  Map.Make
    (struct
      type t = Abs.variable
      let compare = Pervasives.compare
    end)
    
type signature = {
  sorts: sort_data SortMap.t;
  funs: fun_data FunMap.t;
  vars: Abs.sort VarMap.t
}

(* Internal functions *)

let overload_fun sym data signature =
  { sorts = signature.sorts;
    funs =
      if FunMap.mem sym signature.funs
      then
	let envdata = FunMap.find sym signature.funs in
	match envdata, data with
	| Theory_fun_declaration l1, Theory_fun_declaration l2 ->
	   FunMap.add sym (Theory_fun_declaration (l1@l2)) signature.funs
	| Theory_fun_declaration _, _
	| User_fun_declaration _ , _
	| Fun_definition _, _ -> raise Signature_error
      else FunMap.add sym data signature.funs;
    vars = signature.vars;
  }

(* Exported functions *)
    
let add_sort sym data signature =
  { sorts =
      if SortMap.mem sym signature.sorts
      then raise Signature_error
      else SortMap.add sym data signature.sorts;
    funs = signature.funs;
    vars = signature.vars;
  }

let add_fun sym data signature =
  { sorts = signature.sorts;
    funs =
      if FunMap.mem sym signature.funs
      then raise Signature_error
      else FunMap.add sym data signature.funs;
    vars = signature.vars;
  }

let add_var var sort signature =
  { sorts = signature.sorts;
    funs = signature.funs;
    vars = VarMap.add var sort signature.vars;
  }

let find_sort_data sort_sym signature =
  SortMap.find sort_sym signature.sorts

let find_fun_data fun_sym signature =
  FunMap.find fun_sym signature.funs

let find_var_sort var signature =
  VarMap.find var signature.vars
    
let fold_sorts f signature b =
  SortMap.fold f signature.sorts b
let fold_funs f signature b =
  FunMap.fold f signature.funs b
let fold_vars f signature b =
  VarMap.fold f signature.vars b
		   
let empty =
  { sorts = SortMap.empty;
    funs = FunMap.empty;
    vars = VarMap.empty
  }

let logic_signature logic_name =
  let _, theory_names = Abs.get_logic_declaration logic_name in
  let theory_declarations =
    List.map Abs.get_theory_declaration theory_names in
  List.fold_left 
    (fun env th_decl ->
     let sort_decls, par_fun_decls = 
       th_decl.Abs.sort_declarations, th_decl.Abs.par_fun_declarations in
     let newenv = 
       List.fold_left 
	 (fun env sort_decl ->
	  add_sort sort_decl.Abs.sort_symbol (Theory_sort_declaration sort_decl.Abs.number) env) 
	 env sort_decls in
     List.fold_left 
       (fun env par_fun_decl -> 
        overload_fun
	  par_fun_decl.Abs.fun_symbol
	  (Theory_fun_declaration 
	     [par_fun_decl.Abs.sort_parameters, 
	      par_fun_decl.Abs.parametric_sorts, 
	      par_fun_decl.Abs.parametric_sort,
	      par_fun_decl.Abs.attributes]) env) 
       newenv par_fun_decls)
    empty theory_declarations
