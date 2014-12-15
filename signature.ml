(* Signatures *)

exception Signature_error

type sort_data =
  | Theory_sort_declaration of int
  | User_sort_declaration of int
  | Sort_definition of
      Abstract.sort_parameter list * Abstract.parametric_sort

type fun_data =
  | Theory_fun_declaration of
      (Abstract.sort_parameter list *
	 Abstract.parametric_sort list * Abstract.parametric_sort * Abstract.attribute list) list
  | User_fun_declaration of Abstract.sort list * Abstract.sort
  | Fun_definition of
      (Abstract.variable * Abstract.sort) list *
	Abstract.sort * Abstract.term

module SortMap =
  Map.Make
    (struct
      type t = Abstract.sort_symbol
      let compare = Pervasives.compare
    end)

module FunMap =
  Map.Make
    (struct
      type t = Abstract.fun_symbol
      let compare = Pervasives.compare
    end)

module VarMap =
  Map.Make
    (struct
      type t = Abstract.variable
      let compare = Pervasives.compare
    end)
    
type signature = {
  sorts: sort_data SortMap.t;
  funs: fun_data FunMap.t;
  vars: Abstract.sort VarMap.t
}

(* Internal functions *)
		   
let empty =
  { sorts = SortMap.empty;
    funs = FunMap.empty;
    vars = VarMap.empty
  }

let overload_fun sym data signature =
  { sorts = signature.sorts;
    funs =
      if FunMap.mem sym signature.funs
      then
	let envdata = FunMap.find sym signature.funs in
	match envdata, data with
	| Theory_fun_declaration l1, Theory_fun_declaration l2 ->
	   FunMap.add sym (Theory_fun_declaration (l1@l2)) signature.funs
	| _, _ -> raise Signature_error
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

let logic_signature logic_name =
  let _, theory_names = Logic.logic_declaration logic_name in
  let theory_declarations =
    List.map Logic.theory_declaration theory_names in
  List.fold_left 
    (fun env (_, sort_declarations, par_fun_declarations) -> 
     let newenv = 
       List.fold_left 
	 (fun env (sym, n, _) ->
	  add_sort sym (Theory_sort_declaration n) env) 
	 env sort_declarations in
     List.fold_left 
       (fun env (pars, sym, sorts, sort, attributes) -> 
        overload_fun
	  sym (Theory_fun_declaration [pars, sorts, sort, attributes]) env) 
       newenv par_fun_declarations)
    empty theory_declarations
