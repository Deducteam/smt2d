(* Signatures *)

exception Signature_error

type sort_data =
  | Sort_declaration of int
  | Sort_definition of
      Abstract.sort_parameter list * Abstract.parametric_sort

type fun_data =
  | Fun_declaration of
      (Abstract.sort_parameter list *
	 Abstract.parametric_sort list * Abstract.parametric_sort) list
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
    
type signature = {
  sorts: sort_data SortMap.t;
  funs: fun_data FunMap.t;
}

(* Internal functions *)
		   
let empty =
  { sorts = SortMap.empty;
    funs = FunMap.empty;
  }

let overload_fun sym data signature =
  { sorts = signature.sorts;
    funs =
      if FunMap.mem sym signature.funs
      then
	let envdata = FunMap.find sym signature.funs in
	match envdata, data with
	| Fun_declaration l1, Fun_declaration l2 ->
	   FunMap.add sym (Fun_declaration (l1@l2)) signature.funs
	| _, _ -> raise Signature_error
      else FunMap.add sym data signature.funs;
  }

(* Exported functions *)
    
let add_sort sym data signature =
  { sorts =
      if SortMap.mem sym signature.sorts
      then raise Signature_error
      else SortMap.add sym data signature.sorts;
    funs = signature.funs;
  }

let add_fun sym data signature =
  { sorts = signature.sorts;
    funs =
      if FunMap.mem sym signature.funs
      then raise Signature_error
      else FunMap.add sym data signature.funs;
  }

let find_sort sort_sym signature =
  SortMap.find sort_sym signature.sorts

let find_fun fun_sym signature =
  FunMap.find fun_sym signature.funs
    
let logic_signature logic_name =
  let _, theory_names = Logic.logic_declaration logic_name in
  let theory_declarations =
    List.map Logic.theory_declaration theory_names in
  List.fold_left 
    (fun env (_, sort_declarations, par_fun_declarations) -> 
     let newenv = 
       List.fold_left 
	 (fun env (sym, n, _) ->
	  add_sort sym (Sort_declaration n) env) 
	 env sort_declarations in
     List.fold_left 
       (fun env (pars, sym, sorts, sort, _) -> 
        overload_fun
	  sym (Fun_declaration [pars, sorts, sort]) env) 
       newenv par_fun_declarations)
    empty theory_declarations
