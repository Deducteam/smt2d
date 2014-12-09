(* smtlib2 abstract syntax *)

exception Logic_error
exception Signature_error

type number = int
type sort_symbol = Concrete.identifier
type fun_symbol =
  | Spec_constant_fun of Concrete.spec_constant
  | Identifier_fun of Concrete.identifier
type attribute_name = Concrete.keyword
type theory_name = 
  | Core
type sort_parameter = Concrete.symbol
type variable = Concrete.symbol
type attribute_value = Concrete.attribute_value
type logic_name = 
  | Qf_uf

(* Sorts *)

type sort = 
  | Sort of sort_symbol * sort list

type parametric_sort =
  | Par of sort_parameter
  | Par_sort of sort_symbol * parametric_sort list

(* Terms *)

type attribute = attribute_name * attribute_value option

type term =
  | Var of variable
  | App of fun_symbol * sort option * term list
  | Let of (variable * term) list * term
  | Forall of (variable * sort) list * term
  | Exists of (variable * sort) list * term
  | Attributed of term * attribute list

(* Theories *)

type sort_declaration = sort_symbol * number * attribute list

type par_fun_declaration =
    sort_parameter list * fun_symbol * parametric_sort list * parametric_sort * attribute list

type theory_declaration = theory_name * sort_declaration list * par_fun_declaration list

(* Logics *)

type logic_declaration = logic_name * theory_name list

(* Command options and info names *)

type command_option = attribute

type info_flag = attribute_name
			
(* Commands *)

type command =
  | Set_logic of logic_name
  | Set_option of command_option
  | Set_info of attribute
  | Declare_sort of sort_symbol * number
  | Define_sort of sort_symbol * sort_parameter list * parametric_sort
  | Declare_fun of fun_symbol * sort list * sort
  | Define_fun of fun_symbol * (variable * sort) list * sort * term
  | Push of number
  | Pop of number
  | Assert of term
  | Check_sat
  | Get_assertions
  | Get_value of term list
  | Get_assignment
  | Get_proof
  | Get_unsat_core
  | Get_info of info_flag
  | Get_option of attribute_name
  | Exit

type script = command list
		    
(* *** SIGNATURES *** *)

type sort_data =
  | Sort_declaration of int
  | Sort_definition of sort_parameter list * parametric_sort

type fun_data =
  | Fun_declaration of
      (sort_parameter list * parametric_sort list * parametric_sort) list
  | Fun_definition of (variable * sort) list * sort * term

module SortsMap =
  Map.Make
    (struct
      type t = sort_symbol
      let compare = Pervasives.compare
    end)

module FunsMap =
  Map.Make
    (struct
      type t = fun_symbol
      let compare = Pervasives.compare
    end)

module VarsMap =
  Map.Make
    (struct
      type t = variable
      let compare = Pervasives.compare
    end)

module VarsSet =
  Set.Make
    (struct
      type t = variable
      let compare = Pervasives.compare
    end)

let add_vars vars varsset  =
  List.fold_left
    (fun varsset var -> VarsSet.add var varsset) varsset vars
    
(* one can infer a signature from a signature by removing all definitions *)
type signature = {
  sorts: sort_data SortsMap.t;
  funs: fun_data FunsMap.t;
}

let add_sort sym data signature =
  { sorts =
      if SortsMap.mem sym signature.sorts
      then raise Signature_error
      else SortsMap.add sym data signature.sorts;
    funs = signature.funs;
  }

let add_fun sym data signature =
  { sorts = signature.sorts;
    funs =
      if FunsMap.mem sym signature.funs
      then raise Signature_error
      else FunsMap.add sym data signature.funs;
  }

let overload_fun sym data signature =
  { sorts = signature.sorts;
    funs =
      if FunsMap.mem sym signature.funs
      then
	let envdata = FunsMap.find sym signature.funs in
	match envdata, data with
	| Fun_declaration l1, Fun_declaration l2 ->
	   FunsMap.add sym (Fun_declaration (l1@l2)) signature.funs
	| _, _ -> raise Signature_error
      else FunsMap.add sym data signature.funs;
  }

(* *** CONCRETE TO ABSTRACT *** *)

let number num =
  int_of_string num

let sort_symbol id =
  id

let fun_symbol id =
  Identifier_fun id

let attribute_name cattr =
  cattr
		 
let sort_parameter sym =
  sym

let variable sym =
  sym

let logic_name sym =
  match sym with
  | "QF_UF" -> Qf_uf
  | _ -> raise Logic_error

let rec sort csort =
  match csort with
  | Concrete.Sort (id, csorts) ->
     Sort (sort_symbol id, List.map sort csorts)

let rec parametric_sort pars csort =
  match csort with
  | Concrete.Sort ((sym, []), []) ->
     let par = sort_parameter sym in
     if List.mem par pars
     then Par par
     else Par_sort (sort_symbol (sym, []), [])
  | Concrete.Sort (id, csorts) ->
     Par_sort (sort_symbol id, List.map (parametric_sort pars) csorts)
	      
let attribute (key, opt) =
  key, opt

let sorted_var (sym, csort) =
  variable sym, sort csort

let rec var_binding varsset (sym, cterm) =
  variable sym, term varsset cterm

and term varsset cterm =
  let map f opt = match opt with None -> None | Some a -> Some (f a) in
  match cterm with
  | Concrete.Spec_constant_term const ->
     App (Spec_constant_fun const, None, [])
  | Concrete.Qual_identifier_term ((sym, []), opt) ->
     let var = variable sym in
     begin 
       match opt with
       | None ->
	  if VarsSet.mem var varsset
	  then Var var
	  else App (fun_symbol (sym, []), None, []) 
       | Some csort ->
	  App (fun_symbol (sym, []), Some (sort csort), []) end
  | Concrete.Qual_identifier_term (id, opt) ->
     App (fun_symbol id, map sort opt, [])
  | Concrete.App_term (((sym, []), opt), cterms) ->
     let var = variable sym in
     begin 
       match opt, cterms with
       | None, [] ->
	  if VarsSet.mem var varsset
	  then Var var
	  else App (fun_symbol (sym, []), None, List.map (term varsset) cterms)
       | _, _ ->
	  App (fun_symbol (sym, []), map sort opt, List.map (term varsset) cterms) end
  | Concrete.App_term ((id, opt), cterms) ->
     App (fun_symbol id, map sort opt, List.map (term varsset) cterms)
  | Concrete.Let_term (var_bindings, cterm) ->
     let bindings = List.map (var_binding varsset) var_bindings in
     let vars, _ = List.split bindings in
     Let 
       (bindings, 
	term (add_vars vars varsset) cterm)
  | Concrete.Forall_term (csorted_vars, cterm) ->
     let sorted_vars = List.map sorted_var csorted_vars in
     let vars, _ = List.split sorted_vars in
     Forall 
       (sorted_vars, 
	term (add_vars vars varsset) cterm)
  | Concrete.Exists_term (csorted_vars, cterm) ->
     let sorted_vars = List.map sorted_var csorted_vars in
     let vars, _ = List.split sorted_vars in
     Exists
       (sorted_vars, 
	term (add_vars vars varsset) cterm)
  | Concrete.Attributed_term (cterm, cattributes) ->
     Attributed (term varsset cterm, List.map attribute cattributes)

let command_option copt =
  attribute copt

let info_flag cflag =
  cflag
	    
let command ccommand =
  match ccommand with
  | Concrete.Set_logic (sym) ->
     Set_logic (logic_name sym)
  | Concrete.Set_option (copt) ->
     Set_option (command_option copt)
  | Concrete.Set_info (cattribute) ->
     Set_info (attribute cattribute)
  | Concrete.Declare_sort (sym, num) ->
     Declare_sort (sort_symbol (sym, []), number num)
  | Concrete.Define_sort (sym, syms, csort) ->
     let pars = List.map sort_parameter syms in
     Define_sort
       (sort_symbol (sym, []), pars, parametric_sort pars csort)
  | Concrete.Declare_fun (sym, csorts, csort) -> 
     Declare_fun
       (fun_symbol (sym, []), List.map sort csorts, sort csort) 
  | Concrete.Define_fun (sym, csorted_vars, csort, cterm) -> 
     let sorted_vars = List.map sorted_var csorted_vars in
     let vars, _ = List.split sorted_vars in
     Define_fun
       (fun_symbol (sym, []), sorted_vars, sort csort,
	term (add_vars vars VarsSet.empty) cterm)
  | Concrete.Push (num) -> 
     Push (number num)
  | Concrete.Pop (num) -> 
     Pop (number num)
  | Concrete.Assert (cterm) -> 
     Assert (term VarsSet.empty cterm)
  | Concrete.Check_sat -> 
     Check_sat
  | Concrete.Get_assertions -> 
     Get_assertions
  | Concrete.Get_proof -> 
     Get_proof
  | Concrete.Get_unsat_core -> 
     Get_unsat_core
  | Concrete.Get_value (cterms) -> 
     Get_value (List.map (term VarsSet.empty) cterms)
  | Concrete.Get_assignment -> 
     Get_assignment
  | Concrete.Get_option (key) -> 
     Get_option (attribute_name key)
  | Concrete.Get_info (cinfo_flag) -> 
     Get_info (info_flag cinfo_flag)
  | Concrete.Exit -> 
     Exit

(* Sorts are parametric sorts *)
let rec par_sort_of_sort sort =
  match sort with
  | Sort (sort_sym, sorts) ->
     Par_sort (sort_sym, List.map par_sort_of_sort sorts)
       
(* *** CONSTANTS *** *)

let core_declaration =
  Core, 
  [("Bool", []), 0, []], 
  [ [], 
    Identifier_fun ("true", []),
    [],
    Par_sort (("Bool", []), []), 
    []
  ; [], 
    Identifier_fun ("false", []),
    [],
    Par_sort (("Bool", []), []),
    []
  ; [], 
    Identifier_fun ("not", []),
    [Par_sort (("Bool", []), [])],
    Par_sort (("Bool", []), []),
    []
  ; [], 
    Identifier_fun ("=>", []), 
    [Par_sort (("Bool", []), []); Par_sort (("Bool", []), [])],
    Par_sort (("Bool", []), []),
    []
  ; [], 
    Identifier_fun ("and", []),
    [Par_sort (("Bool", []), []); Par_sort (("Bool", []), [])],
    Par_sort (("Bool", []), []),
    []
  ; [], 
    Identifier_fun ("or", []),
    [Par_sort (("Bool", []), []); Par_sort (("Bool", []), [])],
    Par_sort (("Bool", []), []),
    []
  ; [], 
    Identifier_fun ("xor", []),
    [Par_sort (("Bool", []), []); Par_sort (("Bool", []), [])],
    Par_sort (("Bool", []), []),
    []
  ; ["A"],
    Identifier_fun ("=", []), 
    [Par "A"; Par "A"], 
    Par_sort (("Bool", []), []),
    []
  ; ["A"],
    Identifier_fun ("distinct", []), 
    [Par "A"; Par "A"], 
    Par_sort (("Bool", []), []),
    []
  ; ["A"],
    Identifier_fun ("ite", []), 
    [Par_sort (("Bool", []), []); Par "A"; Par "A"], 
    Par "A",
    []
  ]

let qf_uf_declaration = 
  Qf_uf, [Core]

(* *** ASSERTION SETS *** *)

type assertion_set = signature * term list

(* *** LOGIC SIGNATURES *** *)

let theory_declaration theory_name =
  match theory_name with
  | Core -> core_declaration

let logic_declaration logic_name = 
  match logic_name with
  | Qf_uf -> qf_uf_declaration 

let logic_signature logic_name =
  let _, theory_names = logic_declaration logic_name in
  let theory_declarations = List.map theory_declaration theory_names in
  let empty = 
  { sorts = SortsMap.empty;
    funs = FunsMap.empty;
  } in
  List.fold_left 
    (fun env (_, sort_declarations, par_fun_declarations) -> 
     let newenv = 
       List.fold_left 
	 (fun env (sym, n, _) -> add_sort sym (Sort_declaration n) env) 
	 env sort_declarations in
     List.fold_left 
       (fun env (pars, sym, sorts, sort, _) -> 
	overload_fun sym (Fun_declaration [pars, sorts, sort]) env) 
       newenv par_fun_declarations)
    empty theory_declarations
