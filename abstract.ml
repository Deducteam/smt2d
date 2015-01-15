(* smtlib2 abstract syntax *)

type number = int
type sort_symbol = Concrete.identifier
type fun_symbol =
  | Spec_constant_fun of Concrete.spec_constant
  | Identifier_fun of Concrete.identifier
type attribute_name = Concrete.keyword
type theory_name = string
type sort_parameter = Concrete.symbol
type variable = Concrete.symbol
type attribute_value = Concrete.attribute_value
type logic_name = string

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

(* *** CONCRETE TO ABSTRACT *** *)

(* Variable Environment *)
		      
module VarSet =
  Set.Make
    (struct
      type t = variable
      let compare = Pervasives.compare
    end)

type varset = VarSet.t
    
let add_vars vars var_set =
  List.fold_left
    (fun var_set var -> VarSet.add var var_set) var_set vars

(* Translation functions *)
    
let number num = int_of_string num
let sort_symbol id = id
let fun_symbol id = Identifier_fun id
let attribute_name cattr = cattr
let sort_parameter sym = sym
let variable sym = sym
let logic_name sym = sym

(* Sorts *)

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

(* Terms *)
	      
let attribute (key, opt) = key, opt
				  
let sorted_var (sym, csort) = variable sym, sort csort

let rec var_binding var_set (sym, cterm) =
  variable sym, term var_set cterm

and term var_set cterm =
  match cterm with
  | Concrete.Spec_constant_term const ->
     App (Spec_constant_fun const, None, [])
  | Concrete.Qual_identifier_term (((sym, []) as id), None) ->
     let var = variable sym in
     if VarSet.mem var var_set
     then Var var
     else App (fun_symbol id, None, [])
  | Concrete.Qual_identifier_term (id, opt) ->
     App (fun_symbol id, Util.option_map sort opt, [])
  | Concrete.App_term ((id, opt), cterms) ->
     (* cannot be a variable because cterms is not empty *)
     App (fun_symbol id, Util.option_map sort opt,
	  List.map (term var_set) cterms)
  | Concrete.Let_term (var_bindings, cterm) ->
     let bindings = List.map (var_binding var_set) var_bindings in
     let vars, _ = List.split bindings in
     Let (bindings, term (add_vars vars var_set) cterm)
  | Concrete.Forall_term (csorted_vars, cterm) ->
     let sorted_vars = List.map sorted_var csorted_vars in
     let vars, _ = List.split sorted_vars in
     Forall (sorted_vars, term (add_vars vars var_set) cterm)
  | Concrete.Exists_term (csorted_vars, cterm) ->
     let sorted_vars = List.map sorted_var csorted_vars in
     let vars, _ = List.split sorted_vars in
     Exists (sorted_vars, term (add_vars vars var_set) cterm)
  | Concrete.Attributed_term (cterm, cattributes) ->
     Attributed (term var_set cterm, List.map attribute cattributes)

(* Command options and info names *)

let command_option copt = attribute copt

let info_flag cflag = cflag

(* Commands *)
	    
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
       ((sym, []), pars, parametric_sort pars csort)
  | Concrete.Declare_fun (sym, csorts, csort) -> 
     Declare_fun
       (fun_symbol (sym, []),
	List.map sort csorts, sort csort) 
  | Concrete.Define_fun (sym, csorted_vars, csort, cterm) -> 
     let sorted_vars = List.map sorted_var csorted_vars in
     let vars, _ = List.split sorted_vars in
     Define_fun
       (fun_symbol (sym, []),
	sorted_vars, sort csort,
	term (add_vars vars VarSet.empty) cterm)
  | Concrete.Push (num) -> 
     Push (number num)
  | Concrete.Pop (num) -> 
     Pop (number num)
  | Concrete.Assert (cterm) -> 
     Assert (term VarSet.empty cterm)
  | Concrete.Check_sat -> 
     Check_sat
  | Concrete.Get_assertions -> 
     Get_assertions
  | Concrete.Get_proof -> 
     Get_proof
  | Concrete.Get_unsat_core -> 
     Get_unsat_core
  | Concrete.Get_value (cterms) -> 
     Get_value (List.map (term VarSet.empty) cterms)
  | Concrete.Get_assignment -> 
     Get_assignment
  | Concrete.Get_option (key) -> 
     Get_option (attribute_name key)
  | Concrete.Get_info (cinfo_flag) -> 
     Get_info (info_flag cinfo_flag)
  | Concrete.Exit -> 
     Exit
       
(* *** UTILS *** *)

let rec substitute_par_sort bindings par_sort =
  match par_sort with
  | Par par -> List.assoc par bindings
  | Par_sort (sort_sym, par_sorts) -> 
     Sort (sort_sym, List.map (substitute_par_sort bindings) par_sorts)

(* *** CONSTANTS *** *)

let bool_sym = sort_symbol ("Bool", [])
let true_sym = fun_symbol ("true", [])
let false_sym = fun_symbol ("false", [])
let not_sym = fun_symbol ("not", [])
let imply_sym = fun_symbol ("=>", [])
let and_sym = fun_symbol ("and", [])
let or_sym = fun_symbol ("or", [])
let xor_sym = fun_symbol ("xor", [])
let equal_sym = fun_symbol ("=", [])
let distinct_sym = fun_symbol ("distinct", [])
let ite_sym = fun_symbol ("ite", [])

let core_declaration =
  let a_sym = "A" in
  let bool_sort = parametric_sort [] (Concrete.Sort (bool_sym, [])) in
  let a_par = sort_parameter a_sym in
  let a_sort =
    parametric_sort [a_par] (Concrete.Sort ((a_sym, []), [])) in
  "Core",
  [bool_sym, 0, []],
  [ [], true_sym, [], bool_sort, []
  ; [], false_sym, [], bool_sort, []
  ; [], not_sym, [bool_sort], bool_sort, []
  ; [], imply_sym, [bool_sort; bool_sort], bool_sort, [":right-assoc", None]
  ; [], and_sym, [bool_sort; bool_sort], bool_sort, [":left-assoc", None]
  ; [], or_sym, [bool_sort; bool_sort], bool_sort, [":left-assoc", None]
  ; [], xor_sym, [bool_sort; bool_sort], bool_sort, [":left-assoc", None]
  ; [a_par], equal_sym, [a_sort; a_sort], bool_sort, [":chainable", None]
  ; [a_par], distinct_sym, [a_sort; a_sort], bool_sort, [":pairwise", None]
  ; [a_par], ite_sym, [bool_sort; a_sort; a_sort], a_sort, []
  ]

let qf_uf_declaration = "QF_UF", ["Core"]
