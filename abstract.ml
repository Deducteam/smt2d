(* smtlib2 abstract syntax and constants from Core theory *)

exception Abstract_error

(* Basic types *)

type number = int
type sort_symbol = Concrete.identifier
type fun_symbol =
  | Spec_constant_fun of Concrete.spec_constant
  | Identifier_fun of Concrete.identifier
let spec_constant_fun const = Spec_constant_fun const
let identifier_fun const = Identifier_fun const
type attribute_name = Concrete.keyword
type theory_name = string
type sort_parameter = Concrete.symbol
type variable = Concrete.symbol
type attribute_value = Concrete.attribute_value
type logic_name = string

(* Sorts *)

type sort = 
  | Sort of sort_symbol * sort list
  | Bool
let sort sort_sym sorts =
  Sort (sort_sym, sorts)
let bool = Bool

type parametric_sort =
  | Param of sort_parameter
  | Par_sort of sort_symbol * parametric_sort list
  | Par_bool
let param sort_par =
  Param sort_par
let par_sort sort_sym par_sorts =
  Par_sort (sort_sym, par_sorts)
let par_bool = Par_bool

(* Terms *)

type attribute = attribute_name * attribute_value option

type core_app =
  | True
  | False
  | Not of term
  | Imply of term * term
  | And of term * term
  | Or of term * term
  | Xor of term * term
  | Equal of term * term
  | Distinct of term * term
  | Ite of term * term * term

and term =
  | Var of variable
  | App of fun_symbol * sort option * term list
  | Core of core_app
  | Let of (variable * term) list * term
  | Forall of (variable * sort) list * term
  | Exists of (variable * sort) list * term
  | Attributed of term * attribute list
let t_var var = Var var
let t_app fun_sym opt terms = App (fun_sym, opt, terms)
let t_let sorted_vars term = Let (sorted_vars, term)
let t_forall bindings term = Forall (bindings, term)
let t_exists bindings term = Exists (bindings, term)
let t_attributed term attrs = Attributed (term, attrs)
let t_true = Core True
let t_false = Core False
let t_not t = Core (Not t)
let t_imply t1 t2 = Core (Imply (t1, t2))
let t_and t1 t2 = Core (And (t1, t2))
let t_or t1 t2 = Core (Or (t1, t2))
let t_xor t1 t2 = Core (Xor (t1, t2))
let t_equal t1 t2 = Core (Equal (t1, t2))
let t_distinct t1 t2 = Core (Distinct (t1, t2))
let t_ite t1 t2 t3 = Core (Ite (t1, t2, t3))

(* Theories *)

type sort_declaration = 
    { sort_symbol: sort_symbol; number: number; attributes: attribute list; }
let sort_declaration sort_sym num attrs =
    { sort_symbol = sort_sym; number = num; attributes = attrs; }

type par_fun_declaration =
    { sort_parameters: sort_parameter list; fun_symbol: fun_symbol;
      parametric_sorts: parametric_sort list; parametric_sort: parametric_sort;
      attributes: attribute list; }
let par_fun_declaration sort_pars fun_sym par_sorts par_sort attrs =
    { sort_parameters = sort_pars; fun_symbol = fun_sym;
      parametric_sorts = par_sorts; parametric_sort = par_sort;
      attributes = attrs; }

type theory_declaration = 
    { theory_name: theory_name; sort_declarations: sort_declaration list;
      par_fun_declarations: par_fun_declaration list; }
let theory_declaration th_name sort_decls par_fun_decls =
    { theory_name = th_name; sort_declarations = sort_decls;
      par_fun_declarations = par_fun_decls; }

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

let c_set_logic logic_name = Set_logic logic_name
let c_set_option command_option = Set_option command_option
let c_set_info attr = Set_info attr
let c_declare_sort sort_sym num = Declare_sort (sort_sym, num) 
let c_define_sort sort_sym sort_pars par_sort = Define_sort (sort_sym, sort_pars, par_sort)
let c_declare_fun fun_sym sorts sort = Declare_fun (fun_sym, sorts, sort)
let c_define_fun fun_sym sorted_vars sort term = Define_fun (fun_sym, sorted_vars, sort, term)
let c_push num = Push num 
let c_pop num = Pop num 
let c_assert term = Assert term 
let c_check_sat = Check_sat	 
let c_get_assertions = Get_assertions 
let c_get_value terms = Get_value terms
let c_get_assignment = Get_assignment 
let c_get_proof = Get_proof	 
let c_get_unsat_core = Get_unsat_core 
let c_get_info info_flag = Get_info info_flag
let c_get_option attr_name = Get_option attr_name
let c_exit : command = Exit

type script = command list

(* *** CONCRETE TO ABSTRACT *** *)

(* Functions removing chainable, left-assoc, right-assoc, and pairwise syntactic sugar *)
						   
let rec mk_left_assoc binop ts =
  match ts with
  | [] -> assert false 
  | [t] -> t
  | t1 :: t2 :: ts -> 
     let t = binop t1 t2 in
     mk_left_assoc binop (t :: ts)

let rec mk_right_assoc binop ts =
  match ts with
  | [] -> assert false
  | [t] -> t
  | t1 :: t2 :: ts -> 
     let t = mk_right_assoc binop (t2 :: ts) in
     binop t1 t

let mk_chainable binop ts =
  let rec mk_chain ts =
    match ts with
    | [] -> assert false
    | [_] -> [] 
    | t1 :: t2 :: ts -> binop t1 t2 :: mk_chain (t2 :: ts) in
  mk_left_assoc t_and (mk_chain ts)

let rec mk_pairwise binop ts = 
  let mk_pairs ts =
    match ts with 
    | [] -> assert false
    | t :: ts -> 
       List.map (binop t) ts @ [mk_pairwise binop ts] in
  match ts with
  | [] | [_] -> assert false
  | [t1; t2] -> binop t1 t2
  | _ -> mk_left_assoc t_and (mk_pairs ts)

(* Variable Environment *)
		      
module VarSet =
  Set.Make
    (struct
      type t = variable
      let compare = Pervasives.compare
    end)

type varset = VarSet.t

let empty_vars = VarSet.empty
		
let add_vars vars var_set =
  List.fold_left
    (fun var_set var -> VarSet.add var var_set) var_set vars

(* Translation functions from concrete to abstract syntax *)
    
let tr_number num = int_of_string num
let tr_sort_symbol id = id
let tr_fun_symbol id = identifier_fun id
let tr_attribute_name cattr = cattr
let tr_sort_parameter sym = sym
let tr_variable sym = sym
let tr_logic_name sym = sym

(* Sorts *)

let rec tr_sort csort =
  match csort with
  | Concrete.Sort (id, csorts) ->
     sort (tr_sort_symbol id) (List.map tr_sort csorts)
  | Concrete.Core_sort (Concrete.CBool, csorts) ->
     match csorts with
     | [] -> bool
     | _ -> raise Abstract_error

let rec tr_parametric_sort pars csort =
  match csort with
  | Concrete.Sort ((sym, []), []) ->
     let par = tr_sort_parameter sym in
     if List.mem par pars
     then param par
     else par_sort (tr_sort_symbol (sym, [])) []
  | Concrete.Sort (id, csorts) ->
     par_sort (tr_sort_symbol id) (List.map (tr_parametric_sort pars) csorts)
  | Concrete.Core_sort (Concrete.CBool, csorts) ->
     match csorts with
     | [] -> par_bool
     | _ -> raise Abstract_error

(* Terms *)
	      
let tr_attribute (key, opt) = key, opt
				  
let tr_sorted_var (sym, csort) = tr_variable sym, tr_sort csort

let rec tr_var_binding var_set (sym, cterm) =
  tr_variable sym, tr_term var_set cterm

and tr_core_app var_set const cterms =
  match const, List.map (tr_term var_set) cterms with
  | Concrete.CTrue, [] -> t_true
  | Concrete.CTrue, _ -> raise Abstract_error
  | Concrete.CFalse, [] -> t_false
  | Concrete.CFalse, _ -> raise Abstract_error
  | Concrete.CNot, [t] -> t_not t
  | Concrete.CNot, _ -> raise Abstract_error
  | Concrete.CImply, terms -> mk_right_assoc t_imply terms
  | Concrete.CAnd, terms -> mk_left_assoc t_and terms
  | Concrete.COr, terms -> mk_left_assoc t_or terms
  | Concrete.CXor, terms -> mk_left_assoc t_xor terms
  | Concrete.CEqual, terms -> mk_chainable t_equal terms
  | Concrete.CDistinct, terms -> mk_pairwise t_distinct terms
  | Concrete.CIte, [t1; t2; t3] -> t_ite t1 t2 t3
  | Concrete.CIte, _ -> raise Abstract_error

and tr_term var_set cterm =
  match cterm with
  | Concrete.Spec_constant_term const ->
     t_app (spec_constant_fun const) None []
  | Concrete.App_term ((((sym, []) as id), None), []) ->
     let var = tr_variable sym in
     if VarSet.mem var var_set
     then t_var var
     else t_app (tr_fun_symbol id) None []
  | Concrete.App_term ((id, opt), cterms) ->
     t_app (tr_fun_symbol id) (Util.option_map tr_sort opt)
	  (List.map (tr_term var_set) cterms)
  | Concrete.Core_app_term (const, cterms) ->
     tr_core_app var_set const cterms
  | Concrete.Let_term (var_bindings, cterm) ->
     let bindings = List.map (tr_var_binding var_set) var_bindings in
     let vars, _ = List.split bindings in
     t_let bindings (tr_term (add_vars vars var_set) cterm)
  | Concrete.Forall_term (csorted_vars, cterm) ->
     let sorted_vars = List.map tr_sorted_var csorted_vars in
     let vars, _ = List.split sorted_vars in
     t_forall sorted_vars (tr_term (add_vars vars var_set) cterm)
  | Concrete.Exists_term (csorted_vars, cterm) ->
     let sorted_vars = List.map tr_sorted_var csorted_vars in
     let vars, _ = List.split sorted_vars in
     t_exists sorted_vars (tr_term (add_vars vars var_set) cterm)
  | Concrete.Attributed_term (cterm, cattributes) ->
     t_attributed (tr_term var_set cterm) (List.map tr_attribute cattributes)

(* Command options and info names *)

let tr_command_option copt = tr_attribute copt

let tr_info_flag cflag = cflag

(* Commands *)
	    
let tr_command ccommand =
  match ccommand with
  | Concrete.Set_logic (sym) ->
     c_set_logic (tr_logic_name sym)
  | Concrete.Set_option (copt) ->
     c_set_option (tr_command_option copt)
  | Concrete.Set_info (cattribute) ->
     c_set_info (tr_attribute cattribute)
  | Concrete.Declare_sort (sym, num) ->
     c_declare_sort (tr_sort_symbol (sym, [])) (tr_number num)
  | Concrete.Define_sort (sym, syms, csort) ->
     let pars = List.map tr_sort_parameter syms in
     c_define_sort
       (sym, []) pars (tr_parametric_sort pars csort)
  | Concrete.Declare_fun (sym, csorts, csort) -> 
     c_declare_fun
       (tr_fun_symbol (sym, []))
	(List.map tr_sort csorts) (tr_sort csort) 
  | Concrete.Define_fun (sym, csorted_vars, csort, cterm) -> 
     let sorted_vars = List.map tr_sorted_var csorted_vars in
     let vars, _ = List.split sorted_vars in
     c_define_fun
       (tr_fun_symbol (sym, []))
	sorted_vars (tr_sort csort)
	(tr_term (add_vars vars VarSet.empty) cterm)
  | Concrete.Push (num) -> 
     c_push (tr_number num)
  | Concrete.Pop (num) -> 
     c_pop (tr_number num)
  | Concrete.Assert (cterm) -> 
     c_assert (tr_term VarSet.empty cterm)
  | Concrete.Check_sat -> 
     c_check_sat
  | Concrete.Get_assertions -> 
     c_get_assertions
  | Concrete.Get_proof -> 
     c_get_proof
  | Concrete.Get_unsat_core -> 
     c_get_unsat_core
  | Concrete.Get_value (cterms) -> 
     c_get_value (List.map (tr_term VarSet.empty) cterms)
  | Concrete.Get_assignment -> 
     c_get_assignment
  | Concrete.Get_option (key) -> 
     c_get_option (tr_attribute_name key)
  | Concrete.Get_info (cinfo_flag) -> 
     c_get_info (tr_info_flag cinfo_flag)
  | Concrete.Exit -> 
     c_exit
       
(* *** UTILS *** *)

let rec substitute_par_sort bindings par_sort =
  match par_sort with
  | Param par -> List.assoc par bindings
  | Par_sort (sort_sym, par_sorts) -> 
     sort sort_sym (List.map (substitute_par_sort bindings) par_sorts)
  | Par_bool -> Bool

(* *** CONSTANTS *** *)

let bool_sym = "Bool", []
let true_sym = identifier_fun ("true", [])
let false_sym = identifier_fun ("false", [])
let not_sym = identifier_fun ("not", [])
let imply_sym = identifier_fun ("=>", [])
let and_sym = identifier_fun ("and", [])
let or_sym = identifier_fun ("or", [])
let xor_sym = identifier_fun ("xor", [])
let equal_sym = identifier_fun ("=", [])
let distinct_sym = identifier_fun ("distinct", [])
let ite_sym = identifier_fun ("ite", [])

let core_declaration =
  let a_sym = "A" in
  let bool_sort = par_sort bool_sym [] in
  let a_sort = param a_sym in
  let sort_decls = [sort_declaration bool_sym 0 []] in
  let par_fun_decls =
    [ par_fun_declaration [] true_sym [] bool_sort []
    ; par_fun_declaration [] false_sym [] bool_sort []
    ; par_fun_declaration [] not_sym [bool_sort] bool_sort []
    ; par_fun_declaration [] imply_sym [bool_sort; bool_sort] bool_sort [":right-assoc", None]
    ; par_fun_declaration [] and_sym [bool_sort; bool_sort] bool_sort [":left-assoc", None]
    ; par_fun_declaration [] or_sym [bool_sort; bool_sort] bool_sort [":left-assoc", None]
    ; par_fun_declaration [] xor_sym [bool_sort; bool_sort] bool_sort [":left-assoc", None]
    ; par_fun_declaration [a_sym] equal_sym [a_sort; a_sort] bool_sort [":chainable", None]
    ; par_fun_declaration [a_sym] distinct_sym [a_sort; a_sort] bool_sort [":pairwise", None]
    ; par_fun_declaration [a_sym] ite_sym [bool_sort; a_sort; a_sort] a_sort []
    ] in
    theory_declaration "Core" sort_decls par_fun_decls 

let qf_uf_declaration = "QF_UF", ["Core"]

(* Get theories and logics declarations from their names *)

let get_theory_declaration theory_name =
  match theory_name with
  | "Core" -> core_declaration
  | _ -> raise Abstract_error

let get_logic_declaration logic_name = 
  match logic_name with
  | "QF_UF" -> qf_uf_declaration 
  | _ -> raise Abstract_error
