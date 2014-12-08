(* smtlib2 abstract syntax *)

exception Logic_error
exception Signature_error
exception Script_error

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

(* module VarsMap =  *)
(*   Map.Make *)
(*     (struct *)
(*       type t = variable *)
(*       let compare = Pervasives.compare *)
(*     end)       *)

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

let sort_symbol sym =
  sym, []

let logic_name sym =
  match sym with
  | "QF_UF" -> Qf_uf
  | _ -> raise Logic_error

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

(* *** RUN COMMANDS *** *)

let rec push n stack =
  match n, stack with
  | _, [] -> raise Script_error
  | 0, _ -> stack
  | _, current :: other ->
     push (n-1) (current :: stack)     

let rec pop n stack =
  match n, stack with
  | 0, _ -> stack
  | _, [] | _, [_] ->
     raise Script_error
  | _, current :: other ->
     pop (n-1) other

(* assert that the stack has a head and replaces it by (f head) *)
let apply_to_signature f stack =
  match stack with
  | (current, assertions) :: other -> (f current, assertions) :: other
  | [] -> assert false

let run_command command stack =
  match command with
  | Concrete.Push num ->
     let n = number num in push n stack
  | Concrete.Pop num ->
     let n = number num in pop n stack
  | Concrete.Declare_sort (sym, num) ->
     let sort = sort_symbol sym in
     let n = number num in
     apply_to_signature (add_sort sort (Sort_declaration n)) stack
  | Concrete.Define_sort (sym, syms, tau) ->
     raise Error.Not_implemented
  | Concrete.Declare_fun (sym, sorts, sort) ->
     raise Error.Not_implemented
  | Concrete.Define_fun (sym, sorted_vars, s, t) ->
     raise Error.Not_implemented
  | Concrete.Assert t ->
     raise Error.Not_implemented
  | Concrete.Get_value ts ->
     raise Error.Not_implemented
  | Concrete.Set_logic _ ->
     raise Logic_error
  | _ -> stack
