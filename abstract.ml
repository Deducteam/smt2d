(* smtlib2 abstract syntax *)

exception Logic_error
exception Environment_error

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
    Identifier_fun ("imply", []), 
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
    Identifier_fun ("equal", []), 
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

(* *** ENVIRONMENT *** *)

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

(* one can infer a signature from a environment by removing all definitions *)
type environment = {
  sorts: sort_data SortsMap.t;
  funs: fun_data FunsMap.t;
  vars: sort VarsMap.t }

let add_sort sym data environment = 
  { sorts = 
      if SortsMap.mem sym environment.sorts
      then raise Environment_error
      else SortsMap.add sym data environment.sorts;
    funs = environment.funs;
    vars = environment.vars }

let add_fun sym data environment = 
  { sorts = environment.sorts;
    funs = 
      if FunsMap.mem sym environment.funs
      then raise Environment_error
      else FunsMap.add sym data environment.funs;
    vars = environment.vars }

let add_fun_overload sym data environment =
  { sorts = environment.sorts;
    funs = 
      if FunsMap.mem sym environment.funs
      then 
	let envdata = FunsMap.find sym environment.funs in
	match envdata, data with
	| Fun_declaration l1, Fun_declaration l2 -> 
	   FunsMap.add sym (Fun_declaration (l1@l2)) environment.funs
	| _, _ -> raise Environment_error
      else FunsMap.add sym data environment.funs;
    vars = environment.vars }

(* *** SCOPING *** *)

let theory_declaration theory_name =
  match theory_name with
  | Core -> core_declaration

let logic_declaration logic_name = 
  match logic_name with
  | Qf_uf -> qf_uf_declaration 

let environment logic_name =
  let _, theory_names = logic_declaration logic_name in
  let theory_declarations = List.map theory_declaration theory_names in
  let empty = 
  { sorts = SortsMap.empty;
    funs = FunsMap.empty;
    vars = VarsMap.empty } in
  List.fold_left 
    (fun env (_, sort_declarations, par_fun_declarations) -> 
     let newenv = 
       List.fold_left 
	 (fun env (sym, n, _) -> add_sort sym (Sort_declaration n) env) 
	 env sort_declarations in
     List.fold_left 
       (fun env (pars, sym, sorts, sort, _) -> 
	add_fun_overload sym (Fun_declaration [pars, sorts, sort]) env) 
       newenv par_fun_declarations)
    empty theory_declarations

let logic_environment sym =
  match sym with
  | "QF_UF" -> environment Qf_uf
  | _ -> raise Logic_error

let declare_sort sym n = 
  (sym, []), Sort_declaration (int_of_string n)

let define_sort sym syms tau =
  assert false (* à compléter *)

let declare_fun sym sorts sort env =
  assert false (* à compléter *)

let define_fun sym sorted_vars s t env =
  assert false (* à compléter *)

let in_line_assert t env =
  assert false (* à compléter *)

let in_line_get_value ts env =
  assert false (* à compléter *)
