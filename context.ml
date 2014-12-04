(* Contexts *)

module Abs = Abstract

exception ContextError

type logic =
  | Qf_uf

type sort_data =
  | Sort_declaration of int
  | Sort_definition of Abstract.sort_parameter list * Abstract.parametric_sort

type fun_data =
  | Fun_declaration of (Abstract.parametric_sort list * Abstract.parametric_sort) list
  | Fun_definition of (Abstract.variable * Abstract.sort) list * Abstract.sort * Abstract.term

module SortsMap =
  Map.Make
    (struct
      type t = Abstract.sort_symbol
      let compare = Pervasives.compare
    end)

module FunsMap =
  Map.Make
    (struct
      type t = Abstract.fun_symbol
      let compare = Pervasives.compare
    end)

module VarsMap = 
  Map.Make
    (struct
      type t = Abstract.variable
      let compare = Pervasives.compare
    end)      
  
(* one can infer a signature from a context by removing all the definitions *)
type context = {
  sorts: sort_data SortsMap.t;
  funs: fun_data FunsMap.t;
  vars: Abstract.sort VarsMap.t }

let add_sort sym data context = 
  { sorts = 
      if SortsMap.mem sym context.sorts
      then raise ContextError
      else SortsMap.add sym data context.sorts;
    funs = context.funs;
    vars = context.vars }

let add_fun sym data context = 
  { sorts = context.sorts;
    funs = 
      if FunsMap.mem sym context.funs
      then raise ContextError
      else FunsMap.add sym data context.funs;
    vars = context.vars }

let logic_context logic =
  match logic with
  | Qf_uf ->
     { sorts =
	 SortsMap.add (Abs.sort_sym "Bool") (Sort_declaration 0) SortsMap.empty;
       funs =
	 List.fold_left2
	   (fun funs fun_sym sort -> FunsMap.add fun_sym sort funs)
	   FunsMap.empty
	   [Abs.fun_sym "true"; Abs.fun_sym "false";
	    Abs.fun_sym "not"; Abs.fun_sym "imply";
	    Abs.fun_sym "and"; Abs.fun_sym "or";
	    Abs.fun_sym "xor"; Abs.fun_sym "equal";
	    Abs.fun_sym "distinct"; Abs.fun_sym "ite"]
	   [Fun_declaration [[],Abs.p_sort (Abs.sort_sym "Bool") []];
	    Fun_declaration [[],Abs.p_sort (Abs.sort_sym "Bool") []];
	    Fun_declaration [[Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    Fun_declaration [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    Fun_declaration [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    Fun_declaration [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    Fun_declaration [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    Fun_declaration [[Abs.par "A"; Abs.par "A"],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    Fun_declaration [[Abs.par "A"; Abs.par "A"],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    Fun_declaration [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.par "A"; Abs.par "A"],
	     Abs.par "A"]];
       vars = VarsMap.empty }

