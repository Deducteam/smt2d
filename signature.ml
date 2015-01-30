(* Signatures *)

exception Signature_error

module Abs = Abstract

type sort_data =
  | Sort_declaration of int
  | Sort_definition of
      Abs.sort_parameter list * Abs.parametric_sort

type fun_data =
  | Fun_declaration of Abs.sort list * Abs.sort
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
