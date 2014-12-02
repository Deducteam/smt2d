(* Signatures *)

module Abs = Abstract

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
  
type signature = {
  sorts: int SortsMap.t;
  funs:
    ((Abstract.parametric_sort list * Abstract.parametric_sort) list)
      FunsMap.t;
  vars: Abstract.sort VarsMap.t }

let init logic =
  match logic with
  | Logic.Qf_uf ->
     { sorts =
	 SortsMap.add (Abs.sort_sym "Bool") 0 SortsMap.empty;
       funs =
	 List.fold_left2
	   (fun funs fun_sym sort -> FunsMap.add fun_sym sort funs)
	   FunsMap.empty
	   [Abs.fun_sym "true"; Abs.fun_sym "false";
	    Abs.fun_sym "not"; Abs.fun_sym "imply";
	    Abs.fun_sym "and"; Abs.fun_sym "or";
	    Abs.fun_sym "xor"; Abs.fun_sym "equal";
	    Abs.fun_sym "distinct"; Abs.fun_sym "ite"]
	   [[[],Abs.p_sort (Abs.sort_sym "Bool") []];
	    [[],Abs.p_sort (Abs.sort_sym "Bool") []];
	    [[Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.p_sort (Abs.sort_sym "Bool") []],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    [[Abs.par "A"; Abs.par "A"],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    [[Abs.par "A"; Abs.par "A"],
	     Abs.p_sort (Abs.sort_sym "Bool") []];
	    [[Abs.p_sort (Abs.sort_sym "Bool") []; Abs.par "A"; Abs.par "A"],
	     Abs.par "A"]];
       vars = VarsMap.empty}
