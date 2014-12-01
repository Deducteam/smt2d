(* Signatures *)

type signature = {
  sorts: Abstract.sort_symbol * int Hashtbl.t ;
  funs: Abstract.fun_symbol * 
	  ((Abstract.parametric_sort list * Abstract.parametric_sort) list) Hashtbl.t;
  vars: Abstract.variable * Abstract.sort Hashtbl.t }
