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
    
(* one can infer a signature from a signature by removing all definitions *)
type signature = {
  sorts: sort_data SortsMap.t;
  funs: fun_data FunsMap.t;
}

let empty =
  { sorts = SortsMap.empty;
    funs = FunsMap.empty;
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
