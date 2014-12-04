(* Contexts *)

exception ContextError

type logic =
  | Qf_uf

type sort_data =
  | Sort_declaration of int
  | Sort_definition of Abstract.sort_parameter list * Abstract.parametric_sort

type fun_data =
  | Fun_declaration of (Abstract.parametric_sort list * Abstract.parametric_sort) list
  | Fun_definition of (Abstract.variable * Abstract.sort) list * Abstract.sort * Abstract.term

type context

val add_sort: Abstract.sort_symbol -> sort_data -> context -> context

val add_fun: Abstract.fun_symbol -> fun_data -> context -> context

val logic_context: logic -> context
