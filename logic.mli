(* Get theories and logics declarations from their names *)

exception Logic_error

val theory_declaration:
  Abstract.theory_name -> Abstract.theory_declaration

val logic_declaration:
  Abstract.logic_name -> Abstract.logic_declaration
