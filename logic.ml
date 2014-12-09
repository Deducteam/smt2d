(* Get theories and logics declarations from their names *)

exception Logic_error

let theory_declaration theory_name =
  match theory_name with
  | "Core" -> Abstract.core_declaration
  | _ -> raise Logic_error

let logic_declaration logic_name = 
  match logic_name with
  | "QF_UF" -> Abstract.qf_uf_declaration 
  | _ -> raise Logic_error
