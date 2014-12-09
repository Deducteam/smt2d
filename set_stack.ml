(* Assertion-set stack *)

exception Set_stack_error

(* non empty list *)
type set_stack = (Signature.signature * Abstract.term list) list
	    
let rec push n stack =
  match n, stack with
  | _, [] -> raise Set_stack_error
  | 0, _ -> stack
  | _, current :: other -> push (n-1) (current :: stack)     

let rec pop n stack =
  match n, stack with
  | 0, _ -> stack
  | _, [] | _, [_] -> raise Set_stack_error
  | _, current :: other -> pop (n-1) other

let add_sort sort_sym sort_data stack =
  match stack with
  | (signature, current) :: sets -> 
     (Signature.add_sort sort_sym sort_data signature, current) :: sets
  | _ -> assert false

let add_fun fun_sym fun_data stack =
  match stack with
  | (signature, current) :: sets -> 
     (Signature.add_fun fun_sym fun_data signature, current) :: sets
  | _ -> assert false
