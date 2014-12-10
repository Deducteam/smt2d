(* Assertion-set stack *)

exception Set_stack_error

type set_stack = (Signature.signature * Abstract.term list) Stack.t

let create signature =
  let stack = Stack.create () in
  Stack.push (signature, []) stack;
  stack

let all stack = Stack.top stack

let rec push stack n =
  match n with
  | 0 -> ()
  | _ -> 
     let all = all stack in
     Stack.push all stack;
     push stack (n-1)
	  
let rec pop stack n =
  match n with
  | 0 -> ()
  | _ -> 
     try
       let _ = Stack.pop stack in
       let _ = pop stack (n-1) in ()
     with
     | Stack.Empty -> raise Set_stack_error

let add_sort stack sort_sym sort_data =
  let (signature, assertions) = Stack.pop stack in
  Stack.push
    (Signature.add_sort sort_sym sort_data signature, assertions)
    stack

let add_fun stack fun_sym fun_data =
  let (signature, assertions) = Stack.pop stack in
  Stack.push
    (Signature.add_fun fun_sym fun_data signature, assertions)
    stack

let add_assertion stack assertion =
  let (signature, assertions) = Stack.pop stack in
  Stack.push
    (signature, assertion :: assertions)
    stack
  
