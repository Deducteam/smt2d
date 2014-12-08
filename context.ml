type context

(* parse the whole file *)
let get_script lexbuf = 
  let rec add_command () script =
    try
      let command = Parser.command Lexer.token lexbuf in
      add_command () (command :: script)
    with End_of_file -> script in
  add_command () []

(* - finds the first set-logic command, returns its logic
   - checks that no forbiden command is used before *)
let get_logic_signature lexbuf =
  try 
    let rec get_logic_command () =
      let command = Parser.command Lexer.token lexbuf in
      match command with
      | Concrete.Set_option _
      | Concrete.Set_info _
      | Concrete.Get_option _
      | Concrete.Get_info _
        -> get_logic_command ()
      | Concrete.Set_logic s -> Abstract.logic_signature s
      | _ ->
	 let (_, l, c) = Error.get_location lexbuf in
	 raise (Error.Script_error (l, c)) in
    get_logic_command ()
  with End_of_file ->
    let (_, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error ("End-of-file", l, c))
	  
(* - finds the first set-logic command and checks that no forbiden command is used before
   - checks that no set-logic command is used after
   - returns the list of (env, assertion lists) couples 
     corresponding to each check-sat command of the script *)
let get_contexts lexbuf =
  (* let rec push n stack = *)
  (*   match n with *)
  (*   | 0 -> stack *)
  (*   | _ ->  *)
  (*      push (n-1) (([],[],[]) :: stack) in *)
  (* let rec pop n stack = *)
  (*   match n, stack with *)
  (*   | 0, _ -> stack *)
  (*   | _, current :: previous :: other -> *)
  (*      pop (n-1) (previous :: other) *)
  (*   | _, _ -> *)
  (*      let (_, l, c) = Error.get_location lexbuf in *)
  (*      raise (Error.Script_error (l, c)) in *)
  (* (\* assert that the stack has a head and replaces it by (f head) *\) *)
  (* let apply_to_current f stack = *)
  (*   match stack with *)
  (*   | current :: other -> (f current) :: other *)
  (*   | [] -> assert false in *)
  (* (\* add sort bindings, fun bindings or assertions to a set of the stack *\) *)
  (* let add_sort binding (sort_bindings, fun_bindings, asserts) =  *)
  (*   binding :: sort_bindings, fun_bindings, asserts in *)
  (* let add_fun binding (sort_bindings, fun_bindings, asserts) =  *)
  (*   sort_bindings, binding :: fun_bindings, asserts in *)
  (* let add_funs bindings set = List.fold_left (fun set def -> add_fun def set) set bindings in *)
  (* let add_assert term (sort_bindings, fun_bindings, asserts) =  *)
  (*   sort_bindings, fun_bindings, term :: asserts in *)
  (* (\* add all sort and fun bindings of the stack to the env *\) *)
  (* let add_stack_to_env env stack = *)
  (*   List.fold_left  *)
  (*     (fun ctx (sort_bindings, fun_bindings, _) ->  *)
  (*      List.fold_left  *)
  (* 	 (fun ctx (key, data) -> Abstract.add_fun key data ctx)  *)
  (* 	 (List.fold_left  *)
  (* 	    (fun ctx (key, data) -> Abstract.add_sort key data ctx)  *)
  (* 	    ctx (List.rev sort_bindings))  *)
  (* 	 (List.rev fun_bindings))  *)
  (*     env (List.rev stack) in *)
  (* let logic_env = get_logic_signature lexbuf in *)
  (* - contexts: (env, assertion list) list corresponding to previous check-sat commands
     - stack: current assertion-set stack - (sort bindings, fun bindings, assertions) list *)
  let rec get_contexts_command contexts stack =
    try
      let command = Parser.command Lexer.token lexbuf in
      match command with
      (* | Concrete.Push num -> *)
      (* 	 let n = int_of_string num in get_contexts_command contexts (push n stack) *)
      (* | Concrete.Pop num -> *)
      (* 	 let n = int_of_string num in get_contexts_command contexts (pop n stack) *)
      (* | Concrete.Declare_sort (sym, n) -> *)
      (* 	 let sort_binding = Abstract.declare_sort sym n in *)
      (* 	 get_contexts_command  *)
      (* 	   contexts (apply_to_current (add_sort sort_binding) stack) *)
      (* | Concrete.Define_sort (sym, syms, tau) -> *)
      (* 	 let env = add_stack_to_env logic_env stack in *)
      (* 	 let sort_binding = Abstract.define_sort sym syms tau env in *)
      (* 	 get_contexts_command  *)
      (* 	   contexts (apply_to_current (add_sort sort_binding) stack) *)
      (* | Concrete.Declare_fun (sym, sorts, sort) -> *)
      (* 	 let env = add_stack_to_env logic_env stack in *)
      (* 	 let fun_binding = Abstract.declare_fun sym sorts sort env in *)
      (* 	 get_contexts_command *)
      (* 	   contexts (apply_to_current (add_fun fun_binding) stack) *)
      (* | Concrete.Define_fun (sym, sorted_vars, s, t) -> *)
      (* 	 let env = add_stack_to_env logic_env stack in *)
      (* 	 let fun_bindings = Abstract.define_fun sym sorted_vars s t env in *)
      (* 	 get_contexts_command *)
      (* 	   contexts (apply_to_current (add_funs (fun_bindings)) stack) *)
      (* | Concrete.Assert t -> *)
      (* 	 let env = add_stack_to_env logic_env stack in *)
      (* 	 let fun_bindings, term = Abstract.in_line_assert t env in  *)
      (* 	 get_contexts_command *)
      (* 	   contexts  *)
      (* 	   (apply_to_current (add_assert term) (apply_to_current (add_funs fun_bindings) stack)) *)
      (* | Concrete.Get_value ts -> *)
      (* 	 let env = add_stack_to_env logic_env stack in *)
      (* 	 let fun_bindings = Abstract.in_line_get_value ts env in *)
      (* 	 get_contexts_command *)
      (* 	   contexts (apply_to_current (add_funs fun_bindings) stack) *)
      (* | Concrete.Check_sat -> *)
      (* 	 let env = add_stack_to_env logic_env stack in *)
      (* 	 let assertions = List.flatten (List.map (fun (_, _, terms) -> terms) stack) in *)
      (* 	 get_contexts_command *)
      (* 	   ((env, assertions) :: contexts) stack *)
      (* | Concrete.Set_logic _ -> *)
      (* 	 let (_, l, c) = Error.get_location lexbuf in *)
      (* 	 raise (Error.Logic_error ("Forbidden alternative set_logic command", l, c)) *)
      | _ ->
	 get_contexts_command contexts stack
    with End_of_file -> List.rev contexts in
  get_contexts_command [] [([],[],[])]
