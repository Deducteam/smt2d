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
let get_logic_environment lexbuf =
  try 
    let rec get_logic_command () =
      let command = Parser.command Lexer.token lexbuf in
      match command with
      | Concrete.Set_option _
      | Concrete.Set_info _
      | Concrete.Get_option _
      | Concrete.Get_info _
        -> get_logic_command ()
      | Concrete.Set_logic s -> Abstract.logic_environment s
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
let get_checksats lexbuf =
  let rec push n stack =
    match n with
    | 0 -> stack
    | _ -> 
       push (n-1) (([],[],[]) :: stack) in
  let rec pop n stack =
    match n, stack with
    | 0, _ -> stack
    | _, current :: previous :: other ->
       pop (n-1) (previous :: other)
    | _, _ ->
       let (_, l, c) = Error.get_location lexbuf in
       raise (Error.Script_error (l, c)) in
  (* assert that the stack has a head and replaces it by (f head) *)
  let apply_to_current f stack =
    match stack with
    | current :: other -> (f current) :: other
    | [] -> assert false in
  (* add sort bindings, fun bindings or assertions to a set of the stack *)
  let add_sort binding (sort_bindings, fun_bindings, asserts) = 
    binding :: sort_bindings, fun_bindings, asserts in
  let add_fun binding (sort_bindings, fun_bindings, asserts) = 
    sort_bindings, binding :: fun_bindings, asserts in
  let add_funs bindings set = List.fold_left (fun set def -> add_fun def set) set bindings in
  let add_assert term (sort_bindings, fun_bindings, asserts) = 
    sort_bindings, fun_bindings, term :: asserts in
  (* add all sort and fun bindings of the stack to the env *)
  let add_stack_to_env env stack =
    List.fold_left 
      (fun ctx (sort_bindings, fun_bindings, _) -> 
       List.fold_left 
	 (fun ctx (key, data) -> Abstract.add_fun key data ctx) 
	 (List.fold_left 
	    (fun ctx (key, data) -> Abstract.add_sort key data ctx) 
	    ctx (List.rev sort_bindings)) 
	 (List.rev fun_bindings)) 
      env (List.rev stack) in
  let logic_env = get_logic_environment lexbuf in
  (* - checksats: (env, assertion list) list corresponding to previous check-sat commands
     - stack: current assertion-set stack - (sort bindings, fun bindings, assertions) list *)
  let rec get_checksats_command checksats stack =
    try
      let command = Parser.command Lexer.token lexbuf in
      match command with
      | Concrete.Push num ->
      	 let n = int_of_string num in get_checksats_command checksats (push n stack)
      | Concrete.Pop num ->
      	 let n = int_of_string num in get_checksats_command checksats (pop n stack)
      | Concrete.Declare_sort (sym, n) ->
	 let sort_binding = Abstract.declare_sort sym n in
      	 get_checksats_command 
	   checksats (apply_to_current (add_sort sort_binding) stack)
      | Concrete.Define_sort (sym, syms, tau) ->
	 let env = add_stack_to_env logic_env stack in
	 let sort_binding = Abstract.define_sort sym syms tau env in
	 (* let pars = List.map Abstract.sort_parameter syms in *)
	 (* let sort_binding =  *)
	 (*   Abstract.sort_symbol sym,  *)
	 (*   Abstract.Sort_definition (pars, Abstract.parametric_sort env.sorts pars tau) in *)
      	 get_checksats_command 
	   checksats (apply_to_current (add_sort sort_binding) stack)
      | Concrete.Declare_fun (sym, sorts, sort) ->
	 let env = add_stack_to_env logic_env stack in
	 let fun_binding = Abstract.declare_fun sym sorts sort env in
	 (* let fun_binding =  *)
	 (*   Abstract.fun_symbol sym,  *)
	 (*   Abstract.Fun_declaration  *)
	 (*     (List.map (Abstract.parametric_sort env []) sorts,  *)
	 (*      Abstract.parametric_sort env [] sort) in *)
      	 get_checksats_command
      	   checksats (apply_to_current (add_fun fun_binding) stack)
      | Concrete.Define_fun (sym, sorted_vars, s, t) ->
	 let env = add_stack_to_env logic_env stack in
	 let fun_bindings = Abstract.define_fun sym sorted_vars s t env in
	 (* let var_bindings = List.map (Abstract.sorted_vars env) sorted_vars in *)
	 (* let ctx =  *)
	 (*   List.fold_left  *)
	 (*     (fun ctx (key, data) -> Abstract.add_var key data ctx) env var_bindings in *)
	 (* let _, fun_bindings, term = Abstract.in_line_term ctx [] t in *)
	 (* let sort = Abstract.sort env s in *)
	 (* let fun_binding =  *)
	 (*   Abstract.fun_symbol sym,  *)
	 (*   Abstract.Fun_definition (bindings, sort, term) in *)
	 get_checksats_command
	   checksats (apply_to_current (add_funs (fun_bindings)) stack)
      | Concrete.Assert t ->
	 let env = add_stack_to_env logic_env stack in
	 let fun_bindings, term = Abstract.in_line_assert t env in 
	 (* let _, fun_bindings, term = Abstract.in_line_term env [] t in *)
	 get_checksats_command
	   checksats 
	   (apply_to_current (add_assert term) (apply_to_current (add_funs fun_bindings) stack))
      | Concrete.Get_value ts ->
	 let env = add_stack_to_env logic_env stack in
	 let fun_bindings = Abstract.in_line_get_value ts env in
	 (* let _, fun_bindings = *)
	 (*   List.fold_left *)
	 (*     (fun (env, fun_bindings) term -> *)
	 (*      let new_env, new_fun_bindings, _ = Abstract.in_line_term env term in *)
	 (*      (new_env, new_fun_bindings @ fun_bindings)) *)
	 (*     (env, []) ts in *)
	 get_checksats_command
	   checksats (apply_to_current (add_funs fun_bindings) stack)
      | Concrete.Check_sat ->
	 let env = add_stack_to_env logic_env stack in
	 let assertions = List.flatten (List.map (fun (_, _, terms) -> terms) stack) in
      	 get_checksats_command
      	   ((env, assertions) :: checksats) stack
      | Concrete.Set_logic _ ->
      	 let (_, l, c) = Error.get_location lexbuf in
      	 raise (Error.Logic_error ("Forbidden alternative set_logic command", l, c))
      | _ ->
	 get_checksats_command checksats stack
    with End_of_file -> List.rev checksats in
  get_checksats_command [] [([],[],[])]
		
let check_parse file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  begin try
      let _ = get_script lexbuf in ()
    with Parsing.Parse_error ->
      let (s, l, c) = Error.get_location lexbuf in
      raise (Error.Parser_error (s, l, c)) end

let check_context file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  begin try
      let _ = get_checksats lexbuf in ()
    with Parsing.Parse_error ->
      let (s, l, c) = Error.get_location lexbuf in
      raise (Error.Parser_error (s, l, c)) end
