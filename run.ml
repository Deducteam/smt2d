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
let get_logic_name lexbuf =
  try 
    let rec get_logic_name_command () =
      let command = Parser.command Lexer.token lexbuf in
      match command with
      | Concrete.Set_option _
      | Concrete.Set_info _
      | Concrete.Get_option _
      | Concrete.Get_info _
        -> get_logic_name_command ()
      | Concrete.Set_logic s -> Abstract.logic_name s
      | _ ->
	 let (_, l, c) = Error.get_location lexbuf in
	 raise (Error.Script_error (l, c)) in
    get_logic_name_command ()
  with End_of_file ->
    let (_, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error ("End-of-file", l, c))
	  
(* - finds the first set-logic command and checks that no forbiden command is used before
   - checks that no set-logic command is used after
   - returns the list of (env, assertion lists) couples 
     corresponding to each check-sat command of the script *)
let get_contexts lexbuf =
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
  let logic_signature = Abstract.logic_signature (get_logic_name lexbuf) in
  (* - contexts: (env, assertion list) list corresponding to previous check-sat commands
     - stack: current assertion-set stack - (sort bindings, fun bindings, assertions) list *)
  let rec get_contexts_command contexts stack =
    try
      let command = Parser.command Lexer.token lexbuf in
      match command with
      | Concrete.Check_sat ->
	 begin match stack with
	 | current :: _ -> get_contexts_command (current :: contexts) stack
	 | [] -> assert false (* the implementation of pop garantees it *) end
      | _ ->
	 get_contexts_command contexts (Abstract.run_command command stack)
    with End_of_file -> List.rev contexts in
  get_contexts_command [] [(logic_signature,[])]
