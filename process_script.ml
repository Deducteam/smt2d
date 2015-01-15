exception Script_error

(* parses the whole file *)
let get_script lexbuf = 
  let rec add_command () script =
    try
      let command =
	Abstract.command (Parser.command Lexer.token lexbuf) in
      add_command () (command :: script)
    with End_of_file -> script in
  add_command () []

(* - finds the first set-logic command, returns its logic
   - checks that no forbiden command is used before *)
let get_logic_name lexbuf =
  let rec get_logic_name_command () =
    let command =
      Abstract.command (Parser.command Lexer.token lexbuf) in
    match command with
    | Abstract.Set_option _
    | Abstract.Set_info _
    | Abstract.Get_option _
    | Abstract.Get_info _ -> get_logic_name_command ()
    | Abstract.Set_logic s -> s
    | Abstract.Declare_sort _ | Abstract.Define_sort _
    | Abstract.Declare_fun _ | Abstract.Define_fun _
    | Abstract.Push _ | Abstract.Pop _ 
    | Abstract.Assert _ | Abstract.Check_sat | Abstract.Get_value _
    | Abstract.Get_assertions | Abstract.Get_assignment | Abstract.Get_proof
    | Abstract.Get_unsat_core | Abstract.Exit -> raise Script_error in
  get_logic_name_command ()

let rec add_in_line_definitions signature def_bindings term =
  match term with
  | Abstract.Var _ -> def_bindings
  | Abstract.App (_, _, terms) ->
     List.fold_left (add_in_line_definitions signature) def_bindings terms
  | Abstract.Let (_, term)
  | Abstract.Forall (_, term)
  | Abstract.Exists (_, term) ->
     add_in_line_definitions signature def_bindings term 
  | Abstract.Attributed (term, attributes) ->
     let attribute_names, _ = List.split attributes in
     if List.mem ":named" attribute_names
     then raise Error.Not_implemented
     else add_in_line_definitions signature def_bindings term

(* - finds the first set-logic command and checks that no forbiden command is used before
   - checks that no set-logic command is used after
   - returns the list of (env, assertion lists) couples 
     corresponding to each check-sat command of the script *)
let get_contexts lexbuf =
  let logic_signature =
    Signature.logic_signature (get_logic_name lexbuf) in
  (* - contexts: (signature, assertion list) list corresponding to previous check-sat commands
     - stack: current assertion-set stack - (sort bindings, fun bindings, assertions) list *)
  let stack = Set_stack.create logic_signature in
  let contexts = ref [] in
  let rec get_contexts_command () =
    try
      let command = Abstract.command (Parser.command Lexer.token lexbuf) in
      begin 
	match command with
	| Abstract.Check_sat -> contexts := (Set_stack.all stack) :: !contexts
	| Abstract.Push n -> Set_stack.push stack n
	| Abstract.Pop n -> Set_stack.pop stack n
	| Abstract.Declare_sort (sort_sym, n) ->
	   Set_stack.add_sort stack sort_sym (Signature.User_sort_declaration n)
	| Abstract.Define_sort (sort_sym, pars, par_sort) ->
	   Set_stack.add_sort stack sort_sym (Signature.Sort_definition (pars, par_sort))
	| Abstract.Declare_fun (fun_sym, sorts, sort) ->
	   Set_stack.add_fun 
	     stack fun_sym (Signature.User_fun_declaration (sorts, sort))
	| Abstract.Define_fun (fun_sym, sorted_vars, sort, term) ->
	   let signature, _ = Set_stack.all stack in
	   let def_bindings = add_in_line_definitions signature [] term in
	   List.iter 
	     (fun (fun_sym, fun_data) -> Set_stack.add_fun stack fun_sym fun_data) 
	     (List.rev def_bindings);
	   Set_stack.add_fun
	     stack fun_sym (Signature.Fun_definition (sorted_vars, sort, term))
	| Abstract.Assert term ->
	   let signature, _ = Set_stack.all stack in
	   let def_bindings = add_in_line_definitions signature [] term in
	   List.iter 
	     (fun (fun_sym, fun_data) -> Set_stack.add_fun stack fun_sym fun_data) 
	     (List.rev def_bindings);
	   Set_stack.add_assertion stack term;
	| Abstract.Get_value terms ->
	   let signature, _ = Set_stack.all stack in
	   let def_bindings = List.fold_left (add_in_line_definitions signature) [] terms in
	   List.iter 
	     (fun (fun_sym, fun_data) -> Set_stack.add_fun stack fun_sym fun_data) 
	     (List.rev def_bindings);
	| Abstract.Set_logic _ ->
	   raise Script_error
	| Abstract.Set_info _ | Abstract.Set_option _ 
	| Abstract.Get_assertions | Abstract.Get_assignment | Abstract.Get_proof
	| Abstract.Get_unsat_core | Abstract.Get_info _ 
	| Abstract.Get_option _ | Abstract.Exit -> () end;
      get_contexts_command ()
    with End_of_file -> List.rev !contexts in
  get_contexts_command ()
