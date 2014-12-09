exception Logic_error
exception Script_error

(* parse the whole file *)
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
  try 
    let rec get_logic_name_command () =
      let command =
	Abstract.command (Parser.command Lexer.token lexbuf) in
      match command with
      | Abstract.Set_option _
      | Abstract.Set_info _
      | Abstract.Get_option _
      | Abstract.Get_info _
        -> get_logic_name_command ()
      | Abstract.Set_logic s -> s
      | _ ->
	 let (_, l, c) = Error.get_location lexbuf in
	 raise (Error.Script_error (l, c)) in
    get_logic_name_command ()
  with End_of_file ->
    let (_, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error ("End-of-file", l, c))
	  
let theory_declaration theory_name =
  match theory_name with
  | "Core" -> Abstract.core_declaration
  | _ -> raise Logic_error
		
let logic_declaration logic_name = 
  match logic_name with
  | "QF_UF" -> Abstract.qf_uf_declaration 
  | _ -> raise Logic_error
    
let logic_signature logic_name =
  let _, theory_names = logic_declaration logic_name in
  let theory_declarations =
    List.map theory_declaration theory_names in
  let empty = Signature.empty in
  List.fold_left 
    (fun env (_, sort_declarations, par_fun_declarations) -> 
     let newenv = 
       List.fold_left 
	 (fun env (sym, n, _) ->
	  Signature.add_sort sym (Signature.Sort_declaration n) env) 
	 env sort_declarations in
     List.fold_left 
       (fun env (pars, sym, sorts, sort, _) -> 
        Signature.overload_fun
	  sym (Signature.Fun_declaration [pars, sorts, sort]) env) 
       newenv par_fun_declarations)
    empty theory_declarations

let rec push n stack =
  match n, stack with
  | _, [] -> raise Script_error
  | 0, _ -> stack
  | _, current :: other ->
     push (n-1) (current :: stack)     

let rec pop n stack =
  match n, stack with
  | 0, _ -> stack
  | _, [] | _, [_] ->
     raise Script_error
  | _, current :: other ->
     pop (n-1) other

let rec run_in_line_definitions signature term =
  match term with
  | Abstract.Var _ -> signature, term
  | Abstract.App (fun_sym, opt, terms) ->
     let newsignature, newterms_rev = 
       List.fold_left 
	 (fun (signature, newterms_rev) term ->
	  let newsignature, newterm = run_in_line_definitions signature term in
	  newsignature, newterm :: newterms_rev) (signature, []) terms in
     newsignature, Abstract.App (fun_sym, opt, List.rev newterms_rev)
  | Abstract.Let (bindings, term) -> 
     let newsignature, newterm = run_in_line_definitions signature term in
     newsignature, Abstract.Let (bindings, newterm)
  | Abstract.Forall (sorted_vars, term) ->
     let newsignature, newterm = run_in_line_definitions signature term in
     newsignature, Abstract.Forall (sorted_vars, newterm)
  | Abstract.Exists (sorted_vars, term) ->
     let newsignature, newterm = run_in_line_definitions signature term in
     newsignature, Abstract.Exists (sorted_vars, newterm)
  | Abstract.Attributed (term, attributes) ->
     let attribute_names, _ = List.split attributes in
     if List.mem ":named" attribute_names
     then raise Error.Not_implemented
     else 
       let newsignature, newterm = run_in_line_definitions signature term in
       newsignature, Abstract.Attributed (newterm, attributes)

(* - finds the first set-logic command and checks that no forbiden command is used before
   - checks that no set-logic command is used after
   - returns the list of (env, assertion lists) couples 
     corresponding to each check-sat command of the script *)
let get_contexts lexbuf =
  let logic_signature = logic_signature (get_logic_name lexbuf) in
  (* - contexts: (env, assertion list) list corresponding to previous check-sat commands
     - stack: current assertion-set stack - (sort bindings, fun bindings, assertions) list *)
  let rec get_contexts_command contexts stack =
    try
      let signature, assertions, sets = 
	match stack with
	| (signature, assertions) :: sets -> signature, assertions, sets
	| [] -> assert false in
      let command =
	Abstract.command (Parser.command Lexer.token lexbuf) in
      (* pop function allows to assert assert that the stack has a head *)
      let get_contexts_command_with_set set =
	get_contexts_command contexts (set :: sets) in
      match command with
      | Abstract.Check_sat ->
	 get_contexts_command
	   ((signature, assertions) :: contexts) stack
      | Abstract.Push n ->
	 get_contexts_command contexts (push n stack)
      | Abstract.Pop n ->
	 get_contexts_command contexts (pop n stack)
      | Abstract.Declare_sort (sort_sym, n) ->
	 get_contexts_command_with_set
	   (Signature.add_sort
	      sort_sym
	      (Signature.Sort_declaration n) signature, assertions)
      | Abstract.Define_sort (sort_sym, pars, par_sort) ->
	 get_contexts_command_with_set
	   (Signature.add_sort
	      sort_sym
	      (Signature.Sort_definition (pars, par_sort)) signature,
	    assertions)
      | Abstract.Declare_fun (fun_sym, sorts, sort) ->
	 let par_sorts = List.map Abstract.par_sort_of_sort sorts in
	 let par_sort = Abstract.par_sort_of_sort sort in
	 get_contexts_command_with_set
	   (Signature.add_fun
	      fun_sym
	      (Signature.Fun_declaration
		 [[], par_sorts, par_sort]) signature, assertions)
      | Abstract.Define_fun (fun_sym, sorted_vars, sort, term) ->
	 let newsignature, newterm = run_in_line_definitions signature term in
	 get_contexts_command_with_set
	   (Signature.add_fun
	      fun_sym
	      (Signature.Fun_definition
		 (sorted_vars, sort, newterm)) newsignature,
	    assertions)
      | Abstract.Assert term ->
	 let newsignature, newterm = run_in_line_definitions signature term in
	 get_contexts_command_with_set
	   (newsignature, newterm :: assertions)
      | Abstract.Get_value terms ->
	 let newsignature =
	   List.fold_left 
	     (fun signature term -> 
	      let newsignature, _ = run_in_line_definitions signature term in
	      newsignature) signature terms in
	 get_contexts_command_with_set (newsignature, assertions)
      | Abstract.Set_logic _ ->
	 raise Script_error
      | _ ->
	 get_contexts_command contexts stack
    with End_of_file -> List.rev contexts in
  get_contexts_command [] [(logic_signature,[])]
