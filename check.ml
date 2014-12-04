open Printf

let umsg = "Usage: check <file>"

let argspec = []

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
let get_logic lexbuf =
  try 
    let rec get_logic_command () =
      let command = Parser.command Lexer.token lexbuf in
      match command with
      | Script.Set_option _
      | Script.Set_info _
      | Script.Get_option _
      | Script.Get_info _
        -> get_logic_command ()
      | Script.Set_logic s ->
	 if s = "QF_UF"
	 then Context.Qf_uf
	 else
	   let (_, l, c) = Error.get_location lexbuf in
	   raise (Error.Logic_error (sprintf "Unrecognized logic %s" s, l, c))
      | _ ->
	 let (_, l, c) = Error.get_location lexbuf in
	 raise (Error.Script_error (l, c)) in
    get_logic_command ()
  with End_of_file ->
    let (_, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error ("End-of-file", l, c))
	  
(* - finds the first set-logic command and checks that no forbiden command is used before
   - checks that no set-logic command is used after
   - returns the list of (context, assertion lists) couples 
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
  (* add all sort and fun bindings of the stack to the context *)
  let add_stack_to_context context stack =
    List.fold_left 
      (fun ctx (sort_bindings, fun_bindings, _) -> 
       List.fold_left 
	 (fun ctx (key, data) -> Context.add_fun key data ctx) 
	 (List.fold_left 
	    (fun ctx (key, data) -> Context.add_sort key data ctx) 
	    ctx (List.rev sort_bindings)) 
	 (List.rev fun_bindings)) 
      context (List.rev stack) in
  let logic = get_logic lexbuf in
  let logic_context = Context.logic_context logic in
  (* - checksats: (context, assertion list) list corresponding to previous check-sat commands
     - stack: current assertion-set stack - (sort bindings, fun bindings, assertions) list *)
  let rec get_checksats_command checksats stack =
    try
      let command = Parser.command Lexer.token lexbuf in
      match command with
      | Script.Push num ->
      	 let n = int_of_string num in get_checksats_command checksats (push n stack)
      | Script.Pop num ->
      	 let n = int_of_string num in get_checksats_command checksats (pop n stack)
      | Script.Declare_sort (sym, n) ->
	 let sort_binding = 
	   Scope.scope_sort_symbol sym, 
	   Context.Sort_declaration (Scope.scope_number n) in
      	 get_checksats_command 
	   checksats (apply_to_current (add_sort sort_binding) stack)
      | Script.Define_sort (sym, syms, tau) ->
	 let context = add_stack_to_context logic_context stack in
	 let pars = List.map Scope.scope_sort_parameter syms in
	 let sort_binding = 
	   Scope.scope_sort_symbol sym, 
	   Context.Sort_definition (pars, Scope.scope_parametric_sort context.sorts pars tau) in
      	 get_checksats_command 
	   checksats (apply_to_current (add_sort sort_binding) stack)
      | Script.Declare_fun (sym, sorts, sort) ->
	 let context = add_stack_to_context logic_context stack in
	 let fun_binding = 
	   Scope.scope_fun_symbol sym, 
	   Context.Fun_declaration 
	     (List.map (Scope.scope_parametric_sort context []) sorts, 
	      Scope.scope_parametric_sort context [] sort) in
      	 get_checksats_command
      	   checksats (apply_to_current (add_fun fun_binding) stack)
      | Script.Define_fun (sym, sorted_vars, s, t) ->
	 let context = add_stack_to_context logic_context stack in
	 let var_bindings = List.map (Scope.scope_sorted_vars context) sorted_vars in
	 let sort = Scope.scope_sort context s in
	 let ctx = 
	   List.fold_left 
	     (fun ctx (key, data) -> Context.add_var key data ctx) context var_bindings in
	 let _, fun_bindings, term = Scope.scope_in_line_term ctx [] t in
	 let fun_binding = 
	   Scope.scope_fun_symbol sym, 
	   Context.Fun_definition (bindings, sort, term) in
	 get_checksats_command
	   checksats (apply_to_current (add_funs (fun_binding :: fun_bindings)) stack)
      | Script.Assert term ->
	 let context = add_stack_to_context logic_context stack in
	 let _, fun_bindings, term = Scope.scope_in_line_term context [] t in
	 get_checksats_command
	   checksats 
	   (apply_to_current (add_assert term) (apply_to_current (add_funs fun_bindings) stack))
      | Script.Get_value ts ->
	 let context = add_stack_to_context logic_context stack in
	 let _, fun_bindings = 
	   List.fold_left
	     (fun (context, fun_bindings) term -> 
	      let new_context, new_fun_bindings, _ = Scope.scope_in_line context term in
	      (new_context, new_fun_bindings @ fun_bindings)) 
	     (context, []) ts in
	 get_checksats_command
	   checksats (apply_to_current (add_funs fun_bindings) stack)
      | Script.Check_sat ->
	 let context = add_stack_to_context logic_context stack in
	 let assertions = List.flatten (List.map (fun (_, _, terms) -> terms) stack) in
      	 get_checksats_command
      	   ((context, assertions) :: checksats) stack
      | Script.Set_logic _ ->
      	 let (_, l, c) = Error.get_location lexbuf in
      	 raise (Error.Logic_error ("Forbidden alternative set_logic command", l, c))
      | _ ->
	 get_checksats_command checksats stack
    with End_of_file -> List.rev checksats in
  get_checksats_command [] [([],[],[])]
			      
let check_file file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  begin try
    let _ = get_script lexbuf in ()
  with Parsing.Parse_error ->
    let (s, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error (s, l, c)) end;

  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  begin try
    let _ = get_checksats lexbuf in ()
  with Parsing.Parse_error ->
    let (s, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error (s, l, c)) end;
  
  fprintf stdout "OK"
  
let () =
  try
    Arg.parse argspec check_file umsg;
  with
  | Error.Lexer_error (s, l, c) -> 
     Error.print_location_error l c (sprintf "Unexpected character '%s'"s)
  | Error.Parser_error (s, l, c) -> 
     Error.print_location_error l c (sprintf "Unexpected token '%s'"s)
  | Error.Script_error (l, c) ->
     Error.print_location_error l c "Unexpected command"
  | Error.Logic_error (s, l, c) ->
     Error.print_location_error l c s
