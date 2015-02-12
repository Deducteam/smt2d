exception Script_error

module Abs = Abstract

(* parses the whole file *)
let get_script lexbuf = 
  let rec add_command () script =
    try
      let command = Abs.mk_command (Parser.command Lexer.token lexbuf) in
      add_command () (command :: script)
    with End_of_file -> script in
  add_command () []

(* finds the first set-logic command *)
let get_logic_name lexbuf =
  let rec get_logic_name_command () =
    let command = Abs.mk_command (Parser.command Lexer.token lexbuf) in
    match command with
    | Abs.Set_logic s -> s
    | Abs.Set_option _
    | Abs.Set_info _
    | Abs.Declare_sort _
    | Abs.Define_sort _
    | Abs.Declare_fun _
    | Abs.Define_fun _
    | Abs.Push _
    | Abs.Pop _
    | Abs.Assert _
    | Abs.Check_sat	    
    | Abs.Get_assertions  
    | Abs.Get_value _
    | Abs.Get_assignment  
    | Abs.Get_proof	    
    | Abs.Get_unsat_core  
    | Abs.Get_info _
    | Abs.Get_option _
    | Abs.Exit -> get_logic_name_command () in
  try get_logic_name_command () with End_of_file -> raise Script_error

(* raises an error if an in-line definition is found *)
let rec add_in_line_definitions_core signature def_bindings core =
  match core with
  | Abs.True
  | Abs.False -> []
  | Abs.Not t -> 
     add_in_line_definitions signature def_bindings t
  | Abs.Imply (t1, t2)
  | Abs.And (t1, t2)
  | Abs.Or (t1, t2)
  | Abs.Xor (t1, t2)
  | Abs.Equal (t1, t2)
  | Abs.Distinct (t1, t2) ->
     List.fold_left (add_in_line_definitions signature) def_bindings [t1; t2]	  
  | Abs.Ite (t1, t2, t3) ->
     List.fold_left (add_in_line_definitions signature) def_bindings [t1; t2; t3]

and add_in_line_definitions signature def_bindings term =
  match term with
  | Abs.Var _ -> def_bindings
  | Abs.App (_, _, terms) ->
     List.fold_left (add_in_line_definitions signature) def_bindings terms
  | Abs.Core core ->
     add_in_line_definitions_core signature def_bindings core
  | Abs.Let (_, term)
  | Abs.Forall (_, term)
  | Abs.Exists (_, term) ->
     add_in_line_definitions signature def_bindings term 
  | Abs.Attributed (term, attributes) ->
     let attribute_names, _ = List.split attributes in
     if List.mem ":named" attribute_names
     then raise Error.Not_implemented
     else add_in_line_definitions signature def_bindings term

(* returns the list of contexts (signature + assertions) 
   corresponding to each check-sat command of the script *)
let get_context lexbuf =
  let _ = get_logic_name lexbuf in
  (* stack: current assertion-set stack - (sort bindings, fun bindings, assertions) list *)
  let stack = Set_stack.create Signature.empty in
  let rec get_context_command () =
    try
      let command = Abs.mk_command (Parser.command Lexer.token lexbuf) in
      begin 
	match command with
	| Abs.Check_sat -> Set_stack.all stack
	| Abs.Push n -> 
	   Set_stack.push stack n;
	   get_context_command ()
	| Abs.Pop n -> 
	   Set_stack.pop stack n;
	   get_context_command ()
	| Abs.Declare_sort (sort_sym, n) ->
	   Set_stack.add_sort stack sort_sym (Signature.Sort_declaration n);
	   get_context_command ()
	| Abs.Define_sort (sort_sym, pars, par_sort) ->
	   Set_stack.add_sort stack sort_sym (Signature.Sort_definition (pars, par_sort));
	   get_context_command ()
	| Abs.Declare_fun (fun_sym, sorts, sort) ->
	   Set_stack.add_fun 
	     stack fun_sym (Signature.Fun_declaration (sorts, sort));
	   get_context_command ()
	| Abs.Define_fun (fun_sym, sorted_vars, sort, term) ->
	   let signature, _ = Set_stack.all stack in
	   let def_bindings = add_in_line_definitions signature [] term in
	   List.iter 
	     (fun (fun_sym, fun_data) -> Set_stack.add_fun stack fun_sym fun_data) 
	     (List.rev def_bindings);
	   Set_stack.add_fun
	     stack fun_sym (Signature.Fun_definition (sorted_vars, sort, term));
	   get_context_command ()
	| Abs.Assert term ->
	   let signature, _ = Set_stack.all stack in
	   let def_bindings = add_in_line_definitions signature [] term in
	   List.iter 
	     (fun (fun_sym, fun_data) -> Set_stack.add_fun stack fun_sym fun_data) 
	     (List.rev def_bindings);
	   Set_stack.add_assertion stack term;
	   get_context_command ()
	| Abs.Get_value terms ->
	   let signature, _ = Set_stack.all stack in
	   let def_bindings = List.fold_left (add_in_line_definitions signature) [] terms in
	   List.iter 
	     (fun (fun_sym, fun_data) -> Set_stack.add_fun stack fun_sym fun_data) 
	     (List.rev def_bindings);
	   get_context_command ()
	| Abs.Set_logic _
	| Abs.Set_option _ 
	| Abs.Set_info _ 
	| Abs.Get_assertions 
	| Abs.Get_assignment 
	| Abs.Get_proof
	| Abs.Get_unsat_core 
	| Abs.Get_info _ 
	| Abs.Get_option _ 
	| Abs.Exit ->
	   get_context_command () end
    with End_of_file -> raise Script_error in
  get_context_command ()
