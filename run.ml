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
	  
(* - finds the first set-logic command and checks that no forbiden command is used before
   - checks that no set-logic command is used after
   - returns the list of (env, assertion lists) couples 
     corresponding to each check-sat command of the script *)
let get_contexts lexbuf =
  let logic_signature = Abstract.logic_signature (get_logic_name lexbuf) in
  (* - contexts: (env, assertion list) list corresponding to previous check-sat commands
     - stack: current assertion-set stack - (sort bindings, fun bindings, assertions) list *)
  let rec get_contexts_command contexts stack =
    try
      let command =
	Abstract.command (Parser.command Lexer.token lexbuf) in
      match command with
      | Abstract.Check_sat ->
	 begin match stack with
	 | current :: _ -> get_contexts_command (current :: contexts) stack
	 | [] -> assert false (* the implementation of pop garantees it *) end
      | _ ->
	 get_contexts_command contexts (Abstract.run_command command stack)
    with End_of_file -> List.rev contexts in
  get_contexts_command [] [(logic_signature,[])]
