open Printf

module Script = Concrete

let umsg = "Usage: check <file>"

let argspec = []

(* parse the whole file *)
let get_script lexbuf = 
  let rec add_command () script =
    try
      let command = Parser.command Lexer.token lexbuf in
      add_command () (command :: script)
    with Error.End_of_file -> script in
  add_command () []

(* finds the first set-logic command and checks that no forbiden command is used before *)
let get_logic_signature lexbuf =
  try 
    let rec get_logic_signature_command () =
      let command = Parser.command Lexer.token lexbuf in
      match command with
      | Script.Set_option _
      | Script.Set_info _
      | Script.Get_option _
      | Script.Get_info _
        -> get_logic_signature_command ()
      | Script.Set_logic s ->
	 if s <> "QF_UF"
	 then
	   let (_, l, c) = Error.get_location lexbuf in
	   raise (Error.Logic_error (sprintf "Unrecognized logic%s" s, l, c))
	 else Signature.init Logic.Qf_uf
      | _ ->
	 let (_, l, c) = Error.get_location lexbuf in
	 raise (Error.Script_error (l, c)) in
    get_logic_signature_command ()
  with Error.End_of_file ->
    let (_, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error ("End-of-file", l, c))

(* to be used after parsing the first set-logic command
   - returns the list of assertion sets corresponding to each check-sat command of the script
   - checks that there is no alternative set-logic command *)
let get_checksats_sets lexbuf =
    (* sets corresponds to previous check-sat commands
       stack corresponds to the current assertion-set stack *)
    let rec get_checksats_sets_command sets stack =
      try
	let rec push n stack =
	  match n with
	  | 0 -> stack
	  | _ -> push (n-1) ([]::stack) in
	let rec pop n stack =
	  match n, stack with
	  | 0, _ -> stack
	  | _, current :: previous :: old ->
	     pop (n-1) (previous :: old)
	  | _, _ ->
	     let (_, l, c) = Error.get_location lexbuf in
	     raise (Error.Script_error (l, c)) in
	let command = Parser.command Lexer.token lexbuf in
	match command with
	| Script.Push num ->
	   let n = int_of_string num in	   
	   get_checksats_sets_command sets (push n stack)
	| Script.Pop num ->
	   let n = int_of_string num in
	   get_checksats_sets_command sets (pop n stack)
	| Script.Declare_sort _
	| Script.Define_sort _
	| Script.Declare_fun _
	| Script.Define_fun _
	| Script.Assert _ ->
	   begin match stack with
	   | current :: old -> 
	      get_checksats_sets_command
		sets ((command :: current) :: old)
	   | [] -> assert false
		end
	| Script.Check_sat ->
	   get_checksats_sets_command
	     (sets@[List.flatten stack]) stack
	| Script.Set_logic _ ->
	   let (_, l, c) = Error.get_location lexbuf in
	   raise (Error.Logic_error ("Forbidden alternative set_logic command", l, c))
	| _ ->
	   get_checksats_sets_command sets stack
      with Error.End_of_file -> sets in
    get_checksats_sets_command [] [[]]

let get_signature lexbuf =
  let signature = get_logic_signature lexbuf in
  let _ = get_checksats_sets lexbuf in
  signature
			      
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
    let _ = get_signature lexbuf in ()
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
