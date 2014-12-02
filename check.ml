open Printf

module Script = Concrete

let umsg = "Usage: check <file>"

let argspec = []

(* parse the whole file *)
let parse lexbuf = 
  try
    let rec parse_command () = 
      let _ = Parser.command Lexer.token lexbuf in
      parse_command () in
    parse_command ()
  with 
  | Error.End_of_file -> ()
  | Parsing.Parse_error -> 
     let (s, l, c) = Error.get_location lexbuf in
     raise (Error.Parser_error (s, l, c))

(* finds the first set-logic command and checks that no forbiden command is used before *)
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
      | Script.Set_logic _ -> () 
      | _ ->
	 let (_, l, c) = Error.get_location lexbuf in
	 raise (Error.Script_error (l, c)) in
    get_logic_command ()
  with
  | Error.End_of_file ->
     let (_, l, c) = Error.get_location lexbuf in
     raise (Error.Parser_error ("End-of-file", l, c))
  | Parsing.Parse_error -> 
     let (s, l, c) = Error.get_location lexbuf in
     raise (Error.Parser_error (s, l, c))

(* (\* check that there exists a unique set-logic command and no forbiden command before *\) *)
(* let get_logic lexbuf = *)
(*   try  *)
(*     let rec check_unique_set_logic () = *)
(*       try  *)
(* 	let command = Parser.command Lexer.token lexbuf in *)
(* 	match command with  *)
(* 	| Set_logic _ ->  *)
(* 	| _ -> check_unique_set_logic *)
(*       with Error.EndOfFile -> () *)
(*     let get_logic_command () = *)
(*       let command = Parser.command Lexer.token lexbuf in *)
(*       match command with *)
(*       | Set_option _ | Set_info _ | Get_option _ | Get_info _ -> get_logic_command () *)
(*       | Set_logic ->  *)
(*       | _ ->  *)
(*   with *)
(*   | Parsing.Parse_error ->  *)
(*      let (s, l, c) = Error.get_location lexbuf in *)
(*      raise (Error.ParserError (s, l, c)) *)
	   
let check_file file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  parse lexbuf;
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  get_logic lexbuf;
  (* check_unique_logic; *)
  close_in chan;
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
