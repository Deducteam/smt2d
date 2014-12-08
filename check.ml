open Printf
 
let umsg = "Usage: check <files>"

let files = ref []

let argspec = []

let check_get_script file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  try
    let _ = Run.get_script lexbuf in ()
  with Parsing.Parse_error ->
    let (s, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error (s, l, c))
	  
let check_get_logic_name file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  try
    let _ = Run.get_logic_name lexbuf in ()
  with Parsing.Parse_error ->
    let (s, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error (s, l, c))

let check_get_contexts file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  try
    let _ = Run.get_contexts lexbuf in ()
  with Parsing.Parse_error ->
    let (s, l, c) = Error.get_location lexbuf in
    raise (Error.Parser_error (s, l, c))

let check_file file = 
  fprintf stdout "checking file %s: " file;
  check_get_script file;
  check_get_logic_name file;
  check_get_contexts file;
  fprintf stdout "OK\n"
  
let () =
  try
    Arg.parse argspec (fun f -> files := f :: !files) umsg;
    List.iter check_file !files;
  with
  | Error.Lexer_error (s, l, c) -> 
     Error.print_location_error l c (sprintf "Unexpected character '%s'"s)
  | Error.Parser_error (s, l, c) -> 
     Error.print_location_error l c (sprintf "Unexpected token '%s'"s)
  | Error.Script_error (l, c) ->
     Error.print_location_error l c "Unexpected command"
  | Error.Logic_error (s, l, c) ->
     Error.print_location_error l c s
