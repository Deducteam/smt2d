open Printf
 
let umsg = "Usage: check <files>"

let files = ref []

let argspec = []

let check_file file = 
  fprintf stdout "checking file %s: " file;
  Context.check_parse file;
  Context.check_context file;
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
