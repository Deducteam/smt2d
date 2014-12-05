open Printf
 
let umsg = "Usage: check <file>"

let argspec = []
let check_file file = 
  Context.check_parse file;
  Context.check_context file;  
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
