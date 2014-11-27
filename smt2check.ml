open Printf

let umsg = "Usage: smt2check <file>"

let argspec = []

let parse_and_check out lexbuf = 
  try
    let rec parse_command = 
      let _ = Parser.command Lexer.token lexbuf in
      parse_command in
    parse_command
  with 
  | Error.EndOfFile -> fprintf out "OK"
  | Parsing.Parse_error -> 
     let (s, l, c) = Error.get_location lexbuf in
     raise (Error.ParserError (s, l, c))

let check_file file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let out = stdout in
  parse_and_check out lexbuf
     
let () =
  try
    Arg.parse argspec check_file umsg;
  with
  | Error.LexerError (s, l, c) -> 
     Error.print_location_error l c (sprintf "Unexpected character '%s'"s)
  | Error.ParserError (s, l, c) -> 
     Error.print_location_error l c (sprintf "Unexpected token '%s'"s)
