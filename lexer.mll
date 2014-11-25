{
  open Parser
  open Lexing
} 

let space = [' ' '\t']
let digits = ['0'-'9']+
let symbol = 
  ['a'-'z' 'A'-'Z' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']
  ['0'-'9' 'a'-'z' 'A'-'Z' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']*
 (* symbol from smtlib2 syntax, whithout the | | syntax *) 

rule token = parse
  | space+               { token lexbuf }
  | '\n'                 { new_line lexbuf; token lexbuf}
  | '('                  { OPEN }
  | ')'                  { CLOSE }
  | "let"                { LET }
  | "forall"             { FORALL }
  | "exists"             { EXISTS }
  | '!'                  { BANG }

  | "true"               { TRUE }
  | "false"              { FALSE }
  | "not"                { NOT }
  | "=>"                 { IMPLY }
  | "and"                { AND }
  | "or"                 { OR }
  | "xor"                { XOR }
  | "="                  { EQ }
  | "distinct"           { DISTINCT }
  | "ite"                { ITE }

  | "set-logic"          { SET_LOGIC }
  | "set-option"         { SET_OPTION }
  | "set-info"           { SET_INFO }
  | "declare-sort"       { DECLARE_SORT }
  | "define-sort"        { DEFINE_SORT }
  | "declare-fun"        { DECLARE_FUN }
  | "define-fun"         { DEFINE_FUN }
  | "push"               { PUSH }
  | "pop"                { POP }
  | "assert"             { ASSERT }
  | "check-sat"          { CHECK_SAT }
  | "get-assertions"     { GET_ASSERTIONS }
  | "get-values"         { GET_VALUE }
  | "get-proof"          { GET_PROOF }
  | "get-unsat-core"     { GET_UNSAT_CORE }
  | "get-info"           { GET_INFO }
  | "get-option"         { GET_OPTION }
  | "exit"               { EXIT }

  | symbol as s          { SYM s }
  | eof                  { EOF }
  | _                    { let (s, l, c) = Error.get_location lexbuf in
			   raise (Error.LexerError (s, l, c) ) }
      
