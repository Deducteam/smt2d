{
  open Parser
  open Lexing
}

rule token = parse
  | eof                 { EOF }
  | [' ' '\t' '\r']+    { token lexbuf }
  | '\n'                { new_line lexbuf; token lexbuf }
  | ';' (_ # '\n')*     { token lexbuf }
  | '_'                 { UNDERSCORE }
  | "as"                { AS }
  | "let"               { LET }
  | "forall"            { FORALL }
  | "exists"            { EXISTS }
  | '!'                 { ATTRIBUTE }
  | "par"
        { let (s, l, c) = Error.get_location lexbuf in raise (Error.LexerError (s, l, c) ) }
  | "NUMERAL"
        { let (s, l, c) = Error.get_location lexbuf in raise (Error.LexerError (s, l, c) ) }
  | "DECIMAL"
        { let (s, l, c) = Error.get_location lexbuf in raise (Error.LexerError (s, l, c) ) }
  | "STRING"
        { let (s, l, c) = Error.get_location lexbuf in raise (Error.LexerError (s, l, c) ) }
  | "set-logic"         { SET_LOGIC }
  | "set-option"        { SET_OPTION }
  | "set-info"          { SET_INFO }
  | "declare-sort"      { DECLARE_SORT }
  | "define-sort"       { DEFINE_SORT }
  | "declare-fun"       { DECLARE_FUN }
  | "define-fun"        { DEFINE_FUN }
  | "push"              { PUSH }
  | "pop"               { POP }
  | "assert"            { ASSERT }
  | "check-sat"         { CHECK_SAT }
  | "get-assertions"    { GET_ASSERTIONS }
  | "get-proof"         { GET_PROOF }
  | "get-unsat-core"    { GET_UNSAT_CORE }
  | "get-value"         { GET_VALUE }
  | "get-assignment"    { GET_ASSIGNMENT }
  | "get-option"        { GET_OPTION }
  | "get-info"          { GET_INFO }
  | "exit"              { EXIT }
  | '('                 { OPEN }
  | ')'                 { CLOSE }
  | ('0' | ['1'-'9'] ['0'-'9']*) as s
        { NUMERAL s }
  | ('0' | ['1'-'9'] ['0'-'9']*) '.' ['0'-'9']+ as s
        { DECIMAL s }
  | '#' 'x' ['0'-'9' 'A'-'F' 'a'-'f']+ as s 
        { HEXADECIMAL s }
  | '#' 'b' ['0' '1']+ as s
        { BINARY s }
  | '"' (([' '-'~' '\t' '\r' '\n'] # ['\\' '"']) | ('\\' [' '-'~' '\t' '\r' '\n']))* '"' as s
        { STRING s }
  | ['a'-'z' 'A'-'Z' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@'] ['0'-'9' 'a'-'z' 'A'-'Z' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']* as s
        { SYMBOL s }
  | '|' (([' '-'~' '\t' '\r' '\n'] # ['\\' '|'])* as s) '|'
        { SYMBOL s }
  | ':' ['0'-'9' 'a'-'z' 'A'-'Z' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']+ as s
        { KEYWORD s }
  | _
        { let (s, l, c) = Error.get_location lexbuf in raise (Error.LexerError (s, l, c) ) }
      
