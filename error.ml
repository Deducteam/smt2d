open Printf
open Lexing

let get_location lexbuf =
  let start = lexeme_start_p lexbuf in 
  (lexeme lexbuf, start.pos_lnum, 
   (start.pos_cnum - start.pos_bol))

let print_location_error l c str = 
  eprintf "Line %d Col %d: %s\n" l c str; exit 1

exception Lexer_error of string * int * int
exception Parser_error of string * int * int
exception Script_error of int * int
exception Logic_error of string * int * int
