exception Script_error

(* parses the whole file *)
val get_script: Lexing.lexbuf -> Abstract.script

val get_context: Lexing.lexbuf -> Signature.signature * Abstract.term list
