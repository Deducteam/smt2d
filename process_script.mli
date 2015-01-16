exception Script_error

val get_script: Lexing.lexbuf -> Abstract.script

val get_contexts: Lexing.lexbuf -> (Signature.signature * Abstract.term list) list
