val get_script: Lexing.lexbuf -> Concrete.script

val get_logic_signature: Lexing.lexbuf -> Abstract.signature

val get_contexts: Lexing.lexbuf -> Abstract.assertion_set list
