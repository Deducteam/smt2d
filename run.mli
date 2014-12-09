val get_script: Lexing.lexbuf -> Abstract.script

val get_logic_name: Lexing.lexbuf -> Abstract.logic_name

val get_contexts: Lexing.lexbuf ->
		  (Abstract.signature * Abstract.term list) list
