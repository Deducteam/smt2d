open Printf
 
let umsg = "Usage: check <files>"

let files = ref []

let argspec = []

let check_get_script file = 
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let _ = Run_script.get_script lexbuf in ()
	  
let check_get_contexts file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let _ = Run_script.get_contexts lexbuf in ()

let check_get_contexts_sorts file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let contexts = Run_script.get_contexts lexbuf in
  let _ =
    List.map
      (fun (signature, assertions) ->
       List.map (Get_sort.get_sort signature) assertions)
      contexts in ()
					      
let check_file file = 
  fprintf stdout "checking file %s: " file;
  check_get_script file;
  check_get_contexts file; (* à enlever *)
  check_get_contexts_sorts file;
  fprintf stdout "OK\n"
  
let () =
  Arg.parse argspec (fun f -> files := f :: !files) umsg;
  List.iter check_file !files;
