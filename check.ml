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

(* There must exist a unique check_sat in the script *)
let check_print_context file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  Run_script.print_context file lexbuf

let check_file file =
  (* check_get_script file; *)
  (* check_get_contexts file; *)
  (* check_get_contexts_sorts file; *)
  check_print_context file

let () =
  Arg.parse argspec (fun f -> files := f :: !files) umsg;
  List.iter check_file !files;
