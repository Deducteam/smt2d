exception Check_error

let umsg = "Usage: check <files>"

let files = ref []

let argspec = []

let apply_script_processing proc file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  proc lexbuf

(* get context - ie signature + assertions - from a .smt2 script, translate them and print them 
 in a .dk file *)
let check_file file =
  match apply_script_processing Process_script.get_contexts file with
  | [signature, assertions] -> Translate.print_context stdout signature assertions file
  | _ -> raise Check_error

let () =
  Arg.parse argspec (fun f -> files := f :: !files) umsg;
  List.iter check_file !files
