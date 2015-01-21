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
  let prelude = Translate.tr_prelude file in
  match apply_script_processing Process_script.get_contexts file with
  | [signature, assertions] ->
     let sort_context = Translate.tr_sort_context signature in
     let fun_context = Translate.tr_fun_context signature in
     let propositions = Translate.tr_assertions signature assertions in
     Dedukti.print_line stdout prelude;
     List.iter (Dedukti.print_line stdout) sort_context;
     List.iter (Dedukti.print_line stdout) fun_context;
     List.iter (Dedukti.print_line stdout) propositions
  | _ -> raise Check_error

let () =
  Arg.parse argspec (fun f -> files := f :: !files) umsg;
  List.iter check_file !files
