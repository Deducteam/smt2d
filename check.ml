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
  let modname = Translate.tr_string (Filename.chop_extension (Filename.basename file)) in
  let prelude = Dedukti.prelude modname in
  let signature, assertions = apply_script_processing Process_script.get_unique_context file in
  let sort_context = Translate.tr_sort_context signature in
  let fun_context = Translate.tr_fun_context signature in
  let assertion_bindings = 
    List.mapi 
      (fun i term ->
       term, Dedukti.var (("I_"^(string_of_int (i+1))))) assertions in
  let inputs = Translate.tr_assertion_bindings signature assertion_bindings in
  Dedukti.print_line stdout prelude;
  List.iter (Dedukti.print_line stdout) sort_context;
  List.iter (Dedukti.print_line stdout) fun_context;
  List.iter (Dedukti.print_line stdout) inputs

let () =
  Arg.parse argspec (fun f -> files := f :: !files) umsg;
  List.iter check_file !files
