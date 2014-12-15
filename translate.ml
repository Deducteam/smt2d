(* Translate smtlib2 to dedukti *)

(* any special caracter c becomes 
     "_"^(string_of_int (int_of_char c))^"_"
   and "_"^(string_of_char c) with c in ['0' - '9'] becomes "_0"^(string_of_char c) *)
let ident s = 
  let buf = Buffer.create (2*String.length s) in
  let escape = ref false in
  String.iter
    (fun c ->
     match c with
     | 'a'..'z' | 'A'..'Z' -> Buffer.add_char buf c
     | '0'..'9' ->
	if !escape
	then 
	  (Buffer.add_char buf '0'; Buffer.add_char buf c; escape := false) 
	else 
	  Buffer.add_char buf c
     | '_' -> 
	escape := true; Buffer.add_char buf c
     | _ -> 
	Buffer.add_string 
	  buf ("_"^(string_of_int (int_of_char c))^"_")) s;
  Buffer.contents buf
    
let prelude file = 
  let name =
    ident (Filename.chop_extension (Filename.basename file)) in
  Dedukti.prelude name
