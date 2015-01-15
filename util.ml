exception Util_error

let option_map f opt =
  match opt with
  | None -> None
  | Some a -> Some (f a)

let separate_last l =
  match List.rev l with
  | t :: ts -> List.rev ts, t
  | [] -> raise Util_error
