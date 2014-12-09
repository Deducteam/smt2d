let option_map f opt =
  match opt with
  | None -> None
  | Some a -> Some (f a)
