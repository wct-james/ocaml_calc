type token = Number of float | Add | Minus | Multiply | Divide

(* string representation of token *)
let dump_token t =
  match t with
  | Number n -> string_of_float n
  | Add -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Divide -> "/"
