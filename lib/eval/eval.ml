open Parser
open Token

let rec eval expr =
  match expr with
  | Literal (Number n) -> n
  | Literal _ -> failwith "require literal number expr"
  | Unary (_, r) -> -.eval r
  | Group exp -> eval exp
  | Binary (l, t, r) -> (
      match t with
      | Add -> eval l +. eval r
      | Minus -> eval l -. eval r
      | Multiply -> eval l *. eval r
      | Divide -> eval l /. eval r
      | _ -> failwith "foo")
