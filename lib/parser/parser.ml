open Token

type expr =
  | Binary of expr * token * expr
  | Unary of token * expr
  | Literal of token

let dump_tokens tokens =
  List.iter
    (fun x ->
      x |> dump_token |> print_string;
      print_string " ")
    tokens;
  print_newline ()

let parse_expr tokens =
  let rec parse_literal tokens =
    match tokens with
    | Number n :: rest -> (Literal (Number n), rest)
    | Minus :: rest ->
        let right, rest = parse_literal rest in
        (Unary (Minus, right), rest)
    | _ -> failwith "failed on parse_literal"
  and parse_factor tokens =
    let left, tokens = parse_literal tokens in
    match tokens with
    | Multiply :: rest ->
        let right, rest = parse_factor rest in
        (Binary (left, Multiply, right), rest)
    | Divide :: rest ->
        let right, rest = parse_factor rest in
        (Binary (left, Divide, right), rest)
    | _ -> (left, tokens)
  and parse_sum tokens =
    let left, tokens = parse_factor tokens in
    match tokens with
    | Add :: rest ->
        let right, rest = parse_sum rest in
        (Binary (left, Add, right), rest)
    | Minus :: rest ->
        let right, rest = parse_sum rest in
        (Binary (left, Minus, right), rest)
    | _ -> (left, tokens)
  in

  parse_sum tokens

let rec dump_ast ast =
  match ast with
  | Binary (l, t, r) ->
      dump_ast l;
      t |> dump_token |> print_string;
      dump_ast r;
      print_newline ()
  | Unary (t, r) ->
      t |> dump_token |> print_string;
      dump_ast r;
      print_newline ()
  | Literal t -> t |> dump_token |> print_string
