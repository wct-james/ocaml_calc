open Token

let lex str =
  let is_space ch = ch = ' ' || ch = '\n' || ch = '\t' in

  let rec aux acc chars =
    match chars with
    | [] -> List.rev acc
    | c :: rest when is_space c -> aux acc rest
    | '+' :: rest -> aux (Add :: acc) rest
    | '-' :: rest -> aux (Minus :: acc) rest
    | '*' :: rest -> aux (Multiply :: acc) rest
    | '/' :: rest -> aux (Divide :: acc) rest
    | c :: rest when (c >= '0' && c <= '9') || c = '.' ->
        let num_str, remaining = take_number (c :: rest) in
        aux (Number (float_of_string num_str) :: acc) remaining
    | _ -> failwith "unexpected character"
  and take_number chars =
    let rec aux num_str chars =
      match chars with
      | [] -> (num_str, chars)
      | c :: rest when (c >= '0' && c <= '9') || c = '.' ->
          aux (num_str ^ String.make 1 c) rest
      | _ -> (num_str, chars)
    in
    aux "" chars
  in
  aux [] (List.init (String.length str) (String.get str))
