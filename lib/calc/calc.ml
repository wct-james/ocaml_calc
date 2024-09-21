let calculate input =
  let tokens = Lexer.lex input in
  let ast, _ = Parser.parse_expr tokens in

  Eval.eval ast
