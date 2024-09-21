let test_simple_addition () =
  let maths = "1 + 2" in
  let answer = Calc.calculate maths in
  Alcotest.check (Alcotest.float 0.1) "1 + 2 = 3" 3.0 answer

let test_simple_subtraction () =
  let maths = "5 - 2" in
  let answer = Calc.calculate maths in
  Alcotest.check (Alcotest.float 0.1) "5 - 2 = 3" 3.0 answer

let test_simple_multiplication () =
  let maths = "3 * 4" in
  let answer = Calc.calculate maths in
  Alcotest.check (Alcotest.float 0.1) "3 * 4 = 12" 12.0 answer

let test_simple_division () =
  let maths = "10 / 2" in
  let answer = Calc.calculate maths in
  Alcotest.check (Alcotest.float 0.1) "10 / 2 = 5" 5.0 answer

let test_power () =
  let maths = "2 ^ 3" in
  let answer = Calc.calculate maths in
  Alcotest.check (Alcotest.float 0.1) "2 ^ 3 = 8" 8.0 answer

let test_brackets () =
  let maths = "(1 + 2) * 3" in
  let answer = Calc.calculate maths in
  Alcotest.check (Alcotest.float 0.1) "(1 + 2) * 3 = 9" 9.0 answer

let test_complex_expression () =
  let maths = "3 + 5 * (2 ^ 3) - 4 / 2" in
  let answer = Calc.calculate maths in
  Alcotest.check (Alcotest.float 0.1) "complex expression" 41.0 answer

let () =
  let open Alcotest in
  run "Calculations"
    [
      ( "simple operations",
        [
          test_case "Addition" `Quick test_simple_addition;
          test_case "Subtraction" `Quick test_simple_subtraction;
          test_case "Multiplication" `Quick test_simple_multiplication;
          test_case "Division" `Quick test_simple_division;
        ] );
      ( "advanced operations",
        [
          test_case "Power" `Quick test_power;
          test_case "Brackets" `Quick test_brackets;
          test_case "Complex expression" `Quick test_complex_expression;
        ] );
    ]
