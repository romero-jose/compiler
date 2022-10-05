open Compiler.Expr
open Alcotest

let interp_tests =
  let open Compiler.Interp in
  let value : value testable = testable pp_value ( = ) in
  let expect (expected : value) (input : expr) =
    (check value) "same value" expected (interp [] input)
  in
  let test_nat () = expect (NatV 42) (NatE 42) in
  let test_bool_true () = expect (BoolV true) (BoolE true) in
  let test_bool_false () = expect (BoolV false) (BoolE false) in
  let test_let () =
    expect (NatV 2) (LetE ("x", NatE 42, Op2E (Minus, VarE "x", NatE 40)))
  in
  let test_op1_not () = expect (BoolV false) (Op1E (Not, BoolE true)) in
  let test_op2_plus () = expect (NatV 22) (Op2E (Plus, NatE 4, NatE 18)) in
  let test_op2_minus () =
    expect (NatV 5) (Op2E (Minus, Op2E (Plus, NatE 5, NatE 3), NatE 3))
  in
  let test_op2_or () =
    expect (BoolV true) (Op2E (Or, BoolE false, BoolE true))
  in

  ( "interp",
    [
      test_case "a nat" `Quick test_nat;
      test_case "a bool that is true" `Quick test_bool_true;
      test_case "a bool that is false" `Quick test_bool_false;
      test_case "a let expression" `Quick test_let;
      test_case "a not expression" `Quick test_op1_not;
      test_case "a plus expression" `Quick test_op2_plus;
      test_case "a minus expression" `Quick test_op2_minus;
      test_case "a or expression" `Quick test_op2_or;
    ] )

let parse_tests =
  let open Compiler.Parse in
  let expr : expr testable = testable pp_expr ( = ) in
  let expect (expected : expr) (input : string) =
    (check expr) "same expr" expected (parse_string input)
  in
  let test_nat () = expect (NatE 42) "42" in
  let test_bool_true () = expect (BoolE true) "true" in
  let test_bool_false () = expect (BoolE false) "false" in
  let test_var () = expect (VarE "some-var") "some-var" in
  let test_let () =
    expect
      (LetE ("x", NatE 42, Op2E (Minus, VarE "x", NatE 40)))
      "(let (x 42) (- x 40))"
  in
  let test_op1_not () = expect (Op1E (Not, BoolE true)) "(not true)" in
  let test_op2_plus () = expect (Op2E (Plus, NatE 4, NatE 18)) "(+ 4 18)" in
  let test_op2_minus () =
    expect (Op2E (Minus, Op2E (Plus, NatE 5, NatE 3), NatE 3)) "(- (+ 5 3) 3)"
  in
  let test_op2_or () =
    expect (Op2E (Or, BoolE false, BoolE true)) "(or false true)"
  in
  let test_invalid_sum () =
    expect (Op2E (Plus, NatE 1, BoolE false)) "(+ 1 false)"
  in
  ( "parse",
    [
      test_case "a nat" `Quick test_nat;
      test_case "a bool that is true" `Quick test_bool_true;
      test_case "a bool that is false" `Quick test_bool_false;
      test_case "a variable" `Quick test_var;
      test_case "a let expression" `Quick test_let;
      test_case "a not expression" `Quick test_op1_not;
      test_case "a plus expression" `Quick test_op2_plus;
      test_case "a minus expression" `Quick test_op2_minus;
      test_case "a or expression" `Quick test_op2_or;
      test_case "an invalid sum" `Quick test_invalid_sum;
    ] )

let typecheck_tests =
  let open Compiler.Typecheck in
  let pp_t_opt : t option Fmt.t =
   fun fmt t -> match t with Some v -> pp_t fmt v | None -> Fmt.string fmt ""
  in
  let t_opt : t option testable = testable pp_t_opt ( = ) in
  let expect (expected : t option) (e : expr) =
    (check t_opt) "same value" expected (typecheck [] e)
  in
  let test_nat () = expect (Some NatT) (NatE 42) in
  let test_bool_true () = expect (Some BoolT) (BoolE true) in
  let test_bool_false () = expect (Some BoolT) (BoolE false) in
  let test_let () =
    expect (Some NatT) (LetE ("x", NatE 42, Op2E (Minus, VarE "x", NatE 40)))
  in
  let test_op1_not () = expect (Some BoolT) (Op1E (Not, BoolE true)) in
  let test_op2_plus () = expect (Some NatT) (Op2E (Plus, NatE 4, NatE 18)) in
  let test_op2_minus () =
    expect (Some NatT) (Op2E (Minus, Op2E (Plus, NatE 5, NatE 3), NatE 3))
  in
  let test_op2_or () =
    expect (Some BoolT) (Op2E (Or, BoolE false, BoolE true))
  in
  let test_invalid_sum () = expect None (Op2E (Plus, NatE 1, BoolE false)) in

  ( "typecheck",
    [
      test_case "a nat" `Quick test_nat;
      test_case "a bool that is true" `Quick test_bool_true;
      test_case "a bool that is false" `Quick test_bool_false;
      test_case "a let expression" `Quick test_let;
      test_case "a not expression" `Quick test_op1_not;
      test_case "a plus expression" `Quick test_op2_plus;
      test_case "a minus expression" `Quick test_op2_minus;
      test_case "a or expression" `Quick test_op2_or;
      test_case "an invalid sum" `Quick test_invalid_sum;
    ] )

let () = run "Compiler" [ interp_tests; typecheck_tests; parse_tests ]
