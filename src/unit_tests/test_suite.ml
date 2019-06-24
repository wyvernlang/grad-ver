open OUnit2

(* Collect the tests of different modules into one test suite *)
let suite = "Test Suite" >::: [
    Test1.suite;
    Test2.suite
  ]

let _ =
  run_test_tt_main suite
