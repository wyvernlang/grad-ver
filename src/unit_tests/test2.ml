open OUnit

let suite =
  "Test 2" >::: [
    "assert true"  >:: (fun () -> assert_bool "true"  true);
    "assert false" >:: (fun () -> assert_bool "false" false);
  ]
