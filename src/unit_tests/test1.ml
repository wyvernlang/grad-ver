open OUnit

let suite =
  "Test 1" >::: [
    "1 = 1" >:: (fun () -> assert_equal 1 1);
    "1 = 2" >:: (fun () -> assert_equal 1 2);
  ]
