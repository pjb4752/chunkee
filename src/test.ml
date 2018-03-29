open OUnit2

let () = run_test_tt_main Read_test.suite
let () = run_test_tt_main Analyze_test.suite
let () = run_test_tt_main Tree_test.suite
