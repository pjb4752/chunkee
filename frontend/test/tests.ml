open OUnit2

let () = run_test_tt_main Lexing_test.suite
let () = run_test_tt_main Parsing_test.suite
