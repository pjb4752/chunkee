open OUnit2

let () = run_test_tt_main Source_parsing_test.suite
let () = run_test_tt_main Semantic_parsing_test.suite
let () = run_test_tt_main Identifier_resolution_test.suite
