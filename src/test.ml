open OUnit2

let () = run_test_tt_main Emit_test.suite
let () = run_test_tt_main Lex_test.suite
let () = run_test_tt_main Parse_test.suite
let () = run_test_tt_main Resolve_test.suite
let () = run_test_tt_main Table_test.suite
