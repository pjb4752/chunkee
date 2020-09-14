open OUnit2

module Read_list = Common.Read_list

let suite =
  "Read_list suite">::: [
    "from_string of single-line string">:: (fun _ ->
      let read_list = Read_list.from_string "cat" in
      assert_equal read_list [
        { value = 'c'; line_num = 1; char_num = 1 };
        { value = 'a'; line_num = 1; char_num = 2 };
        { value = 't'; line_num = 1; char_num = 3 };
      ]
    );

    "from_string of multi-line string">:: (fun _ ->
      let read_list = Read_list.from_string "cat\npow" in
      assert_equal read_list [
        { value = 'c';  line_num = 1; char_num = 1 };
        { value = 'a';  line_num = 1; char_num = 2 };
        { value = 't';  line_num = 1; char_num = 3 };
        { value = '\n'; line_num = 1; char_num = 4 };
        { value = 'p';  line_num = 2; char_num = 1 };
        { value = 'o';  line_num = 2; char_num = 2 };
        { value = 'w';  line_num = 2; char_num = 3 };
      ]
    );
  ]
