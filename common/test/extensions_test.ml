open OUnit2

open Common.Extensions

module StrMap = Map.Make(String)

let suite =
  "Extensions suite">::: [
    "List.filter_not">::
      (fun _ ->
        assert_equal [2; 3] (List.filter_not (fun n -> n > 3) [2; 3; 4;])
      );

    "List.as_pairs">::
      (fun _ ->
        assert_equal [(1, 2); (3, 4)] (List.as_pairs [1; 2; 3; 4])
      );

    "List.zip">::
      (fun _ ->
        assert_equal [(1, 4); (2, 5); (3, 6)] (List.zip [1; 2; 3] [4; 5; 6])
      );

    "List.inspect">::
      (fun _ ->
        assert_equal "[1; 2; 3]" (List.inspect string_of_int [1; 2; 3;])
      );

    "Map.find_else when key exists">::
      (fun _ ->
        let map = StrMap.add "key" "value" StrMap.empty in
        assert_equal "value" (StrMap.find_else "key" "else" map)
      );

    "Map.find_else on else condition">::
      (fun _ ->
        assert_equal "else" (StrMap.find_else "key" "else" StrMap.empty)
      );

    "String.is_empty with empty string">::
      (fun _ ->
        assert_equal true (String.is_empty "")
      );

    "String.is_empty with blank string">::
      (fun _ ->
        assert_equal false (String.is_empty "   ")
      );

    "String.is_empty with non-blank string">::
      (fun _ ->
        assert_equal false (String.is_empty " hi ")
      );

    "String.is_blank with empty string">::
      (fun _ ->
        assert_equal true (String.is_blank "")
      );

    "String.is_blank with empty string">::
      (fun _ ->
        assert_equal true (String.is_blank "  ")
      );

    "String.is_blank with empty string">::
      (fun _ ->
        assert_equal false (String.is_blank " hi ")
      );

    "String.sub_from from first position">::
      (fun _ ->
        assert_equal "hello" (String.sub_from "hello" 0)
      );

    "String.sub_from from non-first position">::
      (fun _ ->
        assert_equal "sthis" (String.sub_from "whatisthis" 5)
      );

    "String.to_chars">::
      (fun _ ->
        assert_equal ['a'; 'b'; 'c'] (String.to_chars "abc")
      );

    "String.from_chars">::
      (fun _ ->
        assert_equal "abc" (String.from_chars ['a'; 'b'; 'c'])
      );
  ]
