open Printf

type expression = string
type unit_statement = string
type result_statement = { variable: string; statement: string }

type t =
  | Expression of t list * expression
  | UnitStatement of t list * unit_statement
  | ResultStatement of t list * result_statement

let make_expression expression = Expression ([], expression)

let make_unit_statement statement = UnitStatement ([], statement)

let make_result_statement variable statement = ResultStatement ([], { variable; statement })

let insert_preamble snippet preamble =
  match snippet with
  | Expression (snippets, expression) -> Expression (preamble :: snippets, expression)
  | UnitStatement (snippets, statement) -> UnitStatement (preamble :: snippets, statement)
  | ResultStatement (snippets, statement) -> ResultStatement (preamble :: snippets, statement)

let is_expression = function
  | Expression _ -> true
  | UnitStatement _ -> false
  | ResultStatement _ -> false

let is_statement snippet = not (is_expression snippet)

let preamble_string snippet =
  let rec preamble_string' strings = function
    | Expression (snippets, _) ->
        List.fold_left preamble_string' strings snippets
    | UnitStatement (snippets, _) ->
        List.fold_left preamble_string' strings snippets
    | ResultStatement (snippets, { statement; _ }) ->
        List.fold_left preamble_string' (statement :: strings) snippets in
  String.concat "\n" (preamble_string' [] snippet)

let result_expression = function
  | Expression (_, expression) -> expression
  | ResultStatement (_, { variable; _ }) -> variable
  | UnitStatement _ -> assert false

let result_string target preamble snippet =
  let result = result_expression snippet in
  if preamble = "" then sprintf "%s %s" target result
  else if target = "" then sprintf "%s" preamble
  else sprintf "%s\n%s %s" preamble target result

let statement_string preamble statement =
  if preamble = "" then sprintf "%s" statement
  else sprintf "%s\n%s" preamble statement

let lua_string ?target:(target="return") snippet =
  let preamble = preamble_string snippet in
  match snippet with
  | Expression _ | ResultStatement _ -> result_string target preamble snippet
  | UnitStatement (_, statement) -> statement_string preamble statement

let rec inspect snippet =
  let sprintf_snippets snippets =
    List.map inspect snippets |> (String.concat ", ") in
  match snippet with
  | Expression (snippets, expression) ->
      let snippets = sprintf_snippets snippets in
      sprintf "Expression ([%s], %s)" snippets expression
  | UnitStatement (snippets, statement) ->
      let snippets = sprintf_snippets snippets in
      sprintf "UnitStatement ([%s], %s)" snippets statement
  | ResultStatement (snippets, { variable; statement }) ->
      let snippets = sprintf_snippets snippets in
      sprintf "ResultStatement ([%s], { variable: %s, statement: %s })" snippets variable statement

