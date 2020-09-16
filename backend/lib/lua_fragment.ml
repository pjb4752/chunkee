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

let insert_preamble fragment preamble =
  match fragment with
  | Expression (fragments, expression) -> Expression (preamble :: fragments, expression)
  | UnitStatement (fragments, statement) -> UnitStatement (preamble :: fragments, statement)
  | ResultStatement (fragments, statement) -> ResultStatement (preamble :: fragments, statement)

let is_expression = function
  | Expression _ -> true
  | UnitStatement _ -> false
  | ResultStatement _ -> false

let is_statement fragment = not (is_expression fragment)

let preamble_string fragment =
  let rec preamble_string' strings = function
    | Expression (fragments, _) ->
        List.fold_left preamble_string' strings fragments
    | UnitStatement (fragments, _) ->
        List.fold_left preamble_string' strings fragments
    | ResultStatement (fragments, { statement; _ }) ->
        List.fold_left preamble_string' (statement :: strings) fragments in
  String.concat "\n" (preamble_string' [] fragment)

let result_expression = function
  | Expression (_, expression) -> expression
  | ResultStatement (_, { variable; _ }) -> variable
  | UnitStatement _ -> assert false

let result_string target preamble fragment =
  let result = result_expression fragment in
  if preamble = "" then sprintf "%s %s" target result
  else if target = "" then sprintf "%s" preamble
  else sprintf "%s\n%s %s" preamble target result

let statement_string preamble statement =
  if preamble = "" then sprintf "%s" statement
  else sprintf "%s\n%s" preamble statement

let lua_string ?target:(target="return") fragment =
  let preamble = preamble_string fragment in
  match fragment with
  | Expression _ | ResultStatement _ -> result_string target preamble fragment
  | UnitStatement (_, statement) -> statement_string preamble statement

let rec inspect fragment =
  let sprintf_fragments fragments =
    List.map inspect fragments |> (String.concat ", ") in
  match fragment with
  | Expression (fragments, expression) ->
      let fragments = sprintf_fragments fragments in
      sprintf "Expression ([%s], %s)" fragments expression
  | UnitStatement (fragments, statement) ->
      let fragments = sprintf_fragments fragments in
      sprintf "UnitStatement ([%s], %s)" fragments statement
  | ResultStatement (fragments, { variable; statement }) ->
      let fragments = sprintf_fragments fragments in
      sprintf "ResultStatement ([%s], { variable: %s, statement: %s })" fragments variable statement

