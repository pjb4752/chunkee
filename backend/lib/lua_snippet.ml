open Printf

type expr = string
type unit_stmt = string
type result_stmt = { var: string; stmt: string }

type t =
  | Expr of t list * expr
  | UnitStmt of t list * unit_stmt
  | ResultStmt of t list * result_stmt

let make_expr expr = Expr ([], expr)

let make_unit_stmt stmt = UnitStmt ([], stmt)

let make_result_stmt var stmt = ResultStmt ([], { var; stmt })

let insert_preamble snippet p =
  match snippet with
  | Expr (ps, expr) -> Expr (p :: ps, expr)
  | UnitStmt (ps, stmt) -> UnitStmt (p :: ps, stmt)
  | ResultStmt (ps, stmt) -> ResultStmt (p :: ps, stmt)

let is_expr = function
  | Expr _ -> true
  | UnitStmt _ -> false
  | ResultStmt _ -> false

let is_stmt snippet = not (is_expr snippet)

let preamble_string snippet =
  let rec preamble_string' strings = function
    | Expr (snippets, _) ->
        List.fold_left preamble_string' strings snippets
    | UnitStmt (snippets, _) ->
        List.fold_left preamble_string' strings snippets
    | ResultStmt (snippets, { stmt; _ }) ->
        List.fold_left preamble_string' (stmt :: strings) snippets in
  String.concat "\n" (preamble_string' [] snippet)

let result_expr = function
  | Expr (_, expr) -> expr
  | ResultStmt (_, { var; _ }) -> var
  | UnitStmt _ -> assert false

let result_string target preamble snippet =
  let result = result_expr snippet in
  if preamble = "" then sprintf "%s %s" target result
  else if target = "" then sprintf "%s" preamble
  else sprintf "%s\n%s %s" preamble target result

let stmt_string preamble stmt =
  if preamble = "" then sprintf "%s" stmt
  else sprintf "%s\n%s" preamble stmt

let lua_string ?target:(target="return") snippet =
  let preamble = preamble_string snippet in
  match snippet with
  | Expr _ | ResultStmt _ -> result_string target preamble snippet
  | UnitStmt (_, stmt) -> stmt_string preamble stmt

let rec to_string snippet =
  let sprintf_snippets snippets =
    List.map to_string snippets |> (String.concat ", ") in
  match snippet with
  | Expr (snippets, expr) ->
      let snippets = sprintf_snippets snippets in
      sprintf "Expr ([%s], %s)" snippets expr
  | UnitStmt (snippets, stmt) ->
      let snippets = sprintf_snippets snippets in
      sprintf "UnitStmt ([%s], %s)" snippets stmt
  | ResultStmt (snippets, { var; stmt }) ->
      let snippets = sprintf_snippets snippets in
      sprintf "ResultStmt ([%s], { var: %s, stmt: %s })" snippets var stmt

