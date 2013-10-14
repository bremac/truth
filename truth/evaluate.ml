open Expression

module Binding = Map.Make(String)
module Variable = Set.Make(String)


let rec extract_variables_rec expr variables =
  match expr with
    | VariableExpr v -> Variable.add v variables
    | NotExpr subexpr -> extract_variables_rec subexpr variables
    | AndExpr (a_expr, b_expr) -> extract_variables2 a_expr b_expr variables
    | OrExpr (a_expr, b_expr) -> extract_variables2 a_expr b_expr variables
and extract_variables2 a_expr b_expr variables =
  let variables = extract_variables_rec a_expr variables in
  extract_variables_rec b_expr variables

let extract_variables expr =
  extract_variables_rec expr Variable.empty

let rec map_bindings_rec bindings variables proc accum =
  match variables with
    | [] -> (proc bindings) :: accum
    | v :: variables ->
        let true_bindings = Binding.add v true bindings in
        let accum = map_bindings_rec true_bindings variables proc accum in
        let false_bindings = Binding.add v false bindings in
        map_bindings_rec false_bindings variables proc accum

let map_bindings variables proc =
  map_bindings_rec Binding.empty variables proc []

let rec evaluate expr bindings =
  match expr with
    | VariableExpr v -> Binding.find v bindings
    | NotExpr subexpr -> not (evaluate subexpr bindings)
    | AndExpr (a_expr, b_expr) ->
        (evaluate a_expr bindings) && (evaluate b_expr bindings)
    | OrExpr (a_expr, b_expr) ->
        (evaluate a_expr bindings) || (evaluate b_expr bindings)

let compute_table expr =
  let variable_set = extract_variables expr in
  let variables = Variable.elements variable_set in
  map_bindings variables (fun bindings -> (bindings, evaluate expr bindings))
