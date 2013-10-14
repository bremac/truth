open Expression


let rec convert_expr_to_nnf_rec negated expr =
  match (expr, negated) with
    | (VariableExpr _, true) -> NotExpr expr
    | (VariableExpr _, false) -> expr
    | (NotExpr subexpr, _) -> convert_expr_to_nnf_rec (not negated) subexpr
    | (AndExpr (a_expr, b_expr), true) ->
        OrExpr (convert_expr_to_nnf_rec negated a_expr,
                convert_expr_to_nnf_rec negated b_expr)
    | (AndExpr (a_expr, b_expr), false) ->
        AndExpr (convert_expr_to_nnf_rec negated a_expr,
                 convert_expr_to_nnf_rec negated b_expr)
    | (OrExpr (a_expr, b_expr), true) ->
        AndExpr (convert_expr_to_nnf_rec negated a_expr,
                 convert_expr_to_nnf_rec negated b_expr)
    | (OrExpr (a_expr, b_expr), false) ->
        OrExpr (convert_expr_to_nnf_rec negated a_expr,
                convert_expr_to_nnf_rec negated b_expr)

let convert_expr_to_nnf expr =
  convert_expr_to_nnf_rec false expr

let rec is_in_dnf = function
  | VariableExpr _ -> true
  | NotExpr (VariableExpr _) -> true
  | NotExpr _ -> false
  | AndExpr (OrExpr _, _) -> false
  | AndExpr (_, OrExpr _) -> false
  | AndExpr (a_expr, b_expr) -> is_in_dnf a_expr && is_in_dnf b_expr
  | OrExpr (a_expr, b_expr) -> is_in_dnf a_expr && is_in_dnf b_expr

let rec convert_expr_to_dnf_rec expr =
  let nnf_expr = convert_expr_to_nnf expr in
  match nnf_expr with
    | VariableExpr v -> nnf_expr
    | NotExpr v -> nnf_expr
    | OrExpr (a_expr, b_expr) ->
        OrExpr (convert_expr_to_dnf_rec a_expr,
                convert_expr_to_dnf_rec b_expr)
    | AndExpr (a_expr, b_expr) ->
        let a_expr = convert_expr_to_dnf_rec a_expr in
        let b_expr = convert_expr_to_dnf_rec b_expr in
        match (a_expr, b_expr) with
          | (OrExpr (a, b), OrExpr (c, d)) ->
              OrExpr
                (OrExpr (AndExpr (a, c), AndExpr (b, c)),
                 OrExpr (AndExpr (a, d), AndExpr (b, d)))
          | (a, OrExpr (b, c)) ->
              OrExpr (AndExpr (a, b), AndExpr (a, c))
          | (OrExpr (b, c), a) ->
              OrExpr (AndExpr (a, b), AndExpr (a, c))
          | _ -> AndExpr (a_expr, b_expr)

let rec convert_expr_to_dnf before_expr =
  let after_expr = convert_expr_to_dnf_rec before_expr in
  if before_expr = after_expr
  then before_expr
  else convert_expr_to_dnf after_expr
