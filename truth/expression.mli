type expression_t =
  | VariableExpr of string
  | NotExpr of expression_t
  | AndExpr of expression_t * expression_t
  | OrExpr of expression_t * expression_t

type expression_type_t = VariableExprT | NotExprT | AndExprT | OrExprT

val expr_type : expression_t -> expression_type_t

val parse_expr : string -> expression_t

val format_expr : expression_t -> string
