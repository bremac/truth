open Syntax


type expression_t =
  | VariableExpr of string
  | NotExpr of expression_t
  | AndExpr of expression_t * expression_t
  | OrExpr of expression_t * expression_t

type expression_type_t = VariableExprT | NotExprT | AndExprT | OrExprT


let expr_type = function
  | VariableExpr _ -> VariableExprT
  | NotExpr _ -> NotExprT
  | AndExpr _ -> AndExprT
  | OrExpr _ -> OrExprT

let rec desugar = function
  | VariableSyntax v -> VariableExpr v
  | NotSyntax subexpr -> NotExpr (desugar subexpr)
  | AndSyntax (a_expr, b_expr) -> AndExpr (desugar a_expr, desugar b_expr)
  | OrSyntax (a_expr, b_expr) -> OrExpr (desugar a_expr, desugar b_expr)
  | ImpliesSyntax (a_expr, b_expr) ->
      OrExpr (NotExpr (desugar a_expr), desugar b_expr)
  | XorSyntax (a_expr, b_expr) ->
      let desugared_a = desugar a_expr in
      let desugared_b = desugar b_expr in
      AndExpr (OrExpr (desugared_a, desugared_b),
                      (NotExpr (AndExpr (desugared_a, desugared_b))))

let parse_expr str =
  let buffer = Lexing.from_string str in
  let syntax = Parser.main Lexer.token buffer in
  desugar syntax

let rec format_expr expr =
  let outer_type = expr_type expr in
  String.concat "" (
    match expr with
      | VariableExpr v -> [v]
      | NotExpr subexpr -> ["not "; format_expr_with_parens subexpr outer_type]
      | AndExpr (a_expr, b_expr) ->
          [format_expr_with_parens a_expr outer_type; " and ";
           format_expr_with_parens b_expr outer_type]
      | OrExpr (a_expr, b_expr) ->
          [format_expr_with_parens a_expr outer_type; " or ";
           format_expr_with_parens b_expr outer_type]
  )
and format_expr_with_parens expr outer_type =
  let formatted_expr = format_expr expr in
  match expr with
    | VariableExpr _ -> formatted_expr
    | _ ->
        if (expr_type expr) != outer_type
        then String.concat "" ["("; formatted_expr; ")"]
        else formatted_expr
