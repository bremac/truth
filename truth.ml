(* *)

open Syntax

type expression_t =
  | VariableExpr of string
  | NotExpr of expression_t
  | AndExpr of expression_t * expression_t
  | OrExpr of expression_t * expression_t

module Bindings = Map.Make(String)
module Variables = Set.Make(String)


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

let rec extract_variables_rec expr variables =
  match expr with
    | VariableExpr v -> Variables.add v variables
    | NotExpr subexpr -> extract_variables_rec subexpr variables
    | AndExpr (a_expr, b_expr) -> extract_variables2 a_expr b_expr variables
    | OrExpr (a_expr, b_expr) -> extract_variables2 a_expr b_expr variables
and extract_variables2 a_expr b_expr variables =
  let variables = extract_variables_rec a_expr variables in
  extract_variables_rec b_expr variables

let extract_variables expr =
  extract_variables_rec expr Variables.empty

let rec evaluate expr bindings =
  match expr with
    | VariableExpr v -> Bindings.find v bindings
    | NotExpr subexpr -> not (evaluate subexpr bindings)
    | AndExpr (a_expr, b_expr) ->
        (evaluate a_expr bindings) && (evaluate b_expr bindings)
    | OrExpr (a_expr, b_expr) ->
        (evaluate a_expr bindings) || (evaluate b_expr bindings)

let rec map_bindings_rec bindings variables proc accum =
  match variables with
    | [] -> (proc bindings) :: accum
    | v :: variables ->
        let true_bindings = Bindings.add v true bindings in
        let accum = map_bindings_rec true_bindings variables proc accum in
        let false_bindings = Bindings.add v false bindings in
        map_bindings_rec false_bindings variables proc accum

let map_bindings variables proc =
  map_bindings_rec Bindings.empty variables proc []

let compute_table expr =
  let variable_set = extract_variables expr in
  let variables = Variables.elements variable_set in
  map_bindings variables (fun bindings -> (bindings, evaluate expr bindings))


(* EXPRESSION PARSING *)
let parse_expr str =
  let buffer = Lexing.from_string str in
  let syntax = Parser.main Lexer.token buffer in
  desugar syntax


(* EXPRESSION CONVERSION *)

let rec convert_expr_to_nnf negated expr =
  match (expr, negated) with
    | (VariableExpr v, true) -> NotExpr expr
    | (VariableExpr v, false) -> expr
    | (NotExpr subexpr, _) -> convert_expr_to_nnf (not negated) subexpr
    | (AndExpr (a_expr, b_expr), true) ->
        OrExpr (convert_expr_to_nnf negated a_expr,
                convert_expr_to_nnf negated b_expr)
    | (AndExpr (a_expr, b_expr), false) ->
        AndExpr (convert_expr_to_nnf negated a_expr,
                 convert_expr_to_nnf negated b_expr)
    | (OrExpr (a_expr, b_expr), true) ->
        AndExpr (convert_expr_to_nnf negated a_expr,
                 convert_expr_to_nnf negated b_expr)
    | (OrExpr (a_expr, b_expr), false) ->
        OrExpr (convert_expr_to_nnf negated a_expr,
                convert_expr_to_nnf negated b_expr)

let rec convert_expr_to_dnf_rec expr =
  let nnf_expr = convert_expr_to_nnf false expr in
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


(* EXPRESSION FORMATTING *)

type expression_type_t =
  | VariableExprT
  | NotExprT
  | AndExprT
  | OrExprT

let expr_type = function
  | VariableExpr _ -> VariableExprT
  | NotExpr _ -> NotExprT
  | AndExpr _ -> AndExprT
  | OrExpr _ -> OrExprT

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


(* TRUTH TABLE PRINTING *)

let print_table_header (bindings, _) =
  Bindings.iter (fun k v -> Printf.printf "%s " k) bindings;
  print_newline ()

let format_boolean = function
  | true -> "t"
  | false -> "_"

let print_binding k v =
  let name_length = String.length k in
  let str = String.make name_length ' ' in
  str.[name_length / 2] <- (format_boolean v).[0];
  Printf.printf "%s " str

let print_table_row (bindings, result) =
  Bindings.iter print_binding bindings;
  print_endline (format_boolean result)

let print_table table =
  print_table_header (List.hd table);
  List.iter print_table_row table
