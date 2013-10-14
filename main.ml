open Truth


let print_table_header (bindings, _) =
  Evaluate.Binding.iter (fun k v -> Printf.printf "%s " k) bindings;
  print_endline " ="

let format_boolean = function
  | true -> "t"
  | false -> "f"

let print_binding k v =
  let name_length = String.length k in
  let str = String.make name_length ' ' in
  str.[name_length / 2] <- (format_boolean v).[0];
  Printf.printf "%s " str

let print_table_row (bindings, result) =
  Evaluate.Binding.iter print_binding bindings;
  Printf.printf " %s\n" (format_boolean result)

let print_table table =
  print_table_header (List.hd table);
  List.iter print_table_row table

let display_table str =
  let expr = Expression.parse_expr str in
  let table = Evaluate.compute_table expr in
  print_table table

let display_dnf str =
  let expr = Expression.parse_expr str in
  let expr = Normalize.convert_expr_to_dnf expr in
  print_endline (Expression.format_expr expr)

let usage_string = "truth: manipulate boolean expressions

Usage:
    truth table <expr>  print truth table for <expr>
    truth dnf <expr>    print disjuctive normal form of <expr>

Syntax:
    <variable> = [a-z A-Z][a-z A-Z 0-9]*
    <operator> = \"implies\" | \"xor\" | \"and\" | \"or\"
    <atom> = <variable> | \"not\" <atom> | \"(\" <expr> \")\"
    <expr> = <atom> <operator> <expr> | <atom>
"

let display_help () =
  print_string usage_string
  

let _ =
  match Sys.argv with
    | [| _; "table"; str |] -> display_table str
    | [| _; "dnf"; str |] -> display_dnf str
    | [| _; "--help" |] -> display_help ()
    | _ -> display_help ()
