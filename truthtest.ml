open OUnit
open Truth

let print_variables variables =
  String.concat "; " (Variables.elements variables)

let variables_of_list lst =
  List.fold_left (fun vs v -> Variables.add v vs) Variables.empty lst

let assert_variables_equal expected real =
  assert_equal ~cmp:Variables.equal ~printer:print_variables expected real

let int_of_binding binding =
  let sorted_pairs = Bindings.bindings binding in
  (* Make sure that we don't overflow the integer *)
  assert (List.length sorted_pairs <= 31);
  List.fold_left (fun integer (_, value) ->
    (integer lsl 1) lor (if value then 1 else 0)
  ) 0 sorted_pairs

module IntSet = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = int
  end
)

let int_set_of_list lst =
  List.fold_left (fun vs v -> IntSet.add v vs) IntSet.empty lst


(* Variable extraction *)

let extract_variables_for_string str =
  let expr = parse_expr str in
  extract_variables expr

let assert_expr_variables_equal str lst =
  assert_variables_equal
    (variables_of_list lst)
    (extract_variables_for_string str)

let test_extract_variables _ =
  assert_expr_variables_equal "a" ["a"];
  assert_expr_variables_equal "a and b" ["a"; "b"];
  assert_expr_variables_equal "a or not a" ["a"];
  assert_expr_variables_equal "(aa or ac) xor (ac or d)" ["aa"; "ac"; "d"]


(* Computing all bindings *)

let all_bindings variables =
  map_bindings variables (fun x -> x)

let variables_of_binding binding =
  let elements = Bindings.bindings binding in
  let variable_names = List.map (fun (k, _) -> k) elements in
  variables_of_list variable_names

(* There should be 2**n bindings, where n is the number of variables *)
let assert_expected_binding_count variable_list bindings =
  let expected_count = 2.0 ** float_of_int (List.length variable_list) in
  let actual_count = float_of_int (List.length bindings) in
  assert_equal expected_count actual_count

let assert_bindings_are_distinct bindings =
  let ints = List.map int_of_binding bindings in
  let int_set = int_set_of_list ints in
  let unique_ints = IntSet.elements int_set in
  assert_equal (List.length bindings) (List.length unique_ints)
  

let assert_bindings_covered variable_list =
  let variables = variables_of_list variable_list in
  let bindings = all_bindings variable_list in
  List.iter (fun binding ->
    assert_variables_equal (variables_of_binding binding) variables
  ) bindings;
  assert_expected_binding_count variable_list bindings;
  assert_bindings_are_distinct bindings

let test_map_bindings _ =
  assert_bindings_covered [];
  assert_bindings_covered ["a"];
  assert_bindings_covered ["a"; "b"];
  assert_bindings_covered ["c"; "a"; "longer"]


(* Conversion to DNF *)

let rec is_in_dnf = function
  | VariableExpr _ -> true
  | NotExpr (VariableExpr _) -> true
  | NotExpr _ -> false
  | AndExpr (OrExpr _, _) -> false
  | AndExpr (_, OrExpr _) -> false
  | AndExpr (a_expr, b_expr) -> is_in_dnf a_expr && is_in_dnf b_expr
  | OrExpr (a_expr, b_expr) -> is_in_dnf a_expr && is_in_dnf b_expr

let compute_sorted_table a_expr =
  let table = compute_table a_expr in
  let table_with_ints = List.map (fun (binding, v) ->
    (int_of_binding binding, v)
  ) table
  in
  List.sort (fun (i, _) (j, _) -> i - j) table_with_ints

let assert_converts_to_dnf str =
  let base_expr = parse_expr str in
  let dnf_expr = convert_expr_to_dnf base_expr in
  let sorted_table_base = compute_sorted_table base_expr in
  let sorted_table_dnf = compute_sorted_table dnf_expr in

  assert_variables_equal
    (extract_variables base_expr)
    (extract_variables dnf_expr);

  assert_bool "is_in_dnf" (is_in_dnf dnf_expr);

  (* Make sure the expressions are logically equivalent *)
  assert_equal sorted_table_base sorted_table_dnf

let test_convert_to_dnf _ =
  assert_converts_to_dnf "a";
  assert_converts_to_dnf "a and (b or c)";
  assert_converts_to_dnf "a and b and c and (d or not e or f) and f";
  assert_converts_to_dnf "not (a xor b xor (c or d and not e))"

let suite = "Truth" >::: [
  "test_extract_variables" >:: test_extract_variables;
  "test_map_bindings" >:: test_map_bindings;
  "test_convert_to_dnf" >:: test_convert_to_dnf
]

let _ = 
  run_test_tt suite
