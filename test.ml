open OUnit
open Truth.Expression
open Truth.Evaluate
open Truth.Normalize


let print_variables variables =
  String.concat "; " (Variable.elements variables)

let variables_of_list lst =
  List.fold_left (fun vs v -> Variable.add v vs) Variable.empty lst

let assert_variables_equal expected real =
  assert_equal ~cmp:Variable.equal ~printer:print_variables expected real

let int_of_binding binding =
  let sorted_pairs = Binding.bindings binding in
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

let sorted_table_with_ints table =
  let table_with_ints = List.map (fun (binding, v) ->
    (int_of_binding binding, v)
  ) table
  in
  List.sort (fun (i, _) (j, _) -> i - j) table_with_ints


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
  let elements = Binding.bindings binding in
  let variable_names = List.map (fun (k, _) -> k) elements in
  variables_of_list variable_names

(* There should be 2**n bindings, where n is the number of variables *)
let assert_expected_binding_count variable_list bindings =
  let expected_count = 2.0 ** float_of_int (List.length variable_list) in
  let actual_count = float_of_int (List.length bindings) in
  assert_equal expected_count actual_count

let assert_bindings_are_distinct bindings =
  let ints = List.map int_of_binding bindings in
  let unique_ints = int_set_of_list ints |> IntSet.elements in
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


(* Expression Formatting *)
let assert_can_format_expr str =
  let base_expr = parse_expr str in
  let reparsed_expr = format_expr base_expr |> parse_expr in
  assert_equal base_expr reparsed_expr

let test_format_expr _ =
  assert_can_format_expr "a";
  assert_can_format_expr "not a";
  assert_can_format_expr "a or b";
  assert_can_format_expr "a or not b";
  assert_can_format_expr "a and b";
  assert_can_format_expr "a and b and c";
  assert_can_format_expr "a and b or c and d";
  assert_can_format_expr "a xor b";
  assert_can_format_expr "a implies b";
  assert_can_format_expr "a and foo or c11 and drummer"


(* DNF criteria checking *)
let assert_in_dnf expected str =
  let expr = parse_expr str in
  assert_equal expected (is_in_dnf expr)

let test_is_in_dnf _ =
  assert_in_dnf true "a";
  assert_in_dnf true "not a";
  assert_in_dnf true "a or b";
  assert_in_dnf true "a or not b";
  assert_in_dnf true "a and b";
  assert_in_dnf false "not (a and b)";
  assert_in_dnf false "not (a or b)";
  assert_in_dnf false "a and b or c";
  assert_in_dnf false "(a or b) and c"


(* Conversion to DNF *)

let compute_sorted_table a_expr =
  compute_table a_expr |> sorted_table_with_ints

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


(* Evaluate truth table *)
let binding_of_list binding_list =
  List.fold_left
    (fun binding (k, v) -> Binding.add k v binding)
    Binding.empty
    binding_list

let table_of_pairs pairs =
  List.map (fun (binding_list, v) -> (binding_of_list binding_list, v)) pairs

let assert_computed_table_equal str expected =
  let expr = parse_expr str in
  let actual_table = compute_table expr |> sorted_table_with_ints in
  let expected_table = table_of_pairs expected |> sorted_table_with_ints in
  assert_equal expected_table actual_table

let test_compute_table _ =
  assert_computed_table_equal "a" [
    ([("a", true)], true);
    ([("a", false)], false)
  ];
  assert_computed_table_equal "not a" [
    ([("a", true)], false);
    ([("a", false)], true)
  ];
  assert_computed_table_equal "a and b" [
    ([("a", true); ("b", true)], true);
    ([("a", true); ("b", false)], false);
    ([("a", false); ("b", true)], false);
    ([("a", false); ("b", false)], false)
  ];
  assert_computed_table_equal "a or b" [
    ([("a", true); ("b", true)], true);
    ([("a", true); ("b", false)], true);
    ([("a", false); ("b", true)], true);
    ([("a", false); ("b", false)], false)
  ];
  assert_computed_table_equal "a xor b" [
    ([("a", true); ("b", true)], false);
    ([("a", true); ("b", false)], true);
    ([("a", false); ("b", true)], true);
    ([("a", false); ("b", false)], false)
  ];
  assert_computed_table_equal "a implies b" [
    ([("a", true); ("b", true)], true);
    ([("a", true); ("b", false)], false);
    ([("a", false); ("b", true)], true);
    ([("a", false); ("b", false)], true)
  ]


let suite = "Truth" >::: [
  "test_extract_variables" >:: test_extract_variables;
  "test_map_bindings" >:: test_map_bindings;
  "test_format_expr" >:: test_format_expr;
  "test_is_in_dnf" >:: test_is_in_dnf;
  "test_convert_to_dnf" >:: test_convert_to_dnf;
  "test_compute_table" >:: test_compute_table
]

let _ = 
  run_test_tt suite
