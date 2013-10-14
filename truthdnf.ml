(* *)

let main () =
  let expr = Truth.parse_expr Sys.argv.(1) in
  let expr = Truth.convert_expr_to_dnf expr in
  print_endline (Truth.format_expr expr)
;;

main ()
