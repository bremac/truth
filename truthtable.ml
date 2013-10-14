(* *)

let main () =
  let expr = Truth.parse_expr Sys.argv.(1) in
  let table = Truth.compute_table expr in
  Truth.print_table table
;;

main ()
