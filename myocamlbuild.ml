open Ocamlbuild_plugin
open Ocamlbuild_pack

let bisect_path = "/home/bremac/.opam/4.01.0/lib/bisect/"

let () =
  dispatch begin function
    | After_rules ->
        flag ["bisect"; "pp"]
          (S [A"camlp4o"; A"str.cma"; A(bisect_path ^ "bisect_pp.cmo")]);
        flag ["bisect"; "compile"]
          (S [A"-I"; A(bisect_path)]);
        flag ["bisect"; "link"; "byte"]
          (S [A"-I"; A(bisect_path); A"bisect.cma"]);
        flag ["bisect"; "link"; "native"]
          (S [A"-I"; A(bisect_path); A"bisect.cmxa"]);
        flag ["bisect"; "link"; "java"]
          (S [A"-I"; A(bisect_path); A"bisect.cmja"])
    | _ -> ()
end
