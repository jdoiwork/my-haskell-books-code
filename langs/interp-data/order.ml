(* The OCaml code in the chapter on data and case. *)
(* data と case の章の OCaml のコード *)

(* ANCHOR: order *)
(* OCaml: compare looks at the values while running.
   A constructor without arguments comes first. *)
(* OCaml：compare は、動かしながら値を見る。引数のない構成子が先 *)
type t = B of int | A

let () = print_int (compare A (B 0))   (* -1: A is smaller *)
(* ANCHOR_END: order *)

let () = assert (compare A (B 0) < 0)
let () = assert (compare (B 1) (B 2) < 0)
