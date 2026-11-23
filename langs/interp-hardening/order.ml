(* OCaml: a definition sees only the ones above it. Functions that call
   each other are written together, with let rec ... and.
   OCaml：定義が見えるのは、上の定義だけ。おたがいを呼ぶ関数は、
   let rec ... and で一緒に書く *)

(* ANCHOR: order *)
(* OCaml *)
let rec is_even n = if n = 0 then true else is_odd (n - 1)
and is_odd n = if n = 0 then false else is_even (n - 1)

let () = Printf.printf "%b %b\n" (is_even 10) (is_odd 7)
(* ANCHOR_END: order *)

(* ANCHOR: local *)
(* A local let is general in OCaml, even when it uses tag.
   OCaml の中の let は、tag を使っていても一般 *)
let tagged tag n name =
  let label show x = tag ^ ":" ^ show x in
  label string_of_int n ^ " " ^ label (fun s -> s) name
(* ANCHOR_END: local *)

let () =
  assert (is_even 10 && is_odd 7);
  assert (tagged "#" 3 "lam" = "#:3 #:lam")
