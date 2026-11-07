(* The type chart of elements, for the chapter "A map of languages".
   属性の相性。「言語の地図」の章 *)

(* ANCHOR: elements *)
(* OCaml *)
type element = Fire | Water | Grass

let beats attacker defender =
  match attacker, defender with
  | Fire, Grass | Water, Fire | Grass, Water -> true
  | _ -> false

let power attacker defender =
  if beats attacker defender then 200
  else if beats defender attacker then 50
  else 100
(* ANCHOR_END: elements *)

let () =
  assert (power Fire Grass = 200);
  assert (power Grass Fire = 50);
  assert (power Water Water = 100);
  print_endline "ok"
