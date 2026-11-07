(* Effects as requests, for the chapter "Effects".
   頼みごととしてのエフェクト。「エフェクト」の章 *)

(* ANCHOR: effects *)
(* OCaml 5.3 *)
type _ Effect.t += Roll : int -> int Effect.t

let attack () =
  let hit = Effect.perform (Roll 20) in
  if hit >= 10 then begin
    let damage = Effect.perform (Roll 6) in
    Printf.printf "hit for %d\n" damage;
    damage
  end else begin
    print_endline "missed";
    0
  end

let run_scripted (rolls : int list) program =
  let rolls = ref rolls in
  match program () with
  | result -> result
  | effect (Roll _), k ->
      let r = match !rolls with r :: rest -> rolls := rest; r | [] -> 1 in
      Effect.Deep.continue k r
(* ANCHOR_END: effects *)

let () =
  assert (run_scripted [15; 4] attack = 4);
  assert (run_scripted [3] attack = 0);
  print_endline "ok"
