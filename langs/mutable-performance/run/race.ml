(* The sieve race: how long does a sieve up to n take? (the best of 3)
   ふるいの競走：n までのふるいに、どれだけかかる？（3回のうちの最速）
     ocamlfind is not needed:
     ocamlopt -O3 -I +unix unix.cmxa race.ml -o race && ./race 10000000 *)

let sieve n =
  let is_prime = Array.make (n + 1) true in
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  let p = ref 2 in
  while !p * !p <= n do
    if is_prime.(!p) then begin
      let m = ref (!p * !p) in
      while !m <= n do
        is_prime.(!m) <- false;
        m := !m + !p
      done
    end;
    incr p
  done;
  Array.fold_left (fun c ok -> if ok then c + 1 else c) 0 is_prime

let () =
  let n = int_of_string Sys.argv.(1) in
  let best = ref infinity and count = ref 0 in
  for _ = 1 to 3 do
    let start = Unix.gettimeofday () in
    count := sieve n;
    best := Float.min !best ((Unix.gettimeofday () -. start) *. 1000.)
  done;
  Printf.printf "%d %.1f\n" !count !best
