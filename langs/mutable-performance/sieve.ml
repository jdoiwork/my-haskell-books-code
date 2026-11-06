(* The OCaml code in the "rewrite to go faster" chapter.
   書き換えて速くするの章の OCaml のコード *)

(* ANCHOR: array *)
(* OCaml (an array of bools, rewritten in place: OCaml lets you rewrite
   anywhere, no runST needed)
   OCaml（bool の配列を、その場で書き換える。OCaml は runST なしで、
   どこでも書き換えられる） *)
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
  is_prime
(* ANCHOR_END: array *)

let () =
  let primes =
    List.filter (fun i -> (sieve 30).(i)) (List.init 31 Fun.id) in
  assert (primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]);
  let count =
    Array.fold_left (fun c ok -> if ok then c + 1 else c) 0 (sieve 1000000) in
  assert (count = 78498);
  print_endline "ok"
