(* The OCaml code in the chapter on strings, tuples and lists. *)
(* 文字列・タプル・リストの章の OCaml のコード *)

(* ANCHOR: compare *)
(* OCaml: = and < work for any type, but not for functions *)
(* OCaml：= と < は、どんな型にも使える。ただし、関数はだめ *)
let () = print_endline (string_of_bool ([1; 2] < [1; 3]))   (* true *)
let () =
  try ignore ((fun x -> x) = (fun x -> x))
  with Invalid_argument msg -> print_endline msg
  (* compare: functional value *)
(* ANCHOR_END: compare *)

let () = assert ([1; 2] < [1; 3])
let () = assert (("ab", 1) < ("b", 0))
let () =
  match (fun x -> x + 1) = (fun x -> x + 1) with
  | _ -> failwith "functions were compared"
  | exception Invalid_argument _ -> ()
