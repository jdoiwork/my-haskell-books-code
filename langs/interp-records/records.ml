(* The OCaml code in the chapter on type annotations and records. *)
(* 型の注釈とレコードの章の OCaml のコード *)

(* ANCHOR: records *)
(* OCaml (records: each type has its own field names) *)
(* OCaml（レコード：型ごとに、自分のフィールドの名前を持つ） *)
type member = { name : string; lang : string; days : int }

(* A copy with days changed: the same shape as { m | days = ... } *)
(* days だけ変えた写し：{ m | days = ... } と同じ形 *)
let next_day m = { m with days = m.days + 1 }
(* ANCHOR_END: records *)

(* ANCHOR: objects *)
(* OCaml (objects: .. stands for the other methods, like r) *)
(* OCaml（オブジェクト：.. が、ほかのメソッドを表す。r と同じ役） *)
let label o = "[" ^ o#name ^ "]"
(* val label : < name : string; .. > -> string *)

let ruby = object method name = "Ruby" method version = "4.0" end
let () = print_endline (label ruby)          (* [Ruby] *)
(* ANCHOR_END: objects *)

let lam = { name = "Lam"; lang = "lam"; days = 19 }
let () = assert ((next_day lam).days = 20 && lam.days = 19)
let () = assert (label ruby = "[Ruby]")
let () = assert (label (object method name = "Lam" end) = "[Lam]")
