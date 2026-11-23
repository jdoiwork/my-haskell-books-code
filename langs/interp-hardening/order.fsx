// F#: like OCaml, a definition sees only the ones above it, and
// functions that call each other are let rec ... and.
// F#：OCaml と同じく、見えるのは上の定義だけ。おたがいを呼ぶ関数は
// let rec ... and

// ANCHOR: order
// F#
let rec isEven n = if n = 0 then true else isOdd (n - 1)
and isOdd n = if n = 0 then false else isEven (n - 1)

printfn "%b %b" (isEven 10) (isOdd 7)
// ANCHOR_END: order

if not (isEven 10 && isOdd 7) then failwith "wrong"
