// The F# code in the chapter on data and case.
// data と case の章の F# のコード

// ANCHOR: auto
// F#: a union can be compared when its insides can.
//     The case written first is the smaller one.
// F#：中身が比べられれば、共用体も比べられる。先に書いたものが小さい
type Shape =
    | Circle of int
    | Rect of int * int

printfn "%b" (Circle 5 < Rect (1, 1))   // true
printfn "%b" (Some 1 = Some 1)          // true

// type Handler = Button of (int -> int)
// Button id = Button id   // error FS0001: does not support 'equality'
// ANCHOR_END: auto

if not (Circle 5 < Rect (1, 1)) then failwith "order"
if not (Rect (1, 2) < Rect (1, 3)) then failwith "arguments"
if not (None < Some 0) then failwith "option"
