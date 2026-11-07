// The type chart of elements, for the chapter "A map of languages".
// Run it with `dotnet fsi elements.fsx`.
// 属性の相性。「言語の地図」の章。`dotnet fsi elements.fsx` で動く

// ANCHOR: elements
// F#
type Element = Fire | Water | Grass

let beats attacker defender =
    match attacker, defender with
    | Fire, Grass | Water, Fire | Grass, Water -> true
    | _ -> false

let power attacker defender =
    if beats attacker defender then 200
    elif beats defender attacker then 50
    else 100
// ANCHOR_END: elements

let check ok = if not ok then failwith "wrong result"
check (power Fire Grass = 200)
check (power Grass Fire = 50)
check (power Water Water = 100)
printfn "ok"
