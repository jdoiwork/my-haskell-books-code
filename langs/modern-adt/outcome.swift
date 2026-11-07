// The outcome of a quest, for the chapter "ADTs are everywhere".
// 依頼の結果。「代数的データ型は、どこにでもある」の章

// ANCHOR: enum
// Swift (enum with associated values)
// Swift（関連値つきの enum）
enum Outcome {
    case cleared(gold: Int)
    case failed(place: String)
    case fled
}

func describe(_ outcome: Outcome) -> String {
    switch outcome {
    case .cleared(let gold):  return "cleared! +\(gold) G"
    case .failed(let place):  return "failed at \(place)"
    case .fled:               return "ran away"
    }
}
// ANCHOR_END: enum

precondition(describe(.cleared(gold: 50)) == "cleared! +50 G")
precondition(describe(.failed(place: "cave")) == "failed at cave")
precondition(describe(.fled) == "ran away")
print("ok")
