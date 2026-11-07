// The type chart of elements, for the chapter "A map of languages".
// 属性の相性。「言語の地図」の章

// ANCHOR: elements
// Gleam
pub type Element {
  Fire
  Water
  Grass
}

pub fn beats(attacker: Element, defender: Element) -> Bool {
  case attacker, defender {
    Fire, Grass | Water, Fire | Grass, Water -> True
    _, _ -> False
  }
}

pub fn power(attacker: Element, defender: Element) -> Int {
  case beats(attacker, defender), beats(defender, attacker) {
    True, _ -> 200
    _, True -> 50
    _, _ -> 100
  }
}
// ANCHOR_END: elements

// Fails (and stops) when a result is wrong.
// 結果がちがうと、ここで止まる
pub fn main() -> Nil {
  let assert 200 = power(Fire, Grass)
  let assert 50 = power(Grass, Fire)
  let assert 100 = power(Water, Water)
  Nil
}
