// The type chart of elements, for the chapter "A map of languages".
// 属性の相性。「言語の地図」の章

// ANCHOR: elements
// Scala 3
enum Element:
  case Fire, Water, Grass

import Element.*

def beats(attacker: Element, defender: Element): Boolean =
  (attacker, defender) match
    case (Fire, Grass) | (Water, Fire) | (Grass, Water) => true
    case _                                              => false
// ANCHOR_END: elements

@main def check(): Unit =
  assert(beats(Fire, Grass))
  assert(!beats(Grass, Fire))
  assert(!beats(Water, Water))
  println("ok")
