-- The type chart of elements, for the chapter "A map of languages".
-- `lean elements.lean` checks the theorem and the #guard lines.
-- 属性の相性。「言語の地図」の章。
-- `lean elements.lean` で、定理と #guard の行を確かめる

-- ANCHOR: elements
-- Lean 4
inductive Element where
  | fire | water | grass
  deriving Repr, DecidableEq

def beats : Element → Element → Bool
  | .fire,  .grass => true
  | .water, .fire  => true
  | .grass, .water => true
  | _,      _      => false

-- No two elements beat each other both ways.
-- おたがいに強い組はない
theorem not_both (a d : Element) :
    ¬ (beats a d = true ∧ beats d a = true) := by
  cases a <;> cases d <;> decide
-- ANCHOR_END: elements

#guard beats .fire .grass
#guard !beats .grass .fire
#guard !beats .water .water
