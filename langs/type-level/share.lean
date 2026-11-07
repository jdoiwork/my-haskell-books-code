-- Sharing gold, for the chapter "Type-level programming".
-- `lean share.lean` checks the theorem and the #guard lines.
-- ゴールドの山分け。「型レベルプログラミング入門」の章。
-- `lean share.lean` で、定理と #guard の行を確かめる

-- ANCHOR: share
-- Lean 4
def share (gold : Nat) (members : {n : Nat // n > 0}) : Nat :=
  gold / members.val

theorem split_total (gold members : Nat) :
    members * (gold / members) + gold % members = gold :=
  Nat.div_add_mod gold members
-- ANCHOR_END: share

#guard share 100 ⟨3, by decide⟩ == 33
