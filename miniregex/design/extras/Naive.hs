-- ANCHOR: all
-- | The first tree that comes to mind. It can hold every pattern, and
--   also many shapes that are not patterns at all.
-- まず思いつく木。どのパターンも入るが、パターンでない形も、たくさん入る
module Naive where

-- ANCHOR: naive
-- 🌱 A pattern, the simple way.
-- 素朴に書いたパターン
data Regex
  = Lit Char                      -- a
  | AnyChar                       -- .
  | Seq [Regex]                   -- abc
  | Alt [Regex]                   -- a|b|c
  | Repeat Int (Maybe Int) Regex  -- a{2,5}  a{2,}
  | Group Regex                   -- ( ... )
  | LineStart                     -- ^
  | LineEnd                       -- $
  deriving Show
-- ANCHOR_END: naive

-- ANCHOR: odd
-- 🙀 Trees that no pattern makes. PCRE2 refuses all of them.
-- どのパターンからも作られない木。PCRE2 は、どれも断る
noBranch, fiveToTwo, minusOne, repeatedStart, starStar :: Regex
-- an | with no branch at all / 枝が1本もない選択
noBranch      = Alt []
-- a{5,2}
fiveToTwo     = Repeat 5 (Just 2) (Lit 'a')
-- a{-1,}
minusOne      = Repeat (-1) Nothing (Lit 'a')
-- ^*
repeatedStart = Repeat 0 Nothing LineStart
-- a**
starStar      = Repeat 0 Nothing (Repeat 0 Nothing (Lit 'a'))
-- ANCHOR_END: odd
-- ANCHOR_END: all
