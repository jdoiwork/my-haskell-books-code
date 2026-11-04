-- ANCHOR: all
-- | Recursion schemes: write the recursion once, and pass in only what
--   happens at one level.
-- 再帰スキーム：再帰は1回だけ書き、1段で何をするかだけを渡す
module Algo.Schemes
  ( Fix (..)
  , cata
  , ana
  , hylo
  , ListF (..)
  , fromList
  , toList'
  , sumAlg
  , lengthAlg
  , ExprF (..)
  , Expr
  , lit
  , add
  , mul
  , evalAlg
  , showAlg
  , twoTimes
  , TreeF (..)
  , split
  , mergeAlg
  , msortHylo
  , countdown
  , factorial
  ) where

-- ANCHOR: fix
-- 🪢 Tie a one-level shape into a whole recursive structure.
-- 1段の形を結んで、再帰する構造全体にする
newtype Fix f = Fix (f (Fix f))

-- 🔽 cata: fold from the leaves up. The algebra says what to do with one
--    level whose children are already answers.
-- cata：葉から上へ畳む。「代数」は、子どもがもう答えになった1段を、どうするか
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (Fix layer) = alg (fmap (cata alg) layer)

-- 🔼 ana: unfold from a seed. The coalgebra says how to grow one level.
-- ana：種から広げる。「余代数」は、1段をどう育てるか
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg seed = Fix (fmap (ana coalg) (coalg seed))

-- 🔁 hylo: unfold, then fold, without building the middle structure.
-- hylo：広げてから畳む。真ん中の構造は作らない
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg
-- ANCHOR_END: fix

-- ANCHOR: list
-- 📜 One level of a list: empty, or one element and "the rest" (r).
-- リストの1段：空か、要素1つと「残り」（r）
data ListF a r = NilF | ConsF a r
  deriving (Show, Functor)

fromList :: [a] -> Fix (ListF a)
fromList = ana step
  where
    step []       = NilF
    step (x : xs) = ConsF x xs

toList' :: Fix (ListF a) -> [a]
toList' = cata alg
  where
    alg NilF        = []
    alg (ConsF x r) = x : r

-- ➕ Algebras: what to do with one level. No recursion in sight.
-- 代数：1段でやること。再帰は、どこにも出てこない
sumAlg :: ListF Int Int -> Int
sumAlg NilF            = 0
sumAlg (ConsF x total) = x + total

lengthAlg :: ListF a Int -> Int
lengthAlg NilF        = 0
lengthAlg (ConsF _ n) = 1 + n
-- ANCHOR_END: list

-- ANCHOR: expr
-- 🧮 One level of an arithmetic expression.
-- 計算式の1段
data ExprF r = LitF Int | AddF r r | MulF r r
  deriving (Show, Functor)

type Expr = Fix ExprF

lit :: Int -> Expr
lit = Fix . LitF

add, mul :: Expr -> Expr -> Expr
add a b = Fix (AddF a b)
mul a b = Fix (MulF a b)

-- 2 * (3 + 4)
twoTimes :: Expr
twoTimes = mul (lit 2) (add (lit 3) (lit 4))

-- 🟰 Evaluate: the children are already numbers.
-- 計算する：子どもは、もう数になっている
evalAlg :: ExprF Int -> Int
evalAlg (LitF n)   = n
evalAlg (AddF a b) = a + b
evalAlg (MulF a b) = a * b

-- 🖨️ Show: the children are already strings.
-- 表示する：子どもは、もう文字列になっている
showAlg :: ExprF String -> String
showAlg (LitF n)   = show n
showAlg (AddF a b) = "(" ++ a ++ " + " ++ b ++ ")"
showAlg (MulF a b) = a ++ " * " ++ b
-- ANCHOR_END: expr

-- ANCHOR: msort
-- 🌳 One level of merge sort's tree: nothing, one element, or two halves.
-- マージソートの木の1段：何もない、要素1つ、半分2つ
data TreeF a r = EmptyF | OneF a | TwoF r r
  deriving (Show, Functor)

-- ✂️ The coalgebra: split a list in two.
-- 余代数：リストを2つに分ける
split :: [a] -> TreeF a [a]
split []  = EmptyF
split [x] = OneF x
split xs  = let (l, r) = splitAt (length xs `div` 2) xs in TwoF l r

-- 🤝 The algebra: merge two sorted halves.
-- 代数：並んだ半分2つを、まとめる
mergeAlg :: Ord a => TreeF a [a] -> [a]
mergeAlg EmptyF     = []
mergeAlg (OneF x)   = [x]
mergeAlg (TwoF l r) = merge l r
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x : xs) (y : ys)
      | x <= y    = x : merge xs (y : ys)
      | otherwise = y : merge (x : xs) ys

-- 🔀 Merge sort = split all the way down, then merge all the way up.
-- マージソート ＝ 下まで分けて、上までまとめる
msortHylo :: Ord a => [a] -> [a]
msortHylo = hylo mergeAlg split
-- ANCHOR_END: msort

-- ANCHOR: fact
-- 🔢 Unfold n, n-1, ..., 1; fold them with (*). The list is never built.
-- n, n-1, ..., 1 と広げて、(*) で畳む。リストは作られない
countdown :: Int -> ListF Int Int
countdown 0 = NilF
countdown n = ConsF n (n - 1)

factorial :: Int -> Int
factorial = hylo productAlg countdown
  where
    productAlg NilF        = 1
    productAlg (ConsF n r) = n * r
-- ANCHOR_END: fact
-- ANCHOR_END: all
