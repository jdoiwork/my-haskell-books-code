-- ANCHOR: all
-- | Answers to the training of "Foldable and Traversable".
-- 「Foldable・Traversable」のトレーニングの解答
module FoldAnswers
  ( Rose (..)
  , party
  , restock
  ) where

import qualified Data.Map.Strict as Map

-- ANCHOR: rose
-- 🌹 Step 2: a tree whose nodes have any number of children.
-- ステップ2：節が、子どもをいくつでも持てる木
data Rose a = Rose a [Rose a]
  deriving (Show, Eq)

instance Foldable Rose where
  foldMap f (Rose v children) = f v <> foldMap (foldMap f) children

-- 👑 A guild: the master, two leaders, and their members.
-- ギルド：ギルド長、2人のリーダー、その仲間たち
party :: Rose String
party =
  Rose "master"
    [ Rose "lambda" [Rose "python" [], Rose "ts" []]
    , Rose "rust" [Rose "ruby" []]
    ]
-- ANCHOR_END: rose

-- ANCHOR: restock
-- 📦 Step 3: order more of each item; stop if any order is too big.
-- ステップ3：アイテムごとに追加の注文。大きすぎる注文があれば止める
restock :: Map.Map String Int -> Either String (Map.Map String Int)
restock = traverse order
  where
    order n
      | n > 99    = Left ("cannot order " ++ show n)
      | otherwise = Right (n + 10)
-- ANCHOR_END: restock
-- ANCHOR_END: all
