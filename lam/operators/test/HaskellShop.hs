-- ANCHOR: all
-- | The operators of the shop, in Haskell: the same as the ones of
--   examples/operators/strength.lam. Haskell names an operator
--   directly; the fixity line says how it is read.
-- お店の演算子を Haskell で。examples/operators/strength.lam と同じもの。
-- Haskell は演算子に直接名前を付け、fixity の行で読み方を言う。
module HaskellShop (Price (..), (<+>), (*$), (-%), today, sale) where

newtype Price = Price Int
  deriving (Eq, Show)

-- ANCHOR: fixity
-- + is 6 in Haskell (7 in Lam), so these are one less.
-- Haskell の + は 6（Lam は 7）なので、どれも1つ小さい
infixl 7 *$
infixl 6 <+>
infixl 6 -%
-- ANCHOR_END: fixity

-- ANCHOR: defs
(<+>) :: Price -> Price -> Price
Price a <+> Price b = Price (a + b)

(*$) :: Int -> Price -> Price
n *$ Price a = Price (n * a)

(-%) :: Price -> Int -> Price
Price a -% percent = Price (a * (100 - percent) `div` 100)
-- ANCHOR_END: defs

sticker, mug, tshirt :: Price
sticker = Price 300
mug     = Price 1200
tshirt  = Price 2500

-- ANCHOR: use
today, sale :: Price
today = 4 *$ sticker <+> mug <+> 3 *$ tshirt
sale  = 4 *$ sticker <+> mug -% 10
-- ANCHOR_END: use
-- ANCHOR_END: all
