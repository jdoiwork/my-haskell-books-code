-- ANCHOR: all
-- 🧾 The same shop, but a failure tells us why (Either).
-- 同じ道具屋。ただし、失敗したら理由がわかる（Either）
module ShopEither (priceE, payE, buyE, buyThreeE) where

-- ANCHOR: either
-- 💰 The price, or why there is none.
-- 値段か、ない理由
priceE :: String -> Either String Int
priceE "potion" = Right 50
priceE "elixir" = Right 150
priceE "sword"  = Right 300
priceE item     = Left (item ++ " is not for sale")

-- 👛 Pay, or say how much is missing.
-- 払う。足りなければ、いくら足りないかを伝える
payE :: Int -> Int -> Either String Int
payE cost gold
  | cost <= gold = Right (gold - cost)
  | otherwise    = Left ("need " ++ show (cost - gold) ++ " more gold")

-- 🛒 Buy one item, with do.
-- 1つ買う（do で）
buyE :: String -> Int -> Either String Int
buyE item gold = do
  p <- priceE item
  payE p gold

-- 🛒🛒🛒 Three in a row: the very same shape as with Maybe.
-- 3つ続けて：Maybe のときと、まったく同じ形
buyThreeE :: String -> String -> String -> Int -> Either String Int
buyThreeE a b c gold = do
  g1 <- buyE a gold
  g2 <- buyE b g1
  buyE c g2
-- ANCHOR_END: either
-- ANCHOR_END: all
