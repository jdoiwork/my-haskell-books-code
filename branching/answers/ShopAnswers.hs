module ShopAnswers where

import Shop

-- ANCHOR: battleCry
-- 📣 A battle cry for each element. (case)
-- 属性ごとの、技のかけ声（case）
battleCry :: String -> String
battleCry element =
  case element of
    "fire"  -> "Blaze!"
    "water" -> "Splash!"
    "grass" -> "Leaf storm!"
    _       -> "Punch!"
-- ANCHOR_END: battleCry

-- ANCHOR: weakness
-- 🎯 Which element is this element weak against?
--    Nothing means "no weakness".
-- この属性の弱点の属性は？ Nothing は「弱点なし」
weakness :: String -> Maybe String
weakness element =
  case element of
    "fire"  -> Just "water"
    "water" -> Just "grass"
    "grass" -> Just "fire"
    _       -> Nothing
-- ANCHOR_END: weakness

-- ANCHOR: safeDiv
-- ➗ Division that does not crash: dividing by 0 gives Nothing.
-- 落ちない割り算：0 で割ると Nothing
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
-- ANCHOR_END: safeDiv

-- ANCHOR: canBuy
-- ✅ Can we buy this item with this much gold?
-- この所持金で、このアイテムを買える？
canBuy :: String -> Int -> Bool
canBuy item gold =
  case buy item gold of
    Just _  -> True
    Nothing -> False
-- ANCHOR_END: canBuy
