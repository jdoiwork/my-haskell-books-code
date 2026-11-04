module BestiaryAnswers where

import Bestiary

-- ANCHOR: weakAgainst
-- 🛡️ Which element is this element weak against?
-- この属性は、どの属性に弱い？
weakAgainst :: Element -> Element
weakAgainst Fire  = Water
weakAgainst Water = Grass
weakAgainst Grass = Fire
-- ANCHOR_END: weakAgainst

-- ANCHOR: healAmount
-- 💚 How much HP does this action heal? (0 if it is not Heal)
-- この行動で HP をいくつ回復する？（Heal 以外は 0）
healAmount :: Action -> Int
healAmount (Heal amount) = amount
healAmount _             = 0
-- ANCHOR_END: healAmount

-- ANCHOR: parseElement
-- 🔤 Turn a string typed by the player into an Element.
--    An unknown name comes back as a Left with the reason.
-- プレイヤーが入力した文字列を、属性に変える
-- 知らない名前なら、理由付きの Left を返す
parseElement :: String -> Either String Element
parseElement "fire"  = Right Fire
parseElement "water" = Right Water
parseElement "grass" = Right Grass
parseElement other   = Left ("unknown element: " ++ other)
-- ANCHOR_END: parseElement
