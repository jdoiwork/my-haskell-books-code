-- ANCHOR: module
module Shop where

-- fromMaybe: take the value out of a Maybe, with a fallback.
-- fromMaybe：Maybe から値を取り出す（なければ代わりの値）
import Data.Maybe (fromMaybe)
-- ANCHOR_END: module

-- ANCHOR: greeting
-- 💬 What the shopkeeper says, by how much gold you have. (if)
-- 所持金によって変わる、店主のあいさつ（if）
greeting :: Int -> String
greeting gold =
  if gold >= 1000
    then "Welcome, my best customer!"
    else if gold >= 100
      then "Welcome, adventurer!"
      else "Just looking?"
-- ANCHOR_END: greeting

-- ANCHOR: greetingGuards
-- 💬 The same greeting, written with guards.
-- 同じあいさつを、ガードで書いたもの
greeting' :: Int -> String
greeting' gold
  | gold >= 1000 = "Welcome, my best customer!"
  | gold >= 100  = "Welcome, adventurer!"
  | otherwise    = "Just looking?"
-- ANCHOR_END: greetingGuards

-- ANCHOR: category
-- 🗂️ Which shelf does this item go on? (case)
-- このアイテムは、どの棚に並べる？（case）
category :: String -> String
category item =
  case item of
    "potion" -> "medicine"
    "elixir" -> "medicine"
    "sword"  -> "weapon"
    "shield" -> "armor"
    _        -> "unknown"
-- ANCHOR_END: category

-- ANCHOR: category2
-- 🗂️ The same function, written as several equations.
-- 同じ関数を、定義を何行も並べて書いたもの
category' :: String -> String
category' "potion" = "medicine"
category' "elixir" = "medicine"
category' "sword"  = "weapon"
category' "shield" = "armor"
category' _        = "unknown"
-- ANCHOR_END: category2

-- ANCHOR: place
-- 🗺️ Where are we on the map? The position is (x, y).
--    The town is at (0, 0), the road runs along x = 0,
--    and the river runs along y = 0.
-- 地図のどこにいる？ 位置は (x, y)。町は (0, 0)、
-- 道は x = 0 に沿って、川は y = 0 に沿って続いている
place :: (Int, Int) -> String
place (0, 0) = "town"
place (0, _) = "road"
place (_, 0) = "river"
place _      = "wild"
-- ANCHOR_END: place

-- ANCHOR: distance
-- 📏 How far is it from the town? (walking along the grid)
--    The pattern (x, y) gives names to both parts.
-- 町からの距離（マス目に沿って歩く）
-- パターン (x, y) で、2つの部分に名前を付ける
distance :: (Int, Int) -> Int
distance (x, y) = abs x + abs y
-- ANCHOR_END: distance

-- ANCHOR: basket
-- 🧺 What the shopkeeper says about your basket. (list patterns)
-- カゴの中身を見た、店主のひとこと（リストのパターン）
basketComment :: [String] -> String
basketComment []         = "Your basket is empty."
basketComment ["potion"] = "Just a potion? Take care out there!"
basketComment [_]        = "Just one item?"
basketComment [_, _]     = "Two items. Nice choice!"
basketComment _          = "Wow, a big shopping trip!"
-- ANCHOR_END: basket

-- ANCHOR: price
-- 💰 The price of an item.
--    Nothing means "this shop does not sell it".
-- アイテムの値段。Nothing は「この店では売っていない」
price :: String -> Maybe Int
price item =
  case item of
    "potion" -> Just 50
    "elixir" -> Just 150
    "sword"  -> Just 300
    _        -> Nothing
-- ANCHOR_END: price

-- ANCHOR: priceTag
-- 🏷️ The price tag shown in the shop. (case on a Maybe)
-- 店に並べる値札（Maybe を case で分ける）
priceTag :: String -> String
priceTag item =
  case price item of
    Just p  -> item ++ ": " ++ show p ++ " G"
    Nothing -> item ++ ": not for sale"
-- ANCHOR_END: priceTag

-- ANCHOR: myFromMaybe
-- 🧰 Our own fromMaybe: open the box with case.
--    If it is empty, use the fallback.
-- 自分で作る fromMaybe：case で箱を開ける
-- 空っぽなら、代わりの値を使う
myFromMaybe :: a -> Maybe a -> a
myFromMaybe fallback box =
  case box of
    -- something inside: take it out
    -- 中身がある：取り出す
    Just x  -> x
    -- empty: use the fallback
    -- 空っぽ：代わりの値を使う
    Nothing -> fallback
-- ANCHOR_END: myFromMaybe

-- ANCHOR: estimate
-- 🧮 A rough estimate for two items.
--    Items that are not for sale count as 0 G.
-- 2つのアイテムのおおよその見積もり
-- 売っていないアイテムは 0 G として数える
estimate :: String -> String -> Int
estimate item1 item2 =
  fromMaybe 0 (price item1) + fromMaybe 0 (price item2)
-- ANCHOR_END: estimate

-- ANCHOR: pay
-- 👛 Pay for an item. Returns the gold left,
--    or Nothing if you cannot afford it.
-- 代金を払う。残りの所持金を返す
-- お金が足りないときは Nothing
pay :: Int -> Int -> Maybe Int
pay cost gold
  | cost <= gold = Just (gold - cost)
  | otherwise    = Nothing
-- ANCHOR_END: pay

-- ANCHOR: buy
-- 🛒 Buy an item. Returns the gold left, or Nothing
--    if the shop does not sell it or you cannot afford it.
-- アイテムを買う。残りの所持金を返す
-- 売っていない、またはお金が足りないときは Nothing
buy :: String -> Int -> Maybe Int
buy item gold =
  case price item of
    -- not for sale
    -- 売っていない
    Nothing -> Nothing
    -- for sale: pay for it (if we can afford it)
    -- 売っている：代金を払う（お金が足りれば）
    Just p  -> pay p gold
-- ANCHOR_END: buy

-- ANCHOR: buyTwo
-- 🛒🛒 Buy two items, one after the other.
--    If the first purchase fails, we do not try the second.
-- 2つのアイテムを順番に買う
-- 1つ目で失敗したら、2つ目は買わない
buyTwo :: String -> String -> Int -> Maybe Int
buyTwo item1 item2 gold =
  -- the result of the first purchase (a Maybe Int box)
  -- 1つ目の買い物の結果（Maybe Int の箱）
  let afterItem1 = buy item1 gold
  in  case afterItem1 of
        -- the first purchase failed: stop here
        -- 1つ目で失敗した：ここで止まる
        Nothing        -> Nothing
        -- it worked: buy the second item with the gold left
        -- 成功した：残りのお金で2つ目を買う
        Just remaining -> buy item2 remaining
-- ANCHOR_END: buyTwo

-- ANCHOR: buyTwoShort
-- 🛒🛒 The same function, without naming the first result.
-- 同じ関数を、1つ目の結果に名前を付けずに書いたもの
buyTwo' :: String -> String -> Int -> Maybe Int
buyTwo' item1 item2 gold =
  case buy item1 gold of
    Nothing        -> Nothing
    Just remaining -> buy item2 remaining
-- ANCHOR_END: buyTwoShort

-- ANCHOR: buyTwoDo
-- 🦸 buyTwo, written with do.
-- do で書いた buyTwo
buyTwoDo :: String -> String -> Int -> Maybe Int
buyTwoDo item1 item2 gold = do
  remaining <- buy item1 gold
  buy item2 remaining
-- ANCHOR_END: buyTwoDo

-- ANCHOR: buyTwoBind
-- 🦸 buyTwo, written with >>=.
-- >>= で書いた buyTwo
buyTwoBind :: String -> String -> Int -> Maybe Int
buyTwoBind item1 item2 gold = buy item1 gold >>= buy item2
-- ANCHOR_END: buyTwoBind
