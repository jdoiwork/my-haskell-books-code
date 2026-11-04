-- ANCHOR: all
-- 🛒 The shop again: buying three items, one after another.
-- また道具屋：3つのアイテムを、順に買う
module Shop
  ( price
  , pay
  , buy
  , buyThreeCase
  , andThen
  , buyThreeAndThen
  , buyThreeBind
  , buyThreeDo
  ) where

-- ANCHOR: basics
-- 💰 The price of an item, if the shop sells it.
-- アイテムの値段（売っていれば）
price :: String -> Maybe Int
price "potion" = Just 50
price "elixir" = Just 150
price "sword"  = Just 300
price _        = Nothing

-- 👛 Pay; the gold left, if we can afford it.
-- 払う。足りれば、残りの所持金
pay :: Int -> Int -> Maybe Int
pay cost gold
  | cost <= gold = Just (gold - cost)
  | otherwise    = Nothing

-- 🛒 Buy one item: find the price, then pay.
-- 1つ買う：値段を調べて、払う
buy :: String -> Int -> Maybe Int
buy item gold =
  case price item of
    Nothing -> Nothing
    Just p  -> pay p gold
-- ANCHOR_END: basics

-- ANCHOR: case
-- 🪜 Three in a row, with case: the code goes down like stairs.
-- 3つ続けて、case で：コードが階段のように下がっていく
buyThreeCase :: String -> String -> String -> Int -> Maybe Int
buyThreeCase a b c gold =
  case buy a gold of
    Nothing -> Nothing
    Just g1 ->
      case buy b g1 of
        Nothing -> Nothing
        Just g2 ->
          case buy c g2 of
            Nothing -> Nothing
            Just g3 -> Just g3
-- ANCHOR_END: case

-- ANCHOR: andThen
-- 🔗 The pattern, as a function: if there is something in the box,
--    hand it to the next step; if not, stop with Nothing.
-- 決まった形を、関数にする：箱に中身があれば、次の手順に渡す。
-- なければ、Nothing で止まる
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen box next =
  case box of
    Nothing -> Nothing
    Just x  -> next x

-- 🔗 Three in a row, with andThen: no more stairs.
-- 3つ続けて、andThen で：階段がなくなる
buyThreeAndThen :: String -> String -> String -> Int -> Maybe Int
buyThreeAndThen a b c gold =
  buy a gold `andThen` \g1 ->
  buy b g1   `andThen` \g2 ->
  buy c g2
-- ANCHOR_END: andThen

-- ANCHOR: bind
-- 🔗 andThen is already in Haskell, with the name (>>=).
-- andThen は、Haskell に (>>=) という名前で、最初からある
buyThreeBind :: String -> String -> String -> Int -> Maybe Int
buyThreeBind a b c gold =
  buy a gold >>= \g1 ->
  buy b g1   >>= \g2 ->
  buy c g2
-- ANCHOR_END: bind

-- ANCHOR: do
-- ✨ And do is a nicer way to write the (>>=) chain.
-- そして do は、(>>=) のつながりを、読みやすく書く書き方
buyThreeDo :: String -> String -> String -> Int -> Maybe Int
buyThreeDo a b c gold = do
  g1 <- buy a gold
  g2 <- buy b g1
  buy c g2
-- ANCHOR_END: do
-- ANCHOR_END: all
