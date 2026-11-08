-- ANCHOR: all
-- | traverse in four flavors: Maybe, Either, State and IO.
-- traverse を4つの味で：Maybe、Either、State、IO
module Shop
  ( priceOf
  , pricesMaybe
  , priceOrWhy
  , pricesEither
  , numbered
  , printAll
  ) where

-- ANCHOR: imports
import Control.Monad.State
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map

import Tree
-- ANCHOR_END: imports

-- ANCHOR: price
-- 💰 The shop's price list.
-- 道具屋の値段表
priceOf :: String -> Maybe Int
priceOf item = Map.lookup item prices
  where
    prices = Map.fromList
      [ ("potion", 50), ("herb", 10), ("elixir", 500), ("sword", 300) ]
-- ANCHOR_END: price

-- ANCHOR: maybe
-- 🧾 Price every item in the tree; if any has no price, Nothing.
-- 木のアイテムに全部値段を付ける。1つでも値段がなければ Nothing
pricesMaybe :: Tree String -> Maybe (Tree Int)
pricesMaybe = traverse priceOf
-- ANCHOR_END: maybe

-- ANCHOR: either
-- 🧾 The same, but say which item had no price.
-- 同じことを。値段のないアイテムの名前も言う
priceOrWhy :: String -> Either String Int
priceOrWhy item = case priceOf item of
  Just p  -> Right p
  Nothing -> Left (item ++ " is not for sale")

pricesEither :: Tree String -> Either String (Tree Int)
pricesEither = traverse priceOrWhy
-- ANCHOR_END: either

-- ANCHOR: state
-- 🏷️ Number every item, left to right, with State.
-- 左から順に、アイテムに番号を付ける（State で）
numbered :: Tree String -> Tree (Int, String)
numbered tree = evalState (traverse label tree) 1
  where
    label item = do
      n <- get
      put (n + 1)
      pure (n, item)
-- ANCHOR_END: state

-- ANCHOR: io
-- 📢 for_ is traverse that throws the results away: just do it for each.
-- for_ は、結果を捨てる traverse：1つずつ、やるだけ
printAll :: Tree String -> IO ()
printAll tree = for_ tree $ \item ->
  putStrLn ("- " ++ item)
-- ANCHOR_END: io
-- ANCHOR_END: all
