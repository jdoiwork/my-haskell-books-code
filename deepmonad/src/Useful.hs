-- ANCHOR: all
-- | What the three classes buy you: functions written once,
--   for every box.
-- 3つの型クラスで、何が得をするか：一度書いた関数が、どの箱にも使える
module Useful
  ( plus10
  , times10
  , addHp
  , halfMaybe
  , halfEither
  , swing
  , Hero (..)
  , heal
  , pairUp
  , checkout
  , payMaybe
  , payEither
  , payHaggle
  , payLog
  , readHp
  ) where

-- ANCHOR: imports
import Control.Monad (foldM)
import Text.Read (readMaybe)
-- ANCHOR_END: imports

-- ANCHOR: plus10
-- Ordinary functions for the examples. They are not the point:
-- the point is fmap, <*> and >>=, which work the same way on every box.
-- 例に使う、ふつうの関数。覚えなくてよい。主役は、どの箱にも同じように使える
-- fmap・<*>・>>= のほう

-- ➕ Add 10 to a number. It knows nothing about boxes.
-- 数に 10 を足すだけ。箱のことは何も知らない
plus10 :: Int -> Int
plus10 n = n + 10

-- ✖️ Multiply a number by 10. Just as ordinary.
-- 数を 10 倍にするだけ。これも、ふつうの関数
times10 :: Int -> Int
times10 n = n * 10

-- 🧮 Add two HPs. It knows nothing about boxes either.
-- 2つの HP を足すだけ。これも箱のことは知らない
addHp :: Int -> Int -> Int
addHp a b = a + b

-- ✂️ Halve an even number. An odd number cannot be halved:
--    the box says so in its own way.
-- 偶数を半分にする。奇数は半分にできないので、箱ごとのやり方で、そう伝える
halfMaybe :: Int -> Maybe Int
halfMaybe n
  | even n    = Just (n `div` 2)
  | otherwise = Nothing

halfEither :: Int -> Either String Int
halfEither n
  | even n    = Right (n `div` 2)
  | otherwise = Left (show n ++ " is odd")

-- 🎲 Two possible futures: healed by 10, or hurt by 10.
-- 2つの未来：10 回復するか、10 ダメージを受けるか
swing :: Int -> [Int]
swing n = [n + 10, n - 10]
-- ANCHOR_END: plus10

-- ANCHOR: hero
-- 🦸 A hero: a name and an HP. The constructor Hero is a function too:
--    Hero :: String -> Int -> Hero
-- 勇者：名前と HP。コンストラクターの Hero も、関数の1つ
data Hero = Hero String Int
  deriving (Show)
-- ANCHOR_END: hero

-- ANCHOR: heal
-- 💚 Heal every HP in the box, whatever the box is.
-- 箱の中の HP を回復する。箱が何であっても
heal :: Functor f => Int -> f Int -> f Int
heal amount hps = fmap (+ amount) hps
-- ANCHOR_END: heal

-- ANCHOR: pairUp
-- 🤝 Pair up two results that do not depend on each other.
-- おたがいに頼らない2つの結果を、組にする
pairUp :: Applicative f => f a -> f b -> f (a, b)
pairUp first second = (,) <$> first <*> second
-- ANCHOR_END: pairUp

-- ANCHOR: checkout
-- 🛒 Buy the items in order. How paying fails (or branches, or
--    prints) is up to the box.
-- 品物を順に買う。払うときに、どう失敗するか（枝分かれするか、
-- 表示するか）は、箱に任せる
checkout :: Monad m => (Int -> Int -> m Int) -> Int -> [Int] -> m Int
checkout pay gold prices = foldM pay gold prices
-- ANCHOR_END: checkout

-- ANCHOR: pays
-- 🪙 Maybe: stop when the gold runs short.
-- Maybe：お金が足りなければ止まる
payMaybe :: Int -> Int -> Maybe Int
payMaybe gold price
  | gold >= price = Just (gold - price)
  | otherwise     = Nothing

-- 📝 Either: stop, with the reason.
-- Either：理由を連れて止まる
payEither :: Int -> Int -> Either String Int
payEither gold price
  | gold >= price = Right (gold - price)
  | otherwise     = Left ("short for a " <> show price <> " G item")

-- 🎲 List: pay the full price, or haggle it down to half.
--    Every way it could go.
-- リスト：定価で払うか、半額に値切るか。ありうる全部
payHaggle :: Int -> Int -> [Int]
payHaggle gold price =
  [ gold - cost | cost <- [price, price `div` 2], gold >= cost ]

-- 🖨️ IO: really say what happens, and skip what we cannot afford.
-- IO：本当に、何が起きたかを表示する。買えないものは、とばす
payLog :: Int -> Int -> IO Int
payLog gold price
  | gold >= price = do
      -- 「30 G を払って、残りは 70 G」
      putStrLn ("paid " <> show price <> " G, " <> show (gold - price)
                <> " G left")
      pure (gold - price)
  | otherwise = do
      -- 「80 G の品物は買えない。とばす」
      putStrLn ("can't pay " <> show price <> " G, skipped")
      pure gold
-- ANCHOR_END: pays

-- ANCHOR: readHp
-- 🔢 Read one HP, or give up.
-- HP を1つ読む。読めなければあきらめる
readHp :: String -> Maybe Int
readHp text = readMaybe text
-- ANCHOR_END: readHp
-- ANCHOR_END: all
