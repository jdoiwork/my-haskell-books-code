-- ANCHOR: all
-- ANCHOR: parts
-- 🦸 Building a hero from parts that may be missing: <$> and <*>.
-- 欠けているかもしれない部品から勇者を作る：<$> と <*>
module Hero (Hero (..), readLevel, readName, heroFrom, heroFromDo) where

import Text.Read (readMaybe)

-- 📇 A hero has a name and a level.
-- 勇者は、名前とレベルを持つ
data Hero = Hero
  { name  :: String
  , level :: Int
  }
  deriving (Show)

-- 🔢 Read a level, if it is a number from 1 to 99.
-- レベルを読む（1〜99 の数なら）
readLevel :: String -> Maybe Int
readLevel s = case readMaybe s of
  Just n | n >= 1 && n <= 99 -> Just n
  _                          -> Nothing

-- 🏷️ Read a name, if it is not empty.
-- 名前を読む（空でなければ）
readName :: String -> Maybe String
readName "" = Nothing
readName s  = Just s
-- ANCHOR_END: parts

-- ANCHOR: applicative
-- 🦸 Hero name level, but each part is in a Maybe box.
--    The parts do not depend on each other: Applicative is enough.
-- Hero 名前 レベル。ただし部品は Maybe の箱に入っている。
-- 部品どうしは頼り合わないので、Applicative で足りる
heroFrom :: String -> String -> Maybe Hero
heroFrom n l = Hero <$> readName n <*> readLevel l

-- 🦸 The same, with do (it works, since Maybe is also a Monad).
-- 同じものを do で（Maybe は Monad でもあるので、書ける）
heroFromDo :: String -> String -> Maybe Hero
heroFromDo n l = do
  hero <- readName n
  lv   <- readLevel l
  pure (Hero hero lv)
-- ANCHOR_END: applicative
-- ANCHOR_END: all
