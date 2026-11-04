-- ANCHOR: all
-- 📚 The book's own Prelude: the usual Prelude, plus the types and
--    functions we use all the time. Names follow relude where possible,
--    so that we can switch to relude later by changing one line.
-- 本書専用の Prelude：いつもの Prelude に、よく使う型と関数を足したもの
-- 名前はなるべく relude に合わせてあるので、あとで1行の変更で
-- relude に乗り換えられる
module BookPrelude
  ( -- the usual Prelude, except the String versions of lines and words
    -- いつもの Prelude（ただし lines と words は String 版を外す）
    module Prelude
    -- types / 型
  , Text
  , Map
    -- lines and words for Text / Text 用の lines と words
  , lines
  , unlines
  , words
  , unwords
    -- Text and String / Text と String の変換
  , toText
  , toString
    -- input and output with Text / Text での入出力
  , putTextLn
  , readFileText
  , writeFileText
  , appendFileText
    -- the outside world / 外の世界
  , getArgs
  , die
    -- handy tools / 便利な道具
  , readMaybe
  , fromMaybe
  , mapMaybe
  , sortOn
  , when
  , unless
  , forM_
  ) where

import Prelude hiding (lines, unlines, unwords, words)

import Control.Monad      (forM_, unless, when)
import Data.List          (sortOn)
import Data.Map.Strict    (Map)
import Data.Maybe         (fromMaybe, mapMaybe)
import Data.Text          (Text, lines, unlines, unwords, words)
import Data.Text          qualified as T
import Data.Text.IO       qualified as TIO
import System.Environment (getArgs)
import System.Exit        (die)
import Text.Read          (readMaybe)

-- 🔁 String to Text, and back.
-- String から Text へ、そして逆へ
toText :: String -> Text
toText = T.pack

toString :: Text -> String
toString = T.unpack

-- 🖨️ Text versions of putStrLn, readFile, writeFile and appendFile.
-- putStrLn、readFile、writeFile、appendFile の Text 版
putTextLn :: Text -> IO ()
putTextLn = TIO.putStrLn

readFileText :: FilePath -> IO Text
readFileText = TIO.readFile

writeFileText :: FilePath -> Text -> IO ()
writeFileText = TIO.writeFile

appendFileText :: FilePath -> Text -> IO ()
appendFileText = TIO.appendFile
-- ANCHOR_END: all
