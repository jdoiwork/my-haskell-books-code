-- ANCHOR: all
-- 🎯 Problem 2: the weakest attack that beats every monster in time.
-- 問題2：時間内に全部のモンスターを倒せる、いちばん弱い攻撃力
module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Data.Maybe            (mapMaybe)

import Algo (weakestAttack)

ints :: BS.ByteString -> [Int]
ints = map fst . mapMaybe BS.readInt . BS.words

main :: IO ()
main = do
  input <- BS.getContents
  case ints input of
    (n : k : hps) -> print (weakestAttack (take n hps) k)
    _             -> pure ()
-- ANCHOR_END: all
