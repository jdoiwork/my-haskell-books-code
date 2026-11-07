-- check-langs: should fail
-- A call that Liquid Haskell must reject: 0 > 0 is false.
-- check-langs.sh expects this file to fail.
-- Liquid Haskell が止めるはずの呼び出し（0 > 0 ではない）。
-- check-langs.sh は、このファイルが失敗することを確かめる
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
module ShareZero where

import Share

nobody :: Int
nobody = share 100 0
