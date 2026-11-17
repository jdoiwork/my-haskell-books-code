-- ANCHOR: all
-- | The answers of the training of "Jobs that talk: channels and
--   messages".
-- 「仕事どうしで話す：チャネルとメッセージ」のトレーニングの解答。
module Lam.Answers
  ( selectAny
  ) where

import Control.Concurrent.STM (atomically, orElse)

import Lam.Eval (Chan, Value)
import Lam.Io (recvSTM)

-- ANCHOR: selectAny
-- 🔀 Step 3: select. Wait on several channels at once, and take the
--    first value that comes: its place in the list, and Just the value
--    (or Nothing, if that channel was closed). orElse tries the left
--    transaction; if it would wait (retry), it tries the right one.
--    If every one would wait, the whole waits, until any of the
--    channels changes. No value is taken twice, and none is lost.
-- ステップ3：select。いくつかのチャネルを同時に待ち、最初に来た値を取る：
-- リストの中の場所と、値を Just で（そのチャネルが閉じていれば Nothing）。
-- orElse は左のトランザクションを試し、待つ（retry）なら右を試す。どれも
-- 待つなら、全体が、どれかのチャネルが変わるまで待つ。同じ値を2度取る
-- ことも、なくすこともない
selectAny :: [Chan] -> IO (Int, Maybe Value)
selectAny chans =
  atomically $ foldr1 orElse
    [ (,) i <$> recvSTM c | (i, c) <- zip [0 ..] chans ]
-- ANCHOR_END: selectAny
-- ANCHOR_END: all
