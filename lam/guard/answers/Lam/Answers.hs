-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Lock it: permission flags and
--   guards".
-- 「鍵をかける：権限のフラグと見張り」のトレーニングの解答。
module Lam.Answers
  ( denyHosts
  , tracedAnswers
  , limitWrites
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lam.Builtins (con)
import Lam.Eval
import Lam.Guard (hostOf)
import Lam.Io (Handler)

-- ANCHOR: denyHosts
-- 🚫 Step 1: --deny-net=ads.example. A host in the list is refused,
--    even when --allow-net allows the whole network. Put it inside
--    allowOnly, so that a deny always wins.
-- ステップ1：一覧のホストは、--allow-net がネット全部を許していても
-- 断る。allowOnly の内側に置くので、いつも「断る」が勝つ
denyHosts :: Applicative m => [Text] -> Handler m -> Handler m
denyHosts hosts handle request = case request of
  HttpSend _ url _ _
    | hostOf url `elem` hosts ->
        pure (con "Err" [VString (url <> ": denied by --deny-net")])
  _ -> handle request
-- ANCHOR_END: denyHosts

-- ANCHOR: tracedAnswers
-- 👣 Step 2: the trace with the answers. After each request, one more
--    line: what came back, cut at 50 letters.
-- ステップ2：答えも見せる trace。頼みごとのあとに、もう1行：返って
-- きたもの（50 文字で切る）
tracedAnswers :: Monad m => Handler m -> Handler m
tracedAnswers handle request = case request of
  PutStr _ -> handle request
  PutErr _ -> handle request
  _ -> do
    answer <- handle request
    let shown = renderValue answer
        short = if T.length shown > 50 then T.take 49 shown <> "…" else shown
    _ <- handle (PutErr ("  => " <> short <> "\n"))
    pure answer
-- ANCHOR_END: tracedAnswers

-- ANCHOR: limitWrites
-- ✍️ Step 3: --max-write=1000. A file longer than the limit is not
--    written: the answer is Err, and the file stays as it was.
-- ステップ3：上限より長い中身は書かない。答えは Err で、ファイルは
-- そのまま
limitWrites :: Applicative m => Int -> Handler m -> Handler m
limitWrites limit handle request = case request of
  WriteFile path text
    | T.length text > limit ->
        pure (con "Err" [VString (path <> ": " <> count text
          <> " letters is more than --max-write=" <> count' limit)])
  _ -> handle request
  where
    count t = T.pack (show (T.length t))
    count' n = T.pack (show n)
-- ANCHOR_END: limitWrites
-- ANCHOR_END: all
