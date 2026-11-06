-- ANCHOR: all
-- | Answers to the training of "A strict, typed client".
-- 「型に厳しいクライアント」のトレーニングの解答
module MqAnswers
  ( lineQuest
  , invmap
  ) where

import qualified Data.Text as T
import Text.Read (readMaybe)

import MiniMq.Guild
import MiniMq.Typed

-- ANCHOR: lineQuest
-- 📝 Step 2: a quest as one short line, "Slay a slime;30".
-- ステップ2：依頼を、短い1行で。「Slay a slime;30」
lineQuest :: Codec Quest
lineQuest = Codec
  { toBody   = \q -> title q <> ";" <> T.pack (show (reward q))
  , fromBody = \text -> case T.splitOn ";" text of
      [t, r] | Just n <- readMaybe (T.unpack r) -> Right (Quest t n)
      _ -> Left ("not a quest: " <> text)
  }
-- ANCHOR_END: lineQuest

-- ANCHOR: invmap
-- 🔁 Step 3: a codec for `b`, made from a codec for `a` and a way there
--    and back. (A `Codec` both takes and gives `a`s, so it needs both.)
-- ステップ3：a のコーデックと、行きと帰りの関数から、b のコーデックを
-- 作る（Codec は a を受け取りも返しもするので、両方向が要る）
invmap :: (a -> b) -> (b -> a) -> Codec a -> Codec b
invmap there back codec = Codec
  { toBody   = toBody codec . back
  , fromBody = fmap there . fromBody codec
  }
-- ANCHOR_END: invmap
-- ANCHOR_END: all
