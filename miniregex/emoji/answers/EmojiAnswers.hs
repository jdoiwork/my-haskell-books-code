-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training of the emoji chapter: turning a post
--   around without breaking its emoji, and finding the keycaps too.
-- 絵文字の章のトレーニングの解答：絵文字をこわさずに書き込みを逆さにする、
-- キーキャップの絵文字も見つける
module EmojiAnswers
  ( reverseClusters
  , emojiWithKeys
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex (Match (..), findAll)
import MiniRegex.Examples (surely)
import MiniRegex.Grapheme (clusters)
-- ANCHOR_END: head

-- ANCHOR: reverse
-- 🔄 Step 2: turn the text around, a cluster at a time (T.reverse
--    turns 🇯🇵 into 🇵🇯, and takes the skin tone off the thumb).
-- ステップ2：クラスターごとに逆さにする（T.reverse だと 🇯🇵 が 🇵🇯 に
-- なり、親指から肌の色が外れる）
reverseClusters :: Text -> Text
reverseClusters = T.concat . reverse . clusters
-- ANCHOR_END: reverse

-- ANCHOR: keys
-- 🔢 Step 3: the emoji a person sees, and the keycaps (#️⃣ is #, the
--    emoji mark U+FE0F, and the keycap U+20E3).
-- ステップ3：人が見る絵文字と、キーキャップ（#️⃣ は #、絵文字にする印
-- U+FE0F、キーキャップ U+20E3）
emojiWithKeys :: Text -> [Text]
emojiWithKeys post = map matchText (findAll seen post)
  where
    seen = surely
      "(?=[\\p{ExtPict}\\p{RI}]|[0-9#*]\\x{FE0F}?\\x{20E3})\\X"
-- ANCHOR_END: keys
-- ANCHOR_END: all
