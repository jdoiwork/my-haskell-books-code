-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Like a shell".
-- 「シェルのように」のトレーニングの解答。
module Lam.Answers
  ( shQuote
  , shLine
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Eval (ShellCommand (..))

-- ANCHOR: shQuote
-- 🪶 Step 3: a word as sh reads it back. A plain word as it is; any
--    other in '...', where a ' itself is written '\'' (end the quote,
--    a ' outside, start again).  it's  is  'it'\''s'
-- ステップ3：sh が読み戻せる語に。ふつうの語はそのまま。ほかは '…' に
-- 入れ、' そのものは '\'' と書く（クォートを閉じ、外に ' を1つ、また開く）
shQuote :: Text -> Text
shQuote w
  | not (T.null w), T.all plain w = w
  | otherwise = "'" <> T.replace "'" "'\\''" w <> "'"
  where
    plain c = isAlphaNum c || c `elem` ("-_./=:,@+%" :: String)

-- 🐚 A command as a line to paste into sh, with cd and env if it has
--    them:  cd repo && env LC_ALL=C sort
-- sh に貼れる1行。フォルダーと環境変数があれば、cd と env も
shLine :: ShellCommand -> Text
shLine (ShellCommand name args dir env _) =
  inDir <> withEnv <> T.unwords (map shQuote (name : args))
  where
    inDir = if T.null dir then "" else "cd " <> shQuote dir <> " && "
    withEnv = if null env then "" else
      "env " <> T.concat [ shQuote (k <> "=" <> v) <> " " | (k, v) <- env ]
-- ANCHOR_END: shQuote
-- ANCHOR_END: all
