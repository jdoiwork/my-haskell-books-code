-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Run it as a one-liner: like Perl".
-- 「ワンライナーで動かす：Perl のように」のトレーニングの解答。
module Lam.Answers
  ( paragraphs
  , eachNumbered
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lam.Eval
import Lam.Io (Handler, Stop (..), ioOf, runAction)
import Lam.OneLiner (Split, wrapLine)
import Lam.Parser (parseLam)
import Lam.Program
import Lam.Syntax
import Lam.Types (inferType)

-- ANCHOR: paragraphs
-- 📄 Step 2: -00 (Perl's paragraph mode). The text, cut at the empty
--    lines: one piece for each paragraph, its lines joined with "\n".
--    Many empty lines in a row are one cut, as in Perl.
-- ステップ2：空の行で切った、段落ごとのかけら。段落の中の行は "\n" で
-- つなぐ。空の行が続いても、切れ目は1つ（Perl と同じ）
paragraphs :: Text -> [Text]
paragraphs text = go (T.lines text)
  where
    blank = T.null . T.strip
    go ls = case dropWhile blank ls of
      []   -> []
      ls'  -> let (para, rest) = break blank ls'
              in T.intercalate "\n" para : go rest
-- ANCHOR_END: paragraphs

-- ANCHOR: eachNumbered
-- 🔢 Step 3: -n with the number of the line (Perl's $., awk's NR),
--    from 1. The program gets number and line:
--      \number -> \line -> program
--    Each line is done in order; the first error or exit stops it.
-- ステップ3：行の番号（1から）つきの -n。プログラムは number と line を
-- もらう。1行ずつ順にやり、エラーか exit で止まる
eachNumbered
  :: Monad m
  => Handler m -> Session -> Split -> Text -> [Text]
  -> m (Either Text ())
eachNumbered handle session split program ls =
  case parseLam "-e" program of
    Left err -> pure (Left err)
    Right body -> do
      let fun = Lam "number" (wrapLine split body)
      case inferType (sessionData session) (sessionTypes session) fun of
        Left err -> pure (Left (joined (placedType (explain session err))))
        Right _  -> case eval (sessionValues session) fun of
          Left err -> pure (Left (joined (placedEval err)))
          Right f  -> go f (zip [1 ..] ls)
  where
    joined (place, msg) = place <> msg
    go _ [] = pure (Right ())
    go f ((n, l) : rest) =
      case apply f (VInt n) >>= \g -> apply g (VString l) >>= ioOf of
        Left err -> pure (Left (joined (placedEval err)))
        Right action -> do
          done <- runAction handle action
          case done of
            Right _          -> go f rest
            Left (Failed e)  -> pure (Left (joined (placedEval e)))
            Left (Exited _)  -> pure (Right ())
-- ANCHOR_END: eachNumbered
-- ANCHOR_END: all
