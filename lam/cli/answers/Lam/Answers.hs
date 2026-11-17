-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Making a CLI the easy way: options
--   from types, in the style of Typer".
-- 「CLI を手軽に作る：型から作る Typer 風のオプション」のトレーニングの解答。
module Lam.Answers
  ( distance
  , suggest
  , decideKindly
  , completions
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Cli
-- ANCHOR_END: head

-- ANCHOR: distance
-- 📏 Step 2: how many letters to add, remove or change to turn one
--    word into the other (the Levenshtein distance). One row at a time,
--    from the row of the empty word.
-- ステップ2：一方の語をもう一方にするのに、何文字足す・消す・変えるか
-- （レーベンシュタイン距離）。空の語の行から、1行ずつ
distance :: Text -> Text -> Int
distance a b = last (foldl nextRow [0 .. T.length b] (T.unpack a))
  where
    nextRow row c =
      scanl step (head' row + 1) (zip3 (T.unpack b) row (drop 1 row))
      where
        step left (d, diag, up) =
          minimum [left + 1, up + 1, diag + (if c == d then 0 else 1)]
    head' xs = case xs of
      x : _ -> x
      []    -> 0
-- ANCHOR_END: distance

-- ANCHOR: suggest
-- 💡 The nearest name, if it is 2 letters away or less (--cont is 1
--    away from --count).
-- いちばん近い名前。2文字以内なら（--cont は --count から1文字）
suggest :: [Text] -> Text -> Maybe Text
suggest names word =
  case sortOn fst [ (distance word n, n) | n <- names ] of
    (d, n) : _ | d <= 2 -> Just n
    _                   -> Nothing

-- 💡 decide, but an unknown option gets "did you mean …?" on its first
--    line, when a name of the program is near it.
-- decide と同じだが、知らないオプションには、近い名前があれば、1行目に
-- 「もしかして……？」を付ける
decideKindly :: Cli -> [Text] -> Outcome
decideKindly cli ws = case decide cli ws of
  Mistake msg -> Mistake (T.unlines (kindly (T.lines msg)))
  other       -> other
  where
    names = concatMap optionNames (commands (cliRuns cli))
    commands runs = case runs of
      One c   -> commandOpts c
      Many cs -> concat [ commandOpts c | (_, _, c) <- cs ]
    kindly ls = case ls of
      first : rest
        | Just word <- T.stripPrefix "error: unknown option " first
        , Just n <- suggest names word ->
            (first <> " (did you mean " <> n <> "?)") : rest
      _ -> ls
-- ANCHOR_END: suggest

-- ANCHOR: completions
-- ⌨️ Step 3: the words the shell can offer for the last word typed
--    (the words before it are done). Before a subcommand, the
--    commands; after it, its options.
-- ステップ3：最後に打ちかけの語に、シェルが出せる候補（その前の語は
-- 打ち終えたもの）。サブコマンドの前ならコマンド、後ろならその
-- オプション
completions :: Cli -> [Text] -> [Text]
completions cli typed = filter (prefix `T.isPrefixOf`) candidates
  where
    (done, prefix) = case reverse typed of
      p : before -> (reverse before, p)
      []         -> ([], "")
    candidates = case cliRuns cli of
      One c   -> namesOf c
      Many cs -> case [ c | (n, _, c) <- cs, kebab n `elem` done ] of
        c : _ -> namesOf c
        []    -> [ kebab n | (n, _, _) <- cs ] <> ["--help"]
    -- The long names only (--count, not -n).
    -- 長い名前だけ（-n ではなく --count）
    namesOf c = filter ("--" `T.isPrefixOf`)
      (concatMap optionNames (commandOpts c)) <> ["--help"]
-- ANCHOR_END: completions
-- ANCHOR_END: all
