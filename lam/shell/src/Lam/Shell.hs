{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | The builtins of the modules Shell and Term: start a command, quote
--   a word, and the few things of the terminal that Lam cannot write
--   by itself (is the screen a terminal, a line typed unseen, the
--   marks of colors, and how wide a text looks).
-- モジュール Shell と Term の組み込みの関数：コマンドを始める、語を
-- クォートする、そして Lam だけでは書けない端末のこと（画面は端末か、
-- 見せずに打つ1行、色の印、文字の見た目の幅）。
module Lam.Shell
  ( shellBuiltins
  , quoteWord
  , renderCommand
  , stripStyles
  , displayWidth
  ) where

import Data.Char (isAlphaNum, ord)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Builtins
import Lam.Eval
import Lam.Pretty (renderString)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
shellBuiltins :: [Builtin]
shellBuiltins =
  -- ANCHOR: spawn
  -- Start one command, made from the record of the module Shell. The
  -- runner does it (or the guard says no).
  -- モジュール Shell のレコードから、コマンドを1つ始める。やるのは実行役
  -- （見張りが断ることもある）
  [ prim1 "Shell.spawn" (tStep ~> tIO (TCon "Outcome" [])) $ \v -> do
      command <- commandOf v
      Right (VIO (Ask (RunCommand command)))
  -- ANCHOR_END: spawn
  , prim1 "Shell.quote" (tString ~> tString) $ \v ->
      VString . quoteWord <$> string v
  -- ANCHOR: termBuiltins
  -- Is the screen a terminal (not a file, not a pipe)?
  -- 画面は端末か（ファイルでもパイプでもないか）
  , Builtin "Term.isTerminal" (closeOver (tIO TBool)) (VIO (Ask IsTerminal))
  -- A line typed with the letters not shown (Nothing at the end).
  -- 打った字を見せずに読む1行（終わりなら Nothing）
  , Builtin "Term.readSecret" (closeOver (tIO (tMaybe tString)))
      (VIO (Ask GetSecret))
  -- The marks of a style around a text:  style "31" "no" is red.
  -- 文字の前後に、見た目の印を付ける。style "31" "no" は赤
  , prim2 "Term.style" (tString ~> tString ~> tString) $ \c t -> do
      code <- string c
      text <- string t
      Right (VString (esc <> "[" <> code <> "m" <> text <> esc <> "[0m"))
  -- The mark that clears the rest of the line (for a bar drawn again).
  -- 行の残りを消す印（描き直す棒のため）
  , Builtin "Term.clearLine" (closeOver tString) (VString (esc <> "[K"))
  , prim1 "Term.strip" (tString ~> tString) $ \v ->
      VString . stripStyles <$> string v
  , prim1 "Term.width" (tString ~> TInt) $ \v ->
      VInt . toInteger . displayWidth <$> string v
  -- ANCHOR_END: termBuiltins
  ]
  where
    esc = "\ESC"
-- ANCHOR_END: table

-- ANCHOR: step
-- 🧾 The record the module Shell gives to spawn: one command.
-- モジュール Shell が spawn に渡すレコード：コマンド1つ
tStep :: Type
tStep = TRecord (Map.fromList
  [ ("program", tString)
  , ("args",    tList tString)
  , ("dir",     tString)
  , ("env",     tMap tString tString)
  , ("input",   tString) ]) Nothing

-- 🧾 The record, as a ShellCommand of Haskell.
-- レコードを、Haskell の Command に
commandOf :: Value -> Either EvalError ShellCommand
commandOf v = case v of
  VRecord fields -> do
    let field n = maybe (Left (needs n)) Right (Map.lookup n fields)
    name  <- field "program" >>= string
    args  <- field "args" >>= list >>= traverse string
    dir   <- field "dir" >>= string
    env   <- field "env" >>= mapOf >>= traverse pairOf . Map.toList
    input <- field "input" >>= string
    Right (ShellCommand name args dir env input)
  _ -> Left (needs "a command")
  where
    needs n = BuiltinFailed "Shell.spawn" ("needs " <> n)
    pairOf (Key k, x) = (,) <$> string k <*> string x
-- ANCHOR_END: step

-- ANCHOR: quote
-- 🪶 A word as Lam's $"..." would read it back: as it is when it is
--    plain, in '...' when it has a space or a mark, in {"..."} when it
--    has a ' itself.
--      branch   'old fix'   {"it's"}
-- 語を、Lam の $"..." が読み戻せる形に。ふつうの語はそのまま、空白や
-- 記号があれば '...'、' そのものがあれば {"..."}
quoteWord :: Text -> Text
quoteWord w
  | not (T.null w), T.all plain w = w
  | T.any (== '\'') w = "{" <> renderString w <> "}"
  | otherwise = "'" <> T.concatMap brace w <> "'"
  where
    plain c = isAlphaNum c || c `elem` ("-_./=:,@+%()" :: String)
    brace c = case c of
      '{' -> "{{"
      '}' -> "}}"
      '"' -> "\\\""
      '\\' -> "\\\\"
      _   -> T.singleton c

-- 🐚 A command in one line, as $"..." would write it.
-- コマンドを、$"..." の書き方で1行に
renderCommand :: ShellCommand -> Text
renderCommand (ShellCommand name args dir env input) =
  "$\"" <> T.unwords (map quoteWord (name : args)) <> "\""
    <> (if T.null dir then "" else " (in " <> dir <> ")")
    <> T.concat [ " (" <> k <> "=" <> x <> ")" | (k, x) <- env ]
    <> (if T.null input then ""
        else " (" <> T.pack (show (T.length input)) <> " letters in)")
-- ANCHOR_END: quote

-- ANCHOR: width
-- 🎨 The text without the marks of styles (ESC [ ... m).
-- 見た目の印（ESC [ … m）を外した文字
stripStyles :: Text -> Text
stripStyles t = case T.breakOn "\ESC[" t of
  (before, "") -> before
  (before, rest) ->
    let after = T.drop 1 (T.dropWhile (/= 'm') (T.drop 2 rest))
    in before <> stripStyles after

-- 📏 How many columns a text takes on a terminal: the marks of styles
--    take none, wide letters (漢字, かな, full-width forms, most emoji)
--    take two, and the marks that join a letter take none. A rough
--    table, not all of Unicode's East Asian Width.
-- 端末で何桁を使うか：見た目の印は0桁、幅の広い文字（漢字、かな、全角、
-- たいていの絵文字）は2桁、前の文字にくっつく印は0桁。Unicode の
-- East Asian Width 全部ではない、おおまかな表
displayWidth :: Text -> Int
displayWidth = sum . map columns . T.unpack . stripStyles
  where
    columns c
      | joining (ord c) = 0
      | wide (ord c)    = 2
      | otherwise       = 1
    joining n = inRanges n
      [ (0x0300, 0x036F), (0x200B, 0x200F), (0x3099, 0x309A)
      , (0xFE00, 0xFE0F) ]
    wide n = inRanges n
      [ (0x1100, 0x115F), (0x2E80, 0x303E), (0x3041, 0x33FF)
      , (0x3400, 0x4DBF), (0x4E00, 0x9FFF), (0xA000, 0xA4CF)
      , (0xAC00, 0xD7A3), (0xF900, 0xFAFF), (0xFE30, 0xFE4F)
      , (0xFF00, 0xFF60), (0xFFE0, 0xFFE6), (0x1F300, 0x1F64F)
      , (0x1F900, 0x1F9FF), (0x20000, 0x3FFFD) ]
    inRanges n = any (\(lo, hi) -> lo <= n && n <= hi)
-- ANCHOR_END: width
-- ANCHOR_END: all
