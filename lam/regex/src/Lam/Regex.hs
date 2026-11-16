{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | The builtins for regular expressions: find, replace and split.
--   The names start with Regex., like Map. and Json.
-- 正規表現の組み込みの関数：探す・置き換える・切り分ける。
-- 名前は、Map. や Json. と同じく、Regex. で始める。
module Lam.Regex
  ( regexBuiltins
  , tRegex
  , tMatch
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Builtins
import Lam.Eval
import Lam.Pcre
import Lam.Syntax (Name)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: types
-- 🏷️ Regex is a builtin type, like String. Match is a data type of the
--    prelude:  data Match = Match String Int [String] (Map String).
-- Regex は、String と同じく組み込みの型。Match はプレリュードの data
tRegex, tMatch :: Type
tRegex = TCon "Regex" []
tMatch = TCon "Match" []
-- ANCHOR_END: types

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins. The text to search
--    always comes last, so  Regex.findAll digits  is a function that
--    can be used again and again.
-- 組み込みの関数の表に足す行。探す文字列は、いつも最後に置く。
-- Regex.findAll digits は、何度でも使える関数になる
regexBuiltins :: [Builtin]
regexBuiltins =
  -- What re"\d+" opens into:  $regex "\\d+". The parser has already
  -- compiled it once, to find a mistake before the program runs, so
  -- this never fails. ($ cannot start a name in Lam: no program can
  -- call it.)
  -- re"\d+" が開いてできる呼び出し。パーサーが一度コンパイルして、
  -- まちがいを動かす前に見つけてあるので、ここでは失敗しない
  -- （Lam の名前は $ で始められないので、プログラムからは呼べない）
  [ prim1 "$regex" (tString ~> tRegex) $ \v -> do
      source <- string v
      case compileRegex source of
        Right p  -> Right (VRegex p)
        Left msg -> Left (BuiltinFailed "re\"...\"" msg)

  -- Make a regex from a string, while the program runs.
  -- 動いているときに、文字列から正規表現を作る
  , prim1 "Regex.compile" (tString ~> tResult tString tRegex) $ \v -> do
      source <- string v
      Right $ case compileRegex source of
        Right p  -> con "Ok" [VRegex p]
        Left msg -> con "Err" [VString msg]

  -- Look for it.
  -- 探す
  , prim2 "Regex.matches" (tRegex ~> tString ~> TBool) $ \r s -> do
      (_, found) <- search "Regex.matches" r s
      Right (VBool (not (null found)))
  , prim2 "Regex.find" (tRegex ~> tString ~> tMaybe tMatch) $ \r s -> do
      (p, cut) <- searchCut "Regex.find" r s
      Right $ case matchesOf p cut of
        m : _ -> con "Just" [m]
        []    -> con "Nothing" []
  , prim2 "Regex.findAll" (tRegex ~> tString ~> tList tMatch) $ \r s -> do
      (p, cut) <- searchCut "Regex.findAll" r s
      Right (VList (matchesOf p cut))

  -- Put a string in place of the first match, or of every match.
  -- The string is used as it is ($1 and \1 mean nothing).
  -- 最初に見つかったところ、または見つかったところ全部を、文字列で
  -- 置き換える。文字列は、そのまま使う（$1 や \1 に意味はない）
  , prim3 "Regex.replace" (tRegex ~> tString ~> tString ~> tString) $
      \r new s -> do
        Cut pieces found <- snd <$> searchCut "Regex.replace" r s
        with <- string new
        Right (VString (stitch pieces (with : map whole (drop 1 found))))
  , prim3 "Regex.replaceAll" (tRegex ~> tString ~> tString ~> tString) $
      \r new s -> do
        Cut pieces _ <- snd <$> searchCut "Regex.replaceAll" r s
        with <- string new
        Right (VString (T.intercalate with pieces))

  -- Put in place of every match what a Lam function makes of it.
  -- 見つかったところ全部を、Lam の関数が作った文字列で置き換える
  , prim3 "Regex.replaceWith"
      (tRegex ~> (tMatch ~> tString) ~> tString ~> tString) $
      \r f s -> do
        (p, cut@(Cut pieces _)) <- searchCut "Regex.replaceWith" r s
        news <- traverse (\m -> apply f m >>= string) (matchesOf p cut)
        Right (VString (stitch pieces news))

  -- Cut a string at every match:  Regex.split re", *" "a, b,c".
  -- 見つかったところで切る
  , prim2 "Regex.split" (tRegex ~> tString ~> tList tString) $ \r s -> do
      Cut pieces _ <- snd <$> searchCut "Regex.split" r s
      Right (VList (map VString pieces))
  ]
  where
    tMaybe t = TCon "Maybe" [t]
    tResult e a = TCon "Result" [e, a]
    search what r s = do
      (_, Cut pieces found) <- searchCut what r s
      Right (pieces, found)
-- ANCHOR_END: table

-- ANCHOR: search
-- 🔍 Search a text with a regex. A search can fail as it runs, too:
--    PCRE2 gives up when it has tried too many ways (see the chapter).
-- 正規表現で文字列を探す。探すのも失敗することがある。PCRE2 は、
-- 試した道が多すぎると、あきらめる（本文を参照）
searchCut :: Name -> Value -> Value -> Either EvalError (Regex, Cut)
searchCut what r s = do
  p <- case r of
    VRegex p -> Right p
    _        -> Left (BuiltinFailed what "got a value of the wrong type")
  subject <- string s
  case regexSearch p subject of
    Right cut -> Right (p, cut)
    Left msg  -> Left (BuiltinFailed what msg)

-- 🧵 Sew the pieces back together, with a string between each two:
--    stitch ["a", "b", "c"] ["1", "2"]  is  "a1b2c".
-- 切れ端を、間に文字列をはさんで、縫い合わせる
stitch :: [Text] -> [Text] -> Text
stitch pieces between = case (pieces, between) of
  (p : ps@(_ : _), b : bs) -> p <> b <> stitch ps bs
  _                        -> T.concat pieces

whole :: NonEmpty Text -> Text
whole (w :| _) = w
-- ANCHOR_END: search

-- ANCHOR: matchesOf
-- 🎁 The matches as Lam values. Where a match starts is counted in
--    letters, from the pieces and the matches before it.
--    \d+ in "a1b22c":  Match "1" 1 [] {}  and  Match "22" 3 [] {}.
-- 見つかったものを、Lam の値にする。始まる場所は、それより前の
-- 切れ端と、見つかったものの長さを足して、文字の数で数える
matchesOf :: Regex -> Cut -> [Value]
matchesOf p (Cut pieces found) = go 0 pieces found
  where
    go at (piece : rest) ((w :| groups) : more) =
      let start = at + T.length piece
      in matchValue start w groups : go (start + T.length w) rest more
    go _ _ _ = []

    matchValue start w groups = con "Match"
      [ VString w
      , VInt (fromIntegral start)
      , VList (map VString groups)
      , VMap (Map.fromList
          [ (n, VString (groups !! (i - 1)))
          | (n, i) <- regexNames p, i <= length groups ])
      ]
-- ANCHOR_END: matchesOf
-- ANCHOR_END: all
