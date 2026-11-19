{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | The builtins of the module Regex: find, replace and split. Their
--   names start with Regex., so the module Regex (written in Lam, in
--   Lam.Prelude) gets them, as Bytes. went to Bytes.
-- モジュール Regex の組み込みの関数：探す・置き換える・切り分ける。
-- 名前は Regex. で始まるので、Lam で書いたモジュール Regex（Lam.Prelude）
-- のものになる（Bytes. が Bytes のものになったのと同じ）。
module Lam.Regex
  ( regexBuiltins
  , tRegex
  , tMatch
  , matchesOf
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
-- 🏷️ Regex is a type of the language, like String and Bytes. Match is
--    a record: the module Regex names it with a type alias.
--      { text : String, start : Int, groups : [String],
--        named : Map String String }
-- Regex は、String や Bytes と同じく言語の型。Match はレコードで、
-- モジュール Regex が type alias で名前を付ける
tRegex, tMatch :: Type
tRegex = TCon "Regex" []
tMatch = TRecord (Map.fromList
  [ ("text",   tString)
  , ("start",  TInt)
  , ("groups", tList tString)
  , ("named",  tMap tString tString) ]) Nothing
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
      (_, Cut _ found) <- searchCut "Regex.matches" r s
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

  -- The same, but only the first match (what s/.../.../ opens into,
  -- without g, when its replacement has a group in it).
  -- 同じだが、最初に見つかったところだけ（g のない s/…/…/ で、置き換えに
  -- グループがあるとき、これに開く）
  , prim3 "Regex.replaceFirstWith"
      (tRegex ~> (tMatch ~> tString) ~> tString ~> tString) $
      \r f s -> do
        (p, cut@(Cut pieces found)) <- searchCut "Regex.replaceFirstWith" r s
        news <- case matchesOf p cut of
          m : _ -> (: map whole (drop 1 found)) <$> (apply f m >>= string)
          []    -> Right []
        Right (VString (stitch pieces news))

  -- What {last} and {1} in s/.../.../ open into:  $group "last" m.
  -- A group that took no part is "".
  -- s/…/…/ の {last} や {1} が開いてできる呼び出し
  , prim2 "$group" (tString ~> tMatch ~> tString) $ \k m -> do
      key <- string k
      case m of
        VRecord fields
          | Just (VString w)  <- Map.lookup "text" fields
          , Just (VList gs)   <- Map.lookup "groups" fields
          , Just (VMap names) <- Map.lookup "named" fields ->
          Right $ case T.unpack key of
            ds | not (null ds), all (`elem` ['0' .. '9']) ds ->
              let n = read ds :: Int
              in if n == 0 then VString w
                 else case drop (n - 1) gs of
                        g : _ -> g
                        []    -> VString ""
            _ -> Map.findWithDefault (VString "") (Key (VString key)) names
        _ -> Left (BuiltinFailed "$group" "got a value of the wrong type")

  -- Cut a string at every match:  Regex.split re", *" "a, b,c".
  -- 見つかったところで切る
  , prim2 "Regex.split" (tRegex ~> tString ~> tList tString) $ \r s -> do
      Cut pieces _ <- snd <$> searchCut "Regex.split" r s
      Right (VList (map VString pieces))
  ]
  where
    tResult e a = TCon "Result" [e, a]
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
-- 🎁 The matches as Lam records. Where a match starts is counted in
--    letters, from the pieces and the matches before it.
--    \d+ in "a1b22c":  { text = "1", start = 1, ... }  and
--                      { text = "22", start = 3, ... }.
-- 見つかったものを、Lam のレコードにする。始まる場所は、それより前の
-- 切れ端と、見つかったものの長さを足して、文字の数で数える
matchesOf :: Regex -> Cut -> [Value]
matchesOf p (Cut pieces found) = go 0 pieces found
  where
    go at (piece : rest) ((w :| groups) : more) =
      let start = at + T.length piece
      in matchValue start w groups : go (start + T.length w) rest more
    go _ _ _ = []

    matchValue start w groups = VRecord (Map.fromList
      [ ("text",   VString w)
      , ("start",  VInt (fromIntegral start))
      , ("groups", VList (map VString groups))
      , ("named",  VMap (Map.fromList
          [ (Key (VString n), VString (groups !! (i - 1)))
          | (n, i) <- regexNames p, i <= length groups ])) ])
-- ANCHOR_END: matchesOf
-- ANCHOR_END: all
