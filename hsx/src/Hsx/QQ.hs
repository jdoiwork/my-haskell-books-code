{-# LANGUAGE TemplateHaskell #-}
-- ANCHOR: all
-- | The quasiquoter [hsx| ... |]: turns the tree into Haskell code.
-- 準クォート [hsx| ... |]：木を Haskell のコードに変える。
module Hsx.QQ (hsx) where

import qualified Data.Text as T
import Data.Char (isUpper)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Hsx.Html (escapeText, raw, toAttr, toHtml)
import Hsx.Parse

-- ANCHOR: hsx
-- ✨ [hsx|<p>Hello, {name}!</p>|]
hsx :: QuasiQuoter
hsx = QuasiQuoter
  { quoteExp  = compileHsx
  , quotePat  = notHere "a pattern"
  , quoteType = notHere "a type"
  , quoteDec  = notHere "a declaration"
  }
  where
    notHere what _ = fail ("hsx cannot be used as " <> what)

-- 🏭 Parse at compile time. A mistake becomes a compile error.
-- コンパイルのときに読む。まちがいはコンパイルエラーになる
compileHsx :: String -> Q Exp
compileHsx src = do
  -- The template starts right after "[hsx|" (5 characters).
  -- テンプレートは "[hsx|"（5文字）のすぐ後ろから始まる
  (l, c) <- loc_start <$> location
  case parseHsxAt (l, c + 5) src of
    Left err -> fail (renderError err)
    Right ns -> do
      let pieces = mergeStatic (concatMap nodePieces ns)
      [| mconcat $(listE (map pieceExp pieces)) |]
-- ANCHOR_END: hsx

-- ANCHOR: pieces
-- 🧩 The template becomes a row of pieces: fixed text
--    (already escaped) and holes filled at run time.
-- テンプレートは、部品の列になる：決まった文字
-- （エスケープ済み）と、動かすときに埋める穴
data Piece
  = Fixed String       -- "<p>Hello, "
  | HtmlHole [String]  -- {name}
  | AttrHole [String]  -- class={cls}

nodePieces :: Node -> [Piece]
nodePieces (TextNode t) = [Fixed (escape t)]
nodePieces (Splice ws)  = [HtmlHole ws]
nodePieces (Element tag attrs children)
  | tag `elem` voidElements, null children =
      [Fixed ("<" <> tag)] <> concatMap attrPieces attrs
        <> [Fixed ">"]
  | otherwise =
      [Fixed ("<" <> tag)] <> concatMap attrPieces attrs
        <> [Fixed ">"] <> concatMap nodePieces children
        <> [Fixed ("</" <> tag <> ">")]

attrPieces :: Attr -> [Piece]
attrPieces (Attr name Bare) = [Fixed (" " <> name)]
attrPieces (Attr name (Static v)) =
  [Fixed (" " <> name <> "=\"" <> escape v <> "\"")]
attrPieces (Attr name (Dynamic ws)) =
  [Fixed (" " <> name <> "=\""), AttrHole ws, Fixed "\""]

escape :: String -> String
escape s = T.unpack (escapeText (T.pack s))

-- 🧲 Glue neighbouring fixed pieces into one.
-- となりあう決まった文字を、1つにまとめる
mergeStatic :: [Piece] -> [Piece]
mergeStatic (Fixed a : Fixed b : ps) = mergeStatic (Fixed (a <> b) : ps)
mergeStatic (p : ps)                 = p : mergeStatic ps
mergeStatic []                       = []
-- ANCHOR_END: pieces

-- ANCHOR: pieceExp
-- 🔧 Each piece becomes a Haskell expression of type Html.
-- 部品ひとつが、Html 型の Haskell の式になる
pieceExp :: Piece -> Q Exp
pieceExp (Fixed s)     = [| raw (T.pack s) |]
pieceExp (HtmlHole ws) = [| toHtml $(spliceExp ws) |]
pieceExp (AttrHole ws) = [| raw (toAttr $(spliceExp ws)) |]

-- {show level} is the function call  show level.
-- {show level} は、関数の呼び出し show level になる
spliceExp :: [String] -> Q Exp
spliceExp ws = pure (foldl1 AppE (map nameExp ws))
  where
    nameExp w
      | isConstructor w = ConE (mkName w)
      | otherwise       = VarE (mkName w)
    -- Data.Text.pack is a function, Just is a constructor.
    -- Data.Text.pack は関数、Just はコンストラクター
    isConstructor w = case reverse (splitDots w) of
      ((c : _) : _) -> isUpper c
      _             -> False
    splitDots w = case break (== '.') w of
      (a, [])       -> [a]
      (a, _ : more) -> a : splitDots more
-- ANCHOR_END: pieceExp
-- ANCHOR_END: all
