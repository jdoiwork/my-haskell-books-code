-- ANCHOR: all
-- | Show a tree: draw it for people to read, or write it back as a
--   pattern that reads into the same tree.
-- 木を見せる：人が読むために描く。または、同じ木に読めるパターンの
-- 文字列に書き戻す
module MiniRegex.Pretty
  ( render
  , renderQuantifier
  , drawPattern
  ) where

-- ANCHOR: imports
import Data.Char (isPrint)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))

import MiniRegex.Syntax
-- ANCHOR_END: imports

-- ANCHOR: render
-- ✍️ Write a tree back as a pattern. Signs that have a meaning get a
--    \ before them, so that parsing the answer gives the same tree.
-- 木を、パターンの文字列に書き戻す。意味のある記号には \ を付けるので、
-- 書き戻したものを読むと、同じ木になる
render :: Pattern -> Text
render (Pattern branches) =
  T.intercalate "|" (map branch (NE.toList branches))
  where
    branch (Branch ps)        = T.concat (map piece ps)
    piece (Anchor LineStart)  = "^"
    piece (Anchor LineEnd)    = "$"
    piece (Quantified a q)    = atom a <> renderQuantifier q
    atom (Lit c)              = escapeOutside c
    atom AnyChar              = "."
    atom (Class cls)          = renderClass cls
    atom (Group kind inner)   = open kind <> render inner <> ")"
    -- ANCHOR: renderref
    -- \g{n}, not \n: \12 could be read as octal, and a digit after
    -- \1 would join it
    -- \n ではなく \g{n}：\12 は8進と読まれうるし、\1 のうしろの
    -- 数字は、くっついてしまう
    atom (Backref n)          = "\\g{" <> T.pack (show n) <> "}"
    -- ANCHOR_END: renderref
    open Numbered             = "("
    open (Named n)            = "(?<" <> n <> ">"
    open NonCapturing         = "(?:"

-- 🔁 The quantifier as written: "", *, +, ?, {n}, {n,}, {n,m}, and
--    then ? or + for Lazy and Possessive. Once, but not greedy, has to
--    be written as {1}? (a? would mean "zero or one time").
-- 量指定子の書き方：""・*・+・?・{n}・{n,}・{n,m}、そのあとに控えめなら ?、
-- 所有的なら +。1回で欲ばりでないものは {1}? と書く（a? と書くと
-- 「0回か1回」になってしまう）
renderQuantifier :: Quantifier -> Text
renderQuantifier (Quantifier c g) = base <> suffix
  where
    base = case c of
      Count 1 (Just 0) | g == Greedy -> ""
      Count 0 Nothing                -> "*"
      Count 1 Nothing                -> "+"
      Count 0 (Just 1)               -> "?"
      Count n (Just 0)               -> "{" <> num n <> "}"
      Count n Nothing                -> "{" <> num n <> ",}"
      Count n (Just e)               -> "{" <> num n <> ","
                                          <> num (n + e) <> "}"
    suffix = case g of
      Greedy     -> ""
      Lazy       -> "?"
      Possessive -> "+"
    num = T.pack . show

-- 🔤 [ ... ]. In a class, only \ ] ^ - and [ need a \ (and : . = at
--    the head).
-- [ ... ]。[ ] の中で \ が要るのは、\ ] ^ - [ だけ（頭なら : . = も）
renderClass :: CharClass -> Text
renderClass (CharClass neg (i :| is)) = "[" <> hat <> body <> "]"
  where
    hat  = if neg then "^" else ""
    body = item firstOne i <> T.concat (map (item inside) is)
    item f (Single c)  = f c
    item f (Range a b) = f a <> "-" <> inside b
    inside c
      | c `elem` ("\\]^-[" :: String) = T.pack ['\\', c]
      | otherwise                    = T.singleton c
    -- [:x:] at the head would read as a POSIX name; \: never does
    -- 頭の [:x:] は POSIX の名前と読まれる。\: なら、そうならない
    firstOne c
      | c `elem` (":.=" :: String) && not neg = T.pack ['\\', c]
      | otherwise                              = inside c

-- 🛡️ A character outside [ ], with a \ when it has a meaning.
-- [ ] の外の文字。意味のある記号なら \ を付ける
escapeOutside :: Char -> Text
escapeOutside c
  | c `elem` ("\\^$.|?*+()[{" :: String) = T.pack ['\\', c]
  | otherwise                           = T.singleton c
-- ANCHOR_END: render

-- ANCHOR: draw
-- 🌳 Draw a tree with lines, one piece on a line.
-- 木を線で描く。1行に部品1つ
drawPattern :: Pattern -> Text
drawPattern = T.unlines . draw . patternTree

-- 🌱 The tree as a Data.Tree of labels.
-- 木を、ラベルの Data.Tree にする
patternTree :: Pattern -> Tree Text
patternTree (Pattern (b :| [])) = branchTree b
patternTree (Pattern bs)        = Node "or" (map branchTree (NE.toList bs))

branchTree :: Branch -> Tree Text
branchTree (Branch ps) = Node "branch" (map pieceTree ps)

pieceTree :: Piece -> Tree Text
pieceTree (Anchor LineStart) = Node "^ line start" []
pieceTree (Anchor LineEnd)   = Node "$ line end" []
pieceTree (Quantified a q)   = case a of
  Group kind inner -> Node ("group " <> groupName kind <> times)
                           [patternTree inner]
  _                -> Node (label a <> times) []
  where
    times = case renderQuantifier q of
      "" -> ""
      t  -> "  " <> t
    label (Lit c)
      | isPrint c     = T.pack ['\'', c, '\'']
      | otherwise     = T.pack (show c)  -- '\t', '\n' ...
    label AnyChar     = ". any"
    label (Class cls) = renderClass cls
    label (Group _ _) = "group"
    label (Backref n) = "\\" <> T.pack (show n) <> " same as group "
                          <> T.pack (show n)
    groupName Numbered     = "(numbered)"
    groupName (Named n)    = "<" <> n <> ">"
    groupName NonCapturing = "(?:)"

-- ✏️ The lines of a tree: ├─ for a child with more after it, └─ for
--    the last one.
-- 木の行。あとに続く子には ├─、最後の子には └─
draw :: Tree Text -> [Text]
draw (Node top kids) = top : concat (zipWith child [1 :: Int ..] kids)
  where
    n = length kids
    child i kid =
      let (mark, more) = if i == n then ("└─ ", "   ") else ("├─ ", "│  ")
      in  case draw kid of
            first : rest -> (mark <> first) : map (more <>) rest
            []           -> []
-- ANCHOR_END: draw
-- ANCHOR_END: all
