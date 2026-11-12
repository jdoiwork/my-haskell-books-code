-- ANCHOR: all
-- | The shape of a table of contents, checked: no blank headings, no
--   heading twice among siblings, not too deep, not too many. Every
--   table that comes in (added by hand, or fixed by hand) goes through
--   this same check. (The page problems are only for the tables the
--   AI reads from a printed book: the MCP chapter added them.)
-- 目次の形を確かめる：空の見出しがない、兄弟に同じ見出しがない、深すぎない、
-- 多すぎない。入ってくる目次（手で足したものも、手で直したものも）が、
-- みな同じ確かめを通る（ページのまずいところは、AI が印刷された本から読んだ
-- 目次だけのもの。MCP の章で足した）
-- ANCHOR: header
module Tsundoku.TocCheck
  ( shapeProblems
  , newProblems
  , explainToc
  , maxDepth
  , maxItems
  ) where

-- lists, text, and trees
-- リスト、文字列、木
import Data.List (group, nub, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Forest, Tree (..))

import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: shape
-- 📏 How deep, and how many, at most.
-- 深さと数の上限
maxDepth, maxItems :: Int
maxDepth = 4
maxItems = 500

-- 🔍 Every problem with the shape of a tree of headings, all at once
--    (not only the first), each with the path to it, and each said once.
--    Two limits keep the answer small: too many items is said alone
--    (nothing else is counted), and too deep is said only where a
--    branch first goes too deep, not for every row below it.
-- 見出しの木の形のまずいところを、全部いっぺんに（最初の1つだけでなく）。
-- どれも、そこまでの道順つきで、1回ずつ。答えが大きくならないように：
-- 多すぎるときは、それだけを言う（ほかは数えない）。深すぎるのは、
-- 枝がはじめて深すぎた所だけを言う（その下の行は、言わない）
shapeProblems :: Forest Text -> [TocProblem]
shapeProblems forest
  | n > maxItems = [ TooMany n ]
  | otherwise    = details forest
  where
    n = length (walk [] forest)

-- 🆕 The problems a fixed table has that the table before it did not
--    have. Too many is said only when the rows grew, so a table that is
--    already too long can be cut down a little at a time; the other
--    problems are compared one by one even then (a long table does not
--    hide a new blank heading), and only the first ten are said, to keep
--    the answer small.
-- 直した目次の、もとの目次になかったまずい所。多すぎるのは、行が増えた
-- ときだけ言う（もう長すぎる目次を、少しずつ減らして直せる）。ほかの
-- まずい所は、そのときも1つずつくらべる（長い目次の陰に、新しい空の
-- 見出しを隠さない）。答えを小さく保つため、言うのは初めの10個まで
newProblems :: Forest Text -> Forest Text -> [TocProblem]
newProblems before after
  | n > maxItems, n > length (walk [] before) = [ TooMany n ]
  | n > maxItems = take 10 fresh
  | otherwise    = fresh
  where
    n     = length (walk [] after)
    fresh = [ p | p <- details after, p `notElem` details before ]

-- 🔍 The problems other than too many, each said once.
-- 多すぎる、のほかのまずい所を、1回ずつ
details :: Forest Text -> [TocProblem]
details forest = nub $ concat
  [ [ EmptyHeading above | (above, h) <- rows, blank h ]
  , [ SameHeading above h | (above, siblings) <- families
                          , h <- twice siblings ]
  , [ TooDeep (above ++ [h]) | (above, h) <- rows
                             , length above == maxDepth ]
  ]
  where
    rows     = [ (above, rootLabel t) | (above, t) <- walk [] forest ]
    families = ([], map rootLabel forest) :
      [ (above ++ [rootLabel t], map rootLabel (subForest t))
      | (above, t) <- walk [] forest, not (null (subForest t)) ]
    blank    = T.null . T.strip
    twice xs = [ x | x : _ : _ <- group (sort xs), not (blank x) ]

-- 🚶 Every tree, from the top, with the headings above it.
-- すべての木を上から順に、その上の見出しと一緒に
walk :: [Text] -> Forest Text -> [([Text], Tree Text)]
walk above = concatMap $ \t ->
  (above, t) : walk (above ++ [rootLabel t]) (subForest t)
-- ANCHOR_END: shape

-- ANCHOR: explain
-- 💬 A problem, in words that say what to fix.
-- まずいところを、何を直せばよいかがわかる言葉で
explainToc :: TocProblem -> Text
explainToc problem = case problem of
  NoChapters ->
    "目次が空です。章を1つ以上入れてください"
  TooMany n ->
    "項目が " <> num n <> " 個あります。一度に送れるのは "
      <> num maxItems <> " 個までです"
  EmptyHeading above ->
    where_ above <> "に、見出しが空の項目があります。見出しを入れてください"
  SameHeading above h ->
    where_ above <> "に「" <> h <> "」が2つあります。"
      <> "番号の打ちまちがいか、同じ章を2回入れていないか見てください"
  TooDeep path ->
    "「" <> trail path <> "」は深すぎます。深さは " <> num maxDepth
      <> " 段までにしてください"
  PageOutside path p ->
    "「" <> trail path <> "」の始まりのページ " <> num p
      <> " が、本の外です（1 から last_page まで）"
  PageBefore path p before q ->
    "「" <> trail path <> "」の始まりのページ " <> num p <> " が、前の「"
      <> before <> "」の " <> num q <> " より前です。ページを見直してください"
  where
    num      = T.pack . show
    trail    = T.intercalate " > "
    where_ [] = "目次のいちばん上"
    where_ ps = "「" <> trail ps <> "」の下"
-- ANCHOR_END: explain
-- ANCHOR_END: all
