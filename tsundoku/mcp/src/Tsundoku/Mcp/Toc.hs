-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-- | A table of contents the AI hands us, read from a photo or a PDF. The
--   AI can skip a chapter, write a number twice, or mix up the pages, so
--   the tree is checked before anything is added, and every problem is
--   said with the path to it, in words the AI can act on.
-- AI が渡してくる目次（写真や PDF から読んだもの）。AI は章を飛ばしたり、
-- 番号を2回書いたり、ページを取りちがえたりする。だから、何かを足す前に
-- 木を確かめる。まずい所は全部、そこまでの道順と一緒に、AI が直せる言葉で言う
module Tsundoku.Mcp.Toc
  ( TocDraft (..)
  , Chapter (..)
  , TocProblem (..)
  , tocProblems
  , explainToc
  , draftWork
  , preview
  , workKey
  , Plan (..)
  , planToc
  , maxDepth
  , maxItems
  ) where

-- bits and words for the key, lists, text, and trees
-- 鍵のためのビットと数、リスト、文字列、木
import Data.Bits (xor)
import Data.Char (isControl, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import Data.Word (Word64)
import Numeric (showHex)

import Data.Aeson

import Tsundoku.Card (Card (..), checkWork)
import Tsundoku.Form (explain)
import Tsundoku.Import.Types (fromStarts)
import Tsundoku.Outline (outline)
import Tsundoku.TocCheck (explainToc, maxDepth, maxItems, shapeProblems)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: draft
-- 📥 What the AI sends: the book, and its chapters with the page each
--    one starts on (that is what a printed table of contents shows).
-- AI が送ってくるもの：本と、章ごとの始まりのページ（印刷された目次に
-- 書いてあるのは、それ）
data TocDraft = TocDraft
  { title    :: Text
  , creator  :: Text
  , lastPage :: Int          -- the last page of the book / 本の最後のページ
  , toc      :: [Chapter]
  }
  deriving (Show, Eq)

data Chapter = Chapter
  { heading  :: Text
  , page     :: Int          -- where it starts / 始まるページ
  , children :: [Chapter]
  }
  deriving (Show, Eq)

-- 📥 Read from the arguments. creator and children may be left out.
--    A line break in a title, a creator, or a heading becomes a space:
--    the draft is shown line by line, and a line put in there would look
--    like one of ours ("key: …").
-- 引数から読む。creator と children は、なくてもよい。題・作者・見出しの
-- 改行は空白にする。下書きは1行ずつ見せるので、そこに入れた行は、こちらの
-- 行（「key: …」）に見えてしまう
instance FromJSON TocDraft where
  parseJSON = withObject "TocDraft" $ \o -> TocDraft
    <$> (oneLine <$> o .:  "title")
    <*> (oneLine <$> o .:? "creator" .!= "")
    <*> o .:  "last_page"
    <*> o .:  "toc"

instance FromJSON Chapter where
  parseJSON = withObject "Chapter" $ \o -> Chapter
    <$> (oneLine <$> o .:  "heading")
    <*> o .:  "page"
    <*> o .:? "children" .!= []

-- ↩️ Control characters (line breaks, tabs) to spaces.
-- 制御文字（改行・タブ）を空白に
oneLine :: Text -> Text
oneLine = T.map (\c -> if isControl c then ' ' else c)
-- ANCHOR_END: draft

-- ANCHOR: problems
-- 🔍 Every problem of the tree, all at once (not only the first), so
--    that the AI can fix them in one go. The shape is checked as every
--    table is (TocCheck); the pages only here.
-- 木のまずいところを、全部いっぺんに（最初の1つだけでなく）。AI が
-- 一度に直せるように。形は、どの目次とも同じように確かめ（TocCheck）、
-- ページはここだけで確かめる
tocProblems :: TocDraft -> [TocProblem]
tocProblems d
  | any tooBig shape = [ NoChapters | null d.toc ] ++ shape
  | otherwise        = concat
      [ [ NoChapters | null d.toc ]
      , shape
      , [ PageOutside (pathOf above c) c.page
        | (above, c) <- rows, c.page < 1 || c.page > d.lastPage ]
      , [ PageBefore (pathOf above c) c.page before.heading before.page
        | ((_, before), (above, c)) <- zip rows (drop 1 rows)
        , c.page < before.page ]
      ]
  where
    -- too many or too deep: say only that (fix the shape first). The
    -- pages of every row, each with its whole path, would make the
    -- answer grow with the square of the depth
    -- 多すぎ・深すぎ：それだけを言う（まず形を直す）。どの行のページにも
    -- 道順をまるごと付けると、答えが深さの2乗でふくらむ
    shape = shapeProblems (map headings d.toc)
    tooBig p = case p of
      TooMany _ -> True
      TooDeep _ -> True
      _         -> False
    rows = walk [] d.toc
    pathOf above c = above ++ [c.heading]
    headings c = Node c.heading (map headings c.children)

-- 🚶 Every chapter, from the top, with the headings above it.
-- すべての章を上から順に、その上の見出しと一緒に
walk :: [Text] -> [Chapter] -> [([Text], Chapter)]
walk above = concatMap $ \c ->
  (above, c) : walk (above ++ [c.heading]) c.children
-- ANCHOR_END: problems

-- ANCHOR: work
-- 📗 The draft as a book, if the tree has no problems. Pages come from
--    where each chapter starts (as with PDF bookmarks).
-- 木にまずいところがなければ、下書きを本に。ページ数は、章ごとの始まりの
-- ページから（PDF のしおりと同じ）
draftWork :: TocDraft -> Either [TocProblem] Work
draftWork d = case tocProblems d of
  []       -> Right (Work d.title d.creator (Book (numbered pages)))
  problems -> Left problems
  where
    pages = fromStarts d.lastPage (map toTree d.toc)
    toTree c = Node (c.heading, c.page) (map toTree c.children)

-- 👀 The book as lines to show before adding it: depth, pages, heading.
-- 足す前に見せる、本の行：深さ、ページ数、見出し
preview :: Work -> [Text]
preview w = titleLine : case w.contents of
  Book toc -> [ T.replicate depth "  " <> "・" <> e.heading
                <> "（" <> T.pack (show n) <> "ページ）"
              | (depth, e) <- outline toc, let Pages n = e.size ]
  _        -> []
  where
    -- No empty brackets when the creator is not known.
    -- 作者がわからないときは、空のかっこを出さない
    titleLine
      | T.null w.creator = w.title
      | otherwise        = w.title <> "（" <> w.creator <> "）"
-- ANCHOR_END: work

-- ANCHOR: key
-- 🔑 A key made from the shape of the work: its title, its creator, and
--    the headings and sizes of its table, but not the numbers the shelf
--    gave them. The same table sent twice has the same key.
--    (FNV-1a, 64 bits: a small, well-known hash; this is not for secrets.)
-- 作品の形から作る鍵：題、作者、目次の見出しと量。本棚が振った番号は
-- 入れない。同じ目次を2回送れば、同じ鍵になる。
-- （FNV-1a の 64 ビット：小さくて、よく知られたハッシュ。秘密を守るための
-- ものではない）
workKey :: Work -> Text
workKey w = T.justifyRight 16 '0' (T.pack (showHex (fnv1a shape) ""))
  where
    shape = T.intercalate "\n" (w.title : w.creator : rows w.contents)
    rows contents = case contents of
      Book   t         -> tocRows t
      Serial t         -> tocRows t
      Series t         -> tocRows t
      Film (Minutes n) -> ["film " <> T.pack (show n)]
      Game             -> ["game"]
    tocRows :: Amount u => Toc u -> [Text]
    tocRows t =
      [ T.unwords [shown depth, e.heading, shown (count e.size)]
      | (depth, e) <- outline t ]
    shown = T.pack . show

-- 🔢 FNV-1a over the characters of a text.
-- 文字列の文字の上の FNV-1a
fnv1a :: Text -> Word64
fnv1a = T.foldl' step 14695981039346656037
  where
    step h c = (h `xor` fromIntegral (ord c)) * 1099511628211
-- ANCHOR_END: key

-- ANCHOR: plan
-- 🧭 What to do with a draft, given the shelf: say what is wrong, say it
--    is there already, or get ready to add it (with its key).
-- 下書きをどうするか（本棚を見て）：まずいところを言う、もうあると言う、
-- 足す用意をする（鍵と一緒に）
data Plan
  = Problems [Text]
  | Already  WorkId
  | Ready    Work Text
  deriving (Show, Eq)

planToc :: [(WorkId, Card)] -> TocDraft -> Plan
planToc shelf d = case draftWork d of
  Left problems -> Problems (map explainToc problems)
  Right w       -> case checkWork w of
    Left mistake -> Problems (explain mistake)
    Right good   ->
      case [ wid | (wid, c) <- shelf, workKey c.work == workKey good ] of
        wid : _ -> Already wid
        []      -> Ready good (workKey good)
-- ANCHOR_END: plan
-- ANCHOR_END: all
