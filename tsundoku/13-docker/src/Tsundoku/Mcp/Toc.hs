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
import Data.Char (ord)
import Data.List (group, sort)
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
-- 引数から読む。creator と children は、なくてもよい
instance FromJSON TocDraft where
  parseJSON = withObject "TocDraft" $ \o -> TocDraft
    <$> o .:  "title"
    <*> o .:? "creator" .!= ""
    <*> o .:  "last_page"
    <*> o .:  "toc"

instance FromJSON Chapter where
  parseJSON = withObject "Chapter" $ \o -> Chapter
    <$> o .:  "heading"
    <*> o .:  "page"
    <*> o .:? "children" .!= []
-- ANCHOR_END: draft

-- ANCHOR: problems
-- 🚧 What can be wrong with the tree. Each one says where (the path of
--    headings from the top).
-- 木のまずいところ。どれも、どこか（上からの見出しの道順）を言う
data TocProblem
  = NoChapters                              -- the table is empty / 空
  | TooMany      Int                        -- too many items / 多すぎる
  | EmptyHeading [Text]                     -- under this path / この下に
  | SameHeading  [Text] Text                -- twice under one path / 2回
  | TooDeep      [Text]                     -- deeper than maxDepth
  | PageOutside  [Text] Int                 -- before 1 or after the end
  | PageBefore   [Text] Int Text Int        -- starts before the one above
  deriving (Show, Eq)

-- 📏 How deep, and how many, at most.
-- 深さと数の上限
maxDepth, maxItems :: Int
maxDepth = 4
maxItems = 500

-- 🔍 Every problem of the tree, all at once (not only the first), so
--    that the AI can fix them in one go.
-- 木のまずいところを、全部いっぺんに（最初の1つだけでなく）。AI が
-- 一度に直せるように
tocProblems :: TocDraft -> [TocProblem]
tocProblems d = concat
  [ [ NoChapters | null d.toc ]
  , [ TooMany (length rows) | length rows > maxItems ]
  , [ EmptyHeading above | (above, c) <- rows, T.null (T.strip c.heading) ]
  , [ SameHeading above h | (above, siblings) <- families
                          , h <- twice (map (.heading) siblings) ]
  , [ TooDeep (pathOf above c) | (above, c) <- rows
                               , length above >= maxDepth ]
  , [ PageOutside (pathOf above c) c.page
    | (above, c) <- rows, c.page < 1 || c.page > d.lastPage ]
  , [ PageBefore (pathOf above c) c.page before.heading before.page
    | ((_, before), (above, c)) <- zip rows (drop 1 rows)
    , c.page < before.page ]
  ]
  where
    rows     = walk [] d.toc
    families = ([], d.toc) : [ (pathOf above c, c.children)
                             | (above, c) <- rows, not (null c.children) ]
    pathOf above c = above ++ [c.heading]
    twice xs = [ x | x : _ : _ <- group (sort xs), not (T.null (T.strip x)) ]

-- 🚶 Every chapter, from the top, with the headings above it.
-- すべての章を上から順に、その上の見出しと一緒に
walk :: [Text] -> [Chapter] -> [([Text], Chapter)]
walk above = concatMap $ \c ->
  (above, c) : walk (above ++ [c.heading]) c.children
-- ANCHOR_END: problems

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
