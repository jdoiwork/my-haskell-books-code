-- ANCHOR: all
-- | The types of tsundoku: works, tables of contents, notes, and reading.
-- tsundoku の型：作品、目次、感想、読書の状態
-- ANCHOR: head
module Tsundoku.Types
  ( -- * Tables of contents / 目次
    EntryId (..)
  , Pages (..)
  , Minutes (..)
  , Episodes (..)
  , Entry (..)
  , Toc
  , numbered
    -- * Works / 作品
  , Contents (..)
  , Work (..)
  , entryIds
    -- * Notes / 感想
  , Target (..)
  , Note (..)
    -- * Reading / 読書の状態
  , ReadingState (..)
  , Event (..)
    -- * Mistakes / まちがい
  , Mistake (..)
  ) where

-- ANCHOR: imports
-- Text, time, and the rose tree of containers
-- 文字列、日時、containers の rose tree
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Tree (Forest, Tree (..), flatten)
-- ANCHOR_END: imports
-- ANCHOR_END: head

-- ANCHOR: units
-- 📏 How much there is, in its own unit. Pages and minutes never add up.
-- 量。単位ごとに別の型なので、ページと分を足してしまうことはない
newtype Pages    = Pages Int    deriving (Show, Eq, Ord)
newtype Minutes  = Minutes Int  deriving (Show, Eq, Ord)
newtype Episodes = Episodes Int deriving (Show, Eq, Ord)
-- ANCHOR_END: units

-- ANCHOR: work
-- 📚 What a work is made of. Each kind has its own shape.
-- 作品の中身。種類ごとに形がちがう
data Contents
  = Book   (Toc Pages)      -- parts, chapters, sections / 部・章・節
  | Serial (Toc Episodes)   -- arcs and episodes / 章・話。話が増えていく
  | Series (Toc Minutes)    -- seasons and episodes / シーズン・話
  | Film   Minutes          -- just one piece / 1本だけ
  | Game                    -- no table of contents / 目次なし
  deriving (Show, Eq)

-- 📖 A work: its title, who made it, and what it is made of.
-- 作品：題、作った人、中身
data Work = Work
  { title    :: Text
  , creator  :: Text
  , contents :: Contents
  }
  deriving (Show, Eq)
-- ANCHOR_END: work

-- ANCHOR: entry
-- 🏷️ The number of an entry in a table of contents. Not just an Int,
--    so it never gets mixed up with a page count.
-- 目次の1項目の番号。ただの Int にしないので、ページ数と取りちがえない
newtype EntryId = EntryId Int
  deriving (Show, Eq, Ord)

-- 📑 One entry of a table of contents: its number, heading, and size.
--    `size` is the entry's own amount, not counting its children.
-- 目次の1項目：番号、見出し、量。size はその項目だけの量（子の量は含めない）
data Entry u = Entry
  { entryId :: EntryId
  , heading :: Text
  , size    :: u
  }
  deriving (Show, Eq)

-- 🌳 A table of contents is a forest: parts, chapters, sections...
-- 目次は森（木の並び）：部、章、節……
type Toc u = Forest (Entry u)
-- ANCHOR_END: entry

-- ANCHOR: numbered
-- 🔢 Give numbers 1, 2, 3... to an outline, from top to bottom.
-- 見出しと量だけの下書きに、上から順に 1, 2, 3... と番号を振る
numbered :: Forest (Text, u) -> Toc u
numbered outline = fst (numberForest 1 outline)

-- Number a forest, starting at `next`. Also return the next free number.
-- 森に next から番号を振る。次に空いている番号も返す
numberForest :: Int -> Forest (Text, u) -> (Toc u, Int)
numberForest next []           = ([], next)
numberForest next (tree : rest) =
  let (tree' , afterTree) = numberTree next tree
      (rest' , afterRest) = numberForest afterTree rest
  in  (tree' : rest', afterRest)

-- Number one tree: the root first, then its children.
-- 木を1本：まず根に、それから子に
numberTree :: Int -> Tree (Text, u) -> (Tree (Entry u), Int)
numberTree next (Node (title, amount) children) =
  let (children', after) = numberForest (next + 1) children
  in  (Node (Entry (EntryId next) title amount) children', after)
-- ANCHOR_END: numbered

-- ANCHOR: entryIds
-- 🔎 Every entry number of a work, from top to bottom.
-- 作品の目次の番号を、上から順に全部
entryIds :: Contents -> [EntryId]
entryIds what = case what of
  Book   toc -> idsOf toc
  Serial toc -> idsOf toc
  Series toc -> idsOf toc
  Film   _   -> []
  Game       -> []
  where
    idsOf toc = map entryId (concatMap flatten toc)
-- ANCHOR_END: entryIds

-- ANCHOR: note
-- 🎯 What a note is about: the whole work, or one entry of it.
-- 感想が何に付くか：作品全体か、目次の1項目か
data Target
  = OnWork
  | OnEntry EntryId
  deriving (Show, Eq)

-- 📝 A note: what it is about, what it says, when, and whether
--    it gives away the story.
-- 感想：何について、何を書いたか、いつ、ネタバレかどうか
data Note = Note
  { target    :: Target
  , body      :: Text
  , writtenAt :: UTCTime
  , spoiler   :: Bool
  }
  deriving (Show, Eq)
-- ANCHOR_END: note

-- ANCHOR: reading
-- 📗 Where I am with a work.
-- その作品を、いまどこまで読んでいるか
data ReadingState
  = Piled       -- on the pile, not started / 積読
  | Reading     -- reading now / 読書中
  | Finished    -- read to the end / 読了
  | Paused      -- stopped for now / 中断
  | Rereading   -- reading again after finishing / 再読
  deriving (Show, Eq, Ord, Enum, Bounded)

-- 🔔 What I do with a work.
-- 作品に対してすること
data Event
  = Start       -- start reading / 読み始める
  | Finish      -- read to the end / 読み終える
  | Stop        -- stop for now / 中断する
  | Resume      -- go on again / 再開する
  | Reread      -- read again from the start / 読み返す
  deriving (Show, Eq, Ord, Enum, Bounded)
-- ANCHOR_END: reading

-- ANCHOR: mistake
-- 🚫 Everything the core can refuse, with what was wrong.
-- 核が断ることのすべて。何がまずかったかを持つ
data Mistake
  = BadStars    Double             -- not 1-5 by halves / 1〜5、0.5刻みでない
  | BadScore    Int                -- not 0 to 100 / 0〜100 でない
  | NotAllowed  Event ReadingState -- no such move / その遷移はない
  | NoSuchEntry EntryId            -- not in the table / 目次にない
  | EmptyNote                      -- nothing written / 何も書いていない
  deriving (Show, Eq)
-- ANCHOR_END: mistake
-- ANCHOR_END: all
