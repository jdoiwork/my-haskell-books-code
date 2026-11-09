-- ANCHOR: all
-- | The types of tsundoku: works, tables of contents, notes, reading,
--   and the reading log.
-- tsundoku の型：作品、目次、感想、読書の状態、読書ログ
-- ANCHOR: head
module Tsundoku.Types
  ( -- * Tables of contents / 目次
    EntryId (..)
  , Pages (..)
  , Minutes (..)
  , Episodes (..)
  , Amount (..)
  , Entry (..)
  , Toc
  , numbered
    -- * Works / 作品
  , WorkId (..)
  , Contents (..)
  , Work (..)
  , entryIds
    -- * Notes / 感想
  , Target (..)
  , Note (..)
    -- * Reading / 読書の状態
  , ReadingState (..)
  , Event (..)
  , LogLine (..)
    -- * Attributes and states of my own / 自分で決める属性と状態
  , AttrKind (..)
  , AttrDef (..)
  , AttrValue (..)
  , Flow (..)
  , flowStates
  , Schema (..)
  , AttrProblem (..)
  , DefProblem (..)
    -- * Mistakes / まちがい
  , Mistake (..)
  , Checked
  ) where

-- ANCHOR: imports
-- Text, time, and the rose tree of containers
-- 文字列、日時、containers の rose tree
import Data.Text (Text)
import Data.Time (Day, UTCTime)
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

-- ANCHOR: monoids
-- ➕ Amounts of the same unit add up. Nothing at all is 0.
-- 同じ単位の量は、足せる。何もないのは 0
instance Semigroup Pages where
  Pages a <> Pages b = Pages (a + b)
instance Monoid Pages where
  mempty = Pages 0

instance Semigroup Minutes where
  Minutes a <> Minutes b = Minutes (a + b)
instance Monoid Minutes where
  mempty = Minutes 0

instance Semigroup Episodes where
  Episodes a <> Episodes b = Episodes (a + b)
instance Monoid Episodes where
  mempty = Episodes 0
-- ANCHOR_END: monoids

-- ANCHOR: amount
-- 🧮 A unit we can count in: take the number out, put a number in,
--    and say the name of the unit (the argument is only for its type).
-- 数えられる単位：数を取り出す、数から作る、単位の名前を言う
-- （unitName の引数は、型を伝えるためだけのもの）
class Monoid u => Amount u where
  count    :: u -> Int
  amount   :: Int -> u
  unitName :: u -> Text

instance Amount Pages where
  count (Pages n) = n
  amount          = Pages
  unitName _      = "ページ"

instance Amount Minutes where
  count (Minutes n) = n
  amount            = Minutes
  unitName _        = "分"

instance Amount Episodes where
  count (Episodes n) = n
  amount             = Episodes
  unitName _         = "話"
-- ANCHOR_END: amount

-- ANCHOR: work
-- ANCHOR: workId
-- 🪪 The number of a work on the shelf. The store gives it.
-- 本棚の作品の番号。振るのは、しまう場所（窓口の向こう）
newtype WorkId = WorkId Int
  deriving (Show, Eq, Ord)
-- ANCHOR_END: workId

-- 📚 What a work is made of. Each kind has its own shape.
-- 作品の中身。種類ごとに形がちがう
data Contents
  = Book   (Toc Pages)      -- parts, chapters, sections / 部・章・節
  | Serial (Toc Episodes)   -- arcs and episodes / 章・話。話が増えていく
  | Series (Toc Minutes)    -- dramas, anime / ドラマ・アニメ：シーズン・話
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
  deriving (Show, Eq, Functor)

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
numberTree next (Node (title, howMuch) children) =
  let (children', after) = numberForest (next + 1) children
  in  (Node (Entry (EntryId next) title howMuch) children', after)
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
  deriving (Show, Eq, Ord)

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

-- ANCHOR: logLine
-- 📓 One line of the reading log: on this day, I read this entry
--    to the end.
-- 読書ログの1行：この日に、この項目を読み終えた
data LogLine = LogLine
  { readOn    :: Day
  , readEntry :: EntryId
  }
  deriving (Show, Eq)
-- ANCHOR_END: logLine

-- ANCHOR: attr
-- 🧩 The kinds of attribute I can make: text, a number, a date,
--    yes or no, or one of a few choices.
-- 自分で作れる属性の種類：文字、数、日付、はい/いいえ、選択肢
data AttrKind
  = TextKind               -- どこで買った：古本市
  | NumberKind             -- 推し度：5
  | DateKind               -- 買った日：2026-11-01
  | YesNoKind              -- サイン本：はい
  | ChoiceKind [Text]      -- 形：紙・電子・Web のどれか
  deriving (Show, Eq)

-- 📐 The definition of an attribute: its name and its kind.
-- 属性の定義：名前と種類
data AttrDef = AttrDef
  { attrName :: Text
  , attrKind :: AttrKind
  }
  deriving (Show, Eq)

-- 🎁 A value of an attribute, one constructor for each kind.
-- 属性の値。種類ごとに1つのコンストラクター
data AttrValue
  = TextV   Text
  | NumberV Int
  | DateV   Day
  | YesNoV  Bool
  | ChoiceV Text
  deriving (Show, Eq)
-- ANCHOR_END: attr

-- ANCHOR: flow
-- 🔀 States of my own, as a small map: where it starts, the other
--    states, and the arrows I may follow (from, to).
-- 自分で決める状態を、小さな地図に：はじめの状態、ほかの状態、
-- たどってよい矢印（元, 先）
data Flow = Flow
  { flowName :: Text
  , start    :: Text
  , others   :: [Text]
  , arrows   :: [(Text, Text)]
  }
  deriving (Show, Eq)

-- 🗺️ Every state of a flow, the start first.
-- 流れの状態を全部。はじめの状態が先頭
flowStates :: Flow -> [Text]
flowStates flow = start flow : others flow

-- 📒 Everything I defined, in the order I defined it.
-- 自分で決めたものの全部。決めた順に
data Schema = Schema
  { attrDefs :: [AttrDef]
  , flows    :: [Flow]
  }
  deriving (Show, Eq)
-- ANCHOR_END: flow

-- ANCHOR: problems
-- 🚧 What is wrong with a value someone typed in (in a form, or in
--    a search).
-- 打ちこまれた値（フォームや、絞り込みの言葉）の、まずいところ
data AttrProblem
  = UnknownName Text       -- no such attribute / そんな名前はない
  | Unreadable  Text Text  -- name, what was typed / その種類として読めない
  | Unordered   Text       -- cannot say bigger / 大小では比べられない
  deriving (Show, Eq)

-- 🚧 What is wrong with a definition.
-- 定義の、まずいところ
data DefProblem
  = NameTaken   Text       -- the name is in use / その名前は使っている
  | NothingToPick          -- no choices / 選択肢が1つもない
  | Twice       Text       -- the same one twice / 同じものが2つ
  | NoSuchState Text       -- an arrow to nowhere / 矢印の先や元にない
  | Unreachable Text       -- no way from the start / はじめから行けない
  deriving (Show, Eq)
-- ANCHOR_END: problems

-- ANCHOR: mistake
-- 🚫 Everything the core can refuse, with what was wrong.
-- 核が断ることのすべて。何がまずかったかを持つ
data Mistake
  = BadScore      Int                -- not 0 to 100 / 0〜100 でない
  | BadRating     Text               -- cannot read it / 評価として読めない
  | NotAllowed    Event ReadingState -- no such move / その遷移はない
  | NoSuchEntry   EntryId            -- not in the table / 目次にない
  | EmptyNote                        -- nothing written / 何も書いていない
  | NoSuchWork    WorkId             -- not on the shelf / 本棚にない
  | BadDefinition [DefProblem]       -- every problem / 定義のまずい所の全部
  | BadAttrs      [AttrProblem]      -- every problem / 値のまずい所の全部
  | NoSuchFlow    Text               -- no such flow / その流れはない
  | NoSuchArrow   Text (Maybe Text) Text -- flow, from, to / その矢印はない
  | CannotParse   Text               -- the search / 絞り込みの言葉が読めない
  | NotASerial                       -- episodes for a book / 連載でない
  | EmptySearch                      -- no words to look for / 探す言葉がない
  | EmptyTitle                       -- a work with no title / 題がない
  | LongTitle     Int                -- longer than this / この字数より長い題
  | NegativeSize  Text               -- the heading / 量がマイナスの見出し
  deriving (Show, Eq)

-- ✅ An answer the core has checked: the value, or the mistake.
--    Checked Card reads "a Card, or why not".
-- 核が確かめた答え：値か、まちがいか。
-- Checked Card は「Card か、だめな理由」と読む
type Checked = Either Mistake
-- ANCHOR_END: mistake
-- ANCHOR_END: all
