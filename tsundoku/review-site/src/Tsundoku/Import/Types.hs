-- ANCHOR: all
-- ANCHOR: header
-- | What we take in from outside (an EPUB, a PDF, an ISBN, a web
--   novel), and every way taking it in can fail.
-- 外から取り込むもの（EPUB・PDF・ISBN・Web 小説）と、取り込みが失敗する
-- すべての形
module Tsundoku.Import.Types
  ( -- * Drafts / 下書き
    Chars (..)
  , Draft (..)
  , Imported (..)
  , perPage
  , fromStarts
  , outOfOrder
  , bookWork
    -- * Problems / うまくいかなかったこと
  , ImportProblem (..)
  , explainImport
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (mapAccumL)
import Data.Tree (Forest, flatten)

import Tsundoku.Isbn (Isbn, IsbnProblem (..), isbnText)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: draft
-- 🔤 How many characters. An EPUB has no pages (the reader's screen and
--    font decide them), so we count characters instead.
-- 文字の数。EPUB にはページがない（画面と文字の大きさで決まる）ので、
-- かわりに文字を数える
newtype Chars = Chars Int deriving (Show, Eq, Ord)

instance Semigroup Chars where
  Chars a <> Chars b = Chars (a + b)
instance Monoid Chars where
  mempty = Chars 0

-- 📝 What we could read: the title and the creator (if the file says
--    them), and the table, each heading with its own size (not its
--    children's), as in Entry. u is the unit: Chars or Pages.
-- 読めたもの：題と作者（ファイルに書いてあれば）と目次。見出しごとに、
-- その項目だけの量（子の量を含めない。Entry と同じ）。u は単位：
-- Chars か Pages
data Draft u = Draft
  { draftTitle   :: Maybe Text
  , draftCreator :: Maybe Text
  , draftToc     :: Forest (Text, u)
  }
  deriving (Show, Eq, Functor)

-- 📦 What we took in, and what we could not: a draft can come with a
--    few problems that did not stop us.
-- 取り込めたものと、取り込めなかったもの。止まるほどではない問題を、
-- 下書きと一緒に持つ
data Imported u = Imported
  { draft  :: Draft u
  , missed :: [ImportProblem]
  }
  deriving (Show, Eq, Functor)
-- ANCHOR_END: draft

-- ANCHOR: pages
-- 📏 Pages from characters: one page holds `n` characters. A heading
--    with no text of its own is 0 pages.
-- 字数からページ数を見積もる。1ページに n 字。自分の本文がない見出しは
-- 0 ページ
perPage :: Int -> Chars -> Pages
perPage n (Chars c) = Pages ((c + n - 1) `div` n)

-- 📑 Pages from where each heading starts: from its page up to the page
--    before the next heading (in reading order). The last one runs to
--    the end of the book (`total` pages). PDF bookmarks and the tables
--    openBD sends both say only where each heading starts.
-- 見出しが始まるページから、その項目だけのページ数を出す：そのページから、
-- （読む順で）次の見出しの前のページまで。最後の見出しは、本の終わり
-- （total ページ）まで。PDF のしおりも openBD の目次も、見出しが始まる
-- ページしか教えてくれない
fromStarts :: Int -> Forest (Text, Int) -> Forest (Text, Pages)
fromStarts total headings = snd (mapAccumL (mapAccumL own) nexts headings)
  where
    nexts = drop 1 (map snd (concatMap flatten headings)) ++ [total + 1]
    own (next : later) (heading, start) =
      (later, (heading, Pages (max 0 (next - start))))
    own [] (heading, _) = ([], (heading, Pages 0))

-- 🔀 The headings that start before the heading above them. Their
--    pages cannot be right: say so, so that someone can check them.
-- 上の見出しより前のページから始まる見出し。ページ数は正しく出せないので、
-- 確かめてもらえるように知らせる
outOfOrder :: Forest (Text, Int) -> [ImportProblem]
outOfOrder headings =
  [ OutOfOrder heading
  | ((_, before), (heading, start)) <- zip flat (drop 1 flat)
  , start < before ]
  where
    flat = concatMap flatten headings

-- 📗 A book from a draft. The file may not say its title: then the
--    name we were given (the file's name).
-- 下書きから本を作る。ファイルに題がなければ、もらった名前（ファイル名）
bookWork :: Text -> Draft Pages -> Work
bookWork fallback d = Work
  { title    = fromMaybe fallback (draftTitle d)
  , creator  = fromMaybe ""       (draftCreator d)
  , contents = Book (numbered (draftToc d))
  }
-- ANCHOR_END: pages

-- ANCHOR: problems
-- 🚧 Every way taking something in can go wrong.
-- 取り込みがうまくいかない、すべての形
data ImportProblem
  -- the file / ファイル
  = NoSuchFile   FilePath           -- cannot open it / 開けない
  | NotSupported FilePath           -- not EPUB, not PDF / どちらでもない
  -- inside an EPUB / EPUB の中
  | NotAZip      Text               -- not a zip / zip でない
  | MissingFile  FilePath           -- not in the zip / zip の中にない
  | BrokenXml    FilePath Text      -- cannot read it / XML として読めない
  | NoToc                           -- no nav, no toc.ncx / 目次がない
  -- a PDF and its tools / PDF と道具
  | NoOutline                       -- no bookmarks / しおりがない
  | OutOfOrder   Text               -- before the one above / 前より前
  | NoTool       FilePath           -- not installed / 道具がない
  | ToolFailed   FilePath Int Text  -- exit code, stderr / 道具が失敗した
  -- web APIs / Web の API
  | BadIsbn      IsbnProblem        -- not an ISBN / ISBN でない
  | UnknownIsbn  Isbn               -- no such book / その本はない
  | NoPagesInToc Int                -- a table with no pages / ページのない目次
  | UnknownNovel Text               -- no such novel / その作品はない
  | Offline      Text               -- the network / つながらない
  | Unexpected   Text               -- cannot read it / 読めない答え
  deriving (Show, Eq)
-- ANCHOR_END: problems

-- ANCHOR: explain
-- 💬 What went wrong, in words for the person at the screen.
-- うまくいかなかったことを、画面の前の人に向けた言葉で
explainImport :: ImportProblem -> Text
explainImport problem = case problem of
  NoSuchFile path      -> T.pack path <> " が開けません"
  NotSupported path    -> T.pack path <> "：取り込めるのは EPUB と PDF です"
  NotAZip why          -> "EPUB（zip）として開けません：" <> why
  MissingFile path     -> "ファイルの中に " <> T.pack path <> " がありません"
  BrokenXml path why   -> T.pack path <> " が XML として読めません：" <> why
  NoToc                -> "目次（nav.xhtml も toc.ncx も）が見つかりません"
  NoOutline            -> "PDF にしおりがないので、目次は1行だけにしました"
  OutOfOrder heading   -> "「" <> heading <> "」のページが、"
                            <> "前の見出しより前です（前のは 0 ページに）"
  NoTool tool          -> T.pack tool <> " が見つかりません。poppler を"
                            <> "入れてください（brew install poppler）"
  ToolFailed tool n e  -> T.pack tool <> " が失敗しました（終了コード "
                            <> T.pack (show n) <> "）：" <> T.strip e
  BadIsbn why          -> "ISBN として読めません：" <> isbnProblem why
  UnknownIsbn i        -> "ISBN " <> isbnText i <> " の本は見つかりません"
  NoPagesInToc n       -> "目次の " <> T.pack (show n) <> " 行に、始まるページが"
                            <> "ないので、ページ数は見出しに等分しました"
  UnknownNovel ncode   -> "Nコード " <> ncode <> " の作品は見つかりません"
  Offline why          -> "つながりません：" <> why
  Unexpected why       -> "答えが読めません：" <> why
  where
    isbnProblem (NotABookCode p) =
      p <> " で始まる13けたは、本の番号（978・979）ではありません"
        <> "（2段目のバーコードではありませんか）"
    isbnProblem (WrongLength n)  =
      T.pack (show n) <> "けたです（10けたか13けた）"
    isbnProblem (NotADigit c)    =
      "数字でない文字があります：" <> T.singleton c
    isbnProblem (WrongCheck e w) =
      "最後の1けたは " <> T.singleton e <> " のはずが、"
        <> T.singleton w <> " です"
-- ANCHOR_END: explain
-- ANCHOR_END: all
