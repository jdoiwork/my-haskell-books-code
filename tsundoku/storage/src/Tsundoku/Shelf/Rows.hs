-- ANCHOR: all
-- | Cards as plain rows of a table, and back. Both databases share it:
--   only the SQL around it is theirs.
-- カードを、表の素朴な行にする。そして、行からカードに戻す。
-- 2つのデータベースで共有する（それぞれのものは、まわりの SQL だけ）
-- ANCHOR: header
module Tsundoku.Shelf.Rows
  ( -- * Writing / しまう
    toPlain
  , stateName
  , targetEntry
    -- * Reading back / 読み戻す
  , WorkRow (..)
  , assemble
  , BrokenRow (..)
  ) where

-- exceptions, dictionaries, text, time, and trees
-- 例外、辞書、文字列、日時、木
import Control.Exception (Exception)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, UTCTime)

import Tsundoku.Card
import Tsundoku.Outline (fromOutline)
import Tsundoku.Rating (score)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: plain
-- 📤 Contents as plain values: the kind, the length of a film, and the
--    table with every amount as a plain number.
-- 作品の中身を素朴な値に：種類、映画の長さ、量を数にした目次
toPlain :: Contents -> (Text, Maybe Int, Toc Int)
toPlain what = case what of
  Book   toc         -> ("book",   Nothing, counted toc)
  Serial toc         -> ("serial", Nothing, counted toc)
  Series toc         -> ("series", Nothing, counted toc)
  Film   (Minutes m) -> ("film",   Just m,  [])
  Game               -> ("game",   Nothing, [])
  where
    counted :: Amount u => Toc u -> Toc Int
    counted = map (fmap (fmap count))

-- 📥 And back. The kind says which unit the numbers are in.
-- そして、もとに戻す。数の単位は、種類が決める
fromPlain :: Text -> Maybe Int -> Toc Int -> Maybe Contents
fromPlain kind minutes toc = case (kind, minutes) of
  ("book",   _)      -> Just (Book   (amounts toc))
  ("serial", _)      -> Just (Serial (amounts toc))
  ("series", _)      -> Just (Series (amounts toc))
  ("film",   Just m) -> Just (Film (Minutes m))
  ("game",   _)      -> Just Game
  _                  -> Nothing
  where
    amounts :: Amount u => Toc Int -> Toc u
    amounts = map (fmap (fmap amount))
-- ANCHOR_END: plain

-- ANCHOR: names
-- 🏷️ A reading state as a word in a table: "Piled", "Reading", ...
-- 読書の状態を、表の中の言葉に："Piled"、"Reading"……
stateName :: ReadingState -> Text
stateName = T.pack . show

-- 🔎 And back. Only the words of the five states are states.
-- そして、もとに戻す。5つの状態の言葉だけが、状態
stateFromName :: Text -> Maybe ReadingState
stateFromName name =
  lookup name [ (stateName s, s) | s <- [minBound .. maxBound] ]

-- 🎯 The entry a note is about, or no entry (the whole work).
-- 感想が付く項目。作品全体なら、項目なし
targetEntry :: Target -> Maybe Int
targetEntry OnWork                = Nothing
targetEntry (OnEntry (EntryId i)) = Just i
-- ANCHOR_END: names

-- ANCHOR: assemble
-- 🗃️ The row of the works table.
-- 作品の表の1行
data WorkRow = WorkRow
  { rowTitle   :: Text
  , rowCreator :: Text
  , rowKind    :: Text
  , rowMinutes :: Maybe Int
  , rowReading :: Text
  , rowRating  :: Maybe Int
  }

-- 🧩 Put a card together from the rows of its tables: the work, the
--    table of contents (number, depth, heading, size, from top to
--    bottom), the notes, the log, and the ratings of entries.
-- 表の行から、カードを組み立てる：作品、目次（番号・深さ・見出し・量を
-- 上から順に）、感想、読書ログ、項目の評価
assemble
  :: WorkRow
  -> [(Int, Int, Text, Int)]
  -> [(Maybe Int, Text, UTCTime, Bool)]
  -> [(Day, Int)]
  -> [(Int, Int)]
  -> Either BrokenRow Card
assemble row tocRows noteRows logRows ratingRows = do
  contents' <- found "kind"    (fromPlain (rowKind row) (rowMinutes row) toc)
  reading'  <- found "reading" (stateFromName (rowReading row))
  rating'   <- traverse points (rowRating row)
  given     <- traverse (\(i, n) -> (,) (EntryId i) <$> points n) ratingRows
  pure Card
    { work         = Work (rowTitle row) (rowCreator row) contents'
    , reading      = reading'
    , rating       = rating'
    , notes        = map toNote noteRows
    , readLog      = [ LogLine day (EntryId i) | (day, i) <- logRows ]
    , entryRatings = Map.fromList given
    }
  where
    toc = fromOutline
      [ (depth, Entry (EntryId i) heading' size')
      | (i, depth, heading', size') <- tocRows ]
    toNote (entry, body', at, spoiler') =
      Note (maybe OnWork (OnEntry . EntryId) entry) body' at spoiler'
    found what = maybe (Left (BrokenRow what)) Right
    points n   = either (const (Left (BrokenRow "score"))) Right (score n)

-- 💥 A row the core cannot read. Only a broken table makes one.
-- 核が読めない行。表が壊れているときにだけ起きる
newtype BrokenRow = BrokenRow Text
  deriving Show

instance Exception BrokenRow

-- ANCHOR_END: assemble
-- ANCHOR_END: all
