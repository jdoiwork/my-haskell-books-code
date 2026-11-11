-- ANCHOR: all
-- | Ways to show progress as text: bars, lines for a table, badges.
--   Only views: they read Progress, and never change it.
-- 進み具合を文字で見せる：進捗バー、目次の一覧、バッジ。
-- ビューだけ。Progress を読むだけで、変えない
-- ANCHOR: header
module Tsundoku.Bar
  ( showAmount
  , bar
  , summaryLine
  , tocBars
  , unreadBadge
  , cardSummary
  ) where

-- text, sets, and trees
-- 文字列、集合、木
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))

import Tsundoku.Card
import Tsundoku.Progress
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: bar
-- 🏷️ An amount with its unit: "78ページ", "155分", "3話".
-- 量を単位つきで
showAmount :: Amount u => u -> Text
showAmount x = T.pack (show (count x)) <> unitName x

-- ▓ A bar of `width` cells and the percent: "▓▓▓▓▓▓░░░░  62%".
--    Cells are rounded down, so the bar is full only at the end.
-- 幅 width のバーと百分率。マス目は切り捨てなので、
-- バーが満タンになるのは、読み終えたときだけ
bar :: Amount u => Int -> Progress u -> Text
bar width p = T.replicate filled "▓" <> T.replicate (width - filled) "░"
           <> " " <> T.justifyRight 4 ' ' (T.pack (show (percent p)) <> "%")
  where
    t      = count (total p)
    filled
      | t <= 0    = 0
      | otherwise = min width (count (done p) * width `div` t)

-- 📏 A bar, and how much out of how much: "... 62%  48/78ページ".
-- バーと、全体のうちどれだけか
summaryLine :: Amount u => Int -> Progress u -> Text
summaryLine width p =
  bar width p <> "  " <> T.pack (show (count (done p))) <> "/"
              <> showAmount (total p)
-- ANCHOR_END: bar

-- ANCHOR: tocBars
-- 📑 One line per entry: the bar of its whole subtree, then its
--    heading, indented by its depth.
-- 項目ごとに1行：その項目の下の木全体のバーと、深さだけ字下げした見出し
tocBars :: Amount u => Int -> Set EntryId -> Toc u -> [Text]
tocBars width finished toc = concatMap (linesOf 0) toc
  where
    linesOf depth node@(Node entry children) =
      let here = bar width (progressOf finished [node])
                   <> "  " <> T.replicate depth "  " <> heading entry
      in  here : concatMap (linesOf (depth + 1)) children
-- ANCHOR_END: tocBars

-- ANCHOR: badge
-- 🔴 A badge for what is left: "未読 3話". Nothing when all is read.
-- 残りのバッジ。全部読んでいたら Nothing
unreadBadge :: Amount u => Progress u -> Maybe Text
unreadBadge p
  | count left > 0 = Just ("未読 " <> showAmount left)
  | otherwise      = Nothing
  where
    left = remaining p
-- ANCHOR_END: badge

-- ANCHOR: cardSummary
-- 🗂️ The summary line of a card, from its reading log. Each kind
--    of work counts in its own unit. A serial keeps growing, so it
--    shows the episodes left, not a bar. A film or a game has no
--    table, so no line.
-- カードの1行のまとめ（読書ログから）。作品の種類ごとに、その単位で
-- 数える。連載は育つので、バーではなく未読の話数。映画とゲームには
-- 目次がないので、まとめはない
cardSummary :: Int -> Card -> Maybe Text
cardSummary width card =
  case contents (work card) of
    Book   toc -> Just (summaryLine width (progressOf finished toc))
    Serial toc -> Just (unreadOrDone (progressOf finished toc))
    Series toc -> Just (summaryLine width (progressOf finished toc))
    Film   _   -> Nothing
    Game       -> Nothing
  where
    finished = finishedIn (readLog card)
    unreadOrDone p =
      case unreadBadge p of
        Just badge -> badge
        Nothing    -> "最新話まで読了"
-- ANCHOR_END: cardSummary
-- ANCHOR_END: all
