-- ANCHOR: all
-- | A few works and cards of the guild's library, to play with in GHCi.
-- ギルドの図書室の作品とカード（GHCi で遊ぶため）
-- ANCHOR: imports
module Tsundoku.Examples
  ( dragonBook
  , dragonToc
  , librarianSerial
  , librarianToc
  , detectiveDrama
  , lastDragonFilm
  , dungeonGame
  , onDay
  , nov
  , dragonLog
  , dragonCard
  , copyistSerial
  , copyistToc
  , librarianCard
  , copyistCard
  , detectiveCard
  , filmCard
  , myShelf
  , surely
  , numberedShort
  , printToc
  ) where

-- trees, time, walking with a counter, and folding with effects
-- 木、日時、数えながら歩く、失敗しうる畳み込み
import Control.Monad (foldM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, UTCTime (..), fromGregorian)
import Data.Traversable (mapAccumL)
import Data.Tree (Forest, Tree (..), drawForest, flatten)

import Tsundoku.Card
import Tsundoku.Rating
import Tsundoku.Types
-- ANCHOR_END: imports

-- ANCHOR: book
-- 🐉 A book with parts, chapters, and sections.
-- 部・章・節のある本
dragonToc :: Toc Pages
dragonToc = numbered
  [ Node ("第1部 ドラゴンを知る", Pages 2)
      [ Node ("第1章 見分け方", Pages 4)
          [ Node ("1.1 うろこの色", Pages 8) []
          , Node ("1.2 火を吐くもの", Pages 10) []
          ]
      , Node ("第2章 すみかと季節", Pages 24) []
      ]
  , Node ("第2部 ドラゴンと話す", Pages 2)
      [ Node ("第3章 あいさつの作法", Pages 16) []
      , Node ("第4章 怒らせたときは", Pages 12) []
      ]
  ]

dragonBook :: Work
dragonBook = Work
  { title    = "はじめてのドラゴン学"
  , creator  = "ギルド図書室"
  , contents = Book dragonToc
  }
-- ANCHOR_END: book

-- ANCHOR: others
-- 📱 A web serial: arcs of episodes. New episodes keep coming.
-- Web 小説の連載：章ごとに話が並ぶ。話はこれからも増える
librarianSerial :: Work
librarianSerial = Work
  { title    = "図書室の司書に転生した件"
  , creator  = "しおり堂"
  , contents = Serial librarianToc
  }

-- The arcs and episodes so far: 1 第1章, 2 第1話, 3 第2話,
-- 4 第2章, 5 第3話.
-- いままでの章と話：1 第1章、2 第1話、3 第2話、4 第2章、5 第3話
librarianToc :: Toc Episodes
librarianToc = numbered
  [ Node ("第1章 目覚めたら本の山", Episodes 0)
      [ Node ("第1話 ここはどこ", Episodes 1) []
      , Node ("第2話 返却期限", Episodes 1) []
      ]
  , Node ("第2章 禁書の棚", Episodes 0)
      [ Node ("第3話 鍵のかかった扉", Episodes 1) []
      ]
  ]

-- 📺 A streaming drama: seasons of episodes. Season 2 is still
--    coming out, a new episode every week.
-- 配信の連続ドラマ：シーズンごとに話が並ぶ。シーズン2は配信中で、
-- 毎週1話ずつ増える
detectiveDrama :: Work
detectiveDrama = Work
  { title    = "王都の路地裏探偵"
  , creator  = "王都配信"
  , contents = Series (numbered
      [ Node ("シーズン1", Minutes 0)
          [ Node ("第1話 消えた写本", Minutes 52) []
          , Node ("第2話 閉館後の足音", Minutes 48) []
          , Node ("第3話 司書は知っていた", Minutes 55) []
          ]
      , Node ("シーズン2", Minutes 0)
          [ Node ("第1話 帰ってきた探偵", Minutes 50) []
          ]
      ])
  }

-- 🎬 A film is one piece. 🎮 A game has no table of contents here.
-- 映画は1本。ゲームは、ここでは目次を持たない
lastDragonFilm :: Work
lastDragonFilm = Work "最後のドラゴン" "王都映画組" (Film (Minutes 121))

dungeonGame :: Work
dungeonGame = Work "ダンジョン司書" "地下書庫ゲームズ" Game
-- ANCHOR_END: others

-- ANCHOR: card
-- 📅 Midnight (UTC) of a day, for short examples.
-- ある日の 0 時（UTC）。短い例のため
onDay :: Integer -> Int -> Int -> UTCTime
onDay y m d = UTCTime (fromGregorian y m d) 0

-- ANCHOR: log
-- 📅 A day of November 2026, for the reading log.
-- 2026年11月の、ある日（読書ログのため）
nov :: Int -> Day
nov d = fromGregorian 2026 11 d

-- 📓 What I read of the dragon book, day by day: part 1 and
--    chapter 1 first, then 1.1, 1.2, and chapter 2 in a row.
-- ドラゴン学の本の読書ログ：第1部と第1章のあと、
-- 1.1・1.2・第2章を3日続けて
dragonLog :: [LogLine]
dragonLog =
  [ LogLine (nov 1) (EntryId 1)   -- 第1部 ドラゴンを知る (2)
  , LogLine (nov 2) (EntryId 2)   -- 第1章 見分け方 (4)
  , LogLine (nov 6) (EntryId 3)   -- 1.1 うろこの色 (8)
  , LogLine (nov 7) (EntryId 4)   -- 1.2 火を吐くもの (10)
  , LogLine (nov 8) (EntryId 5)   -- 第2章 すみかと季節 (24)
  ]
-- ANCHOR_END: log

-- 🗂️ The dragon book: started, two notes, rated ★4.5 (kept as 90),
--    and the reading log.
-- ドラゴン学の本：読み始めて、感想を2つ書き、★4.5 を付けた
-- （90 点で残る）。読書ログも付ける
dragonCard :: Card
dragonCard = surely $ do
  started <- applyEvent Start (newCard dragonBook)
  noted   <- addNote firstNote started
  noted'  <- addNote secondNote noted
  given   <- parse FiveStars "★4.5"
  foldM (flip logReading) (rate given noted') dragonLog
  where
    firstNote  = Note OnWork "絵が多くて読みやすい" (onDay 2026 11 1) False
    secondNote = Note (OnEntry (EntryId 4)) "火を吐かない種もいるとは！"
                      (onDay 2026 11 3) True

-- 🙏 For GHCi and examples: take the value out of a Right we know is fine.
-- GHCi と例のため：うまくいくとわかっている Right から中身を取り出す
surely :: Checked a -> a
surely (Right x) = x
surely (Left m)  = error ("surely: " ++ show m)
-- ANCHOR_END: card

-- ANCHOR: shelf
-- 📜 A long web serial: 3 arcs of 10 episodes. Numbers: arc 1 is 1,
--    its episodes 2-11; arc 2 is 12, episodes 13-22; arc 3 is 23,
--    episodes 24-33.
-- 長い Web 小説：10話ずつ3章。番号は、第1章が 1、その話が 2〜11、
-- 第2章が 12、話が 13〜22、第3章が 23、話が 24〜33
copyistToc :: Toc Episodes
copyistToc = numbered
  [ Node (arcName a, Episodes 0)
      [ Node (episodeName (10 * (a - 1) + e), Episodes 1) []
      | e <- [1 .. 10]
      ]
  | a <- [1 .. 3 :: Int]
  ]
  where
    arcName a     = "第" <> T.pack (show a) <> "章"
    episodeName n = "第" <> T.pack (show n) <> "話"

copyistSerial :: Work
copyistSerial = Work
  { title    = "迷宮図書館の写本係"
  , creator  = "インク壺"
  , contents = Serial copyistToc
  }

-- 📱 The librarian serial: I read and rated only episode 1, ★5.
-- 司書の連載：読んで評価したのは第1話だけ。★5
librarianCard :: Card
librarianCard = surely $ do
  started <- applyEvent Start (newCard librarianSerial)
  rateEntry (EntryId 2) (surely (score 100)) started

-- 📜 The copyist serial: all 30 episodes rated, 92 points on average
--    (★5, ★5, ★4.5, ★3.5, ★5, again and again).
-- 写本係の連載：30話ぜんぶに評価。平均 92 点
-- （★5、★5、★4.5、★3.5、★5 のくり返し）
copyistCard :: Card
copyistCard = surely $ do
  started <- applyEvent Start (newCard copyistSerial)
  foldM rateOne started (zip episodes (cycle [100, 100, 90, 70, 100]))
  where
    episodes = [ entryId e | e <- concatMap flatten copyistToc
                           , size e == Episodes 1 ]
    rateOne card (i, n) = do
      given <- score n
      rateEntry i given card

-- 📺 The detective drama: 4 episodes rated, 80 points on average.
-- 探偵ドラマ：4話に評価。平均 80 点
detectiveCard :: Card
detectiveCard = surely $ do
  started <- applyEvent Start (newCard detectiveDrama)
  foldM rateOne started [(2, 80), (3, 70), (4, 90), (6, 80)]
  where
    rateOne card (i, n) = do
      given <- score n
      rateEntry (EntryId i) given card

-- 🎬 The film: watched to the end, ★3 (60 points) for the whole film.
-- 映画：最後まで観て、作品全体に★3（60 点）
filmCard :: Card
filmCard = surely $ do
  started  <- applyEvent Start (newCard lastDragonFilm)
  finished <- applyEvent Finish started
  given    <- parse FiveStars "★3"
  pure (rate given finished)

-- 📚 My whole shelf. The game is not rated yet.
-- わたしの本棚の全部。ゲームは、まだ評価していない
myShelf :: [Card]
myShelf =
  [ dragonCard
  , librarianCard
  , copyistCard
  , detectiveCard
  , filmCard
  , newCard dungeonGame
  ]
-- ANCHOR_END: shelf

-- ANCHOR: short
-- 🔢 The same numbering, in one line: walk the trees with a counter.
-- 同じ番号振りを1行で：数を持ち回りながら、木を歩く
numberedShort :: Forest (Text, u) -> Toc u
numberedShort outline = snd (mapAccumL (mapAccumL label) 1 outline)
  where
    label n (name, howMuch) = (n + 1, Entry (EntryId n) name howMuch)
-- ANCHOR_END: short

-- ANCHOR: print
-- 🖨️ For GHCi: draw the headings of a table as a tree.
--    (Drawing is the screen's job; this is just for looking.)
-- GHCi のため：目次の見出しを木の形に描く
-- （描くのは画面の仕事。これは、のぞく用）
printToc :: Toc u -> IO ()
printToc toc = putStr (drawForest (map (fmap showEntry) toc))
  where
    showEntry entry =
      let EntryId n = entryId entry
      in  show n ++ ". " ++ T.unpack (heading entry)
-- ANCHOR_END: print
-- ANCHOR_END: all
