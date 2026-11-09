-- ANCHOR: all
-- | A few works and cards of the guild's library, to play with in GHCi.
-- ギルドの図書室の作品とカード（GHCi で遊ぶため）
-- ANCHOR: imports
module Tsundoku.Examples
  ( dragonBook
  , dragonToc
  , librarianSerial
  , detectiveDrama
  , lastDragonFilm
  , dungeonGame
  , onDay
  , dragonCard
  , surely
  , numberedShort
  , printToc
  ) where

-- trees, time, and walking with a counter
-- 木、日時、数えながら歩く
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Data.Traversable (mapAccumL)
import Data.Tree (Forest, Tree (..), drawForest)

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
  , contents = Serial (numbered
      [ Node ("第1章 目覚めたら本の山", Episodes 0)
          [ Node ("第1話 ここはどこ", Episodes 1) []
          , Node ("第2話 返却期限", Episodes 1) []
          ]
      , Node ("第2章 禁書の棚", Episodes 0)
          [ Node ("第3話 鍵のかかった扉", Episodes 1) []
          ]
      ])
  }

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

-- 🗂️ The dragon book: started, two notes, rated ★4.5 (kept as 90).
-- ドラゴン学の本：読み始めて、感想を2つ書き、★4.5 を付けた（90 点で残る）
dragonCard :: Card
dragonCard = surely $ do
  started <- applyEvent Start (newCard dragonBook)
  noted   <- addNote firstNote started
  noted'  <- addNote secondNote noted
  given   <- parse FiveStars "★4.5"
  pure (rate given noted')
  where
    firstNote  = Note OnWork "絵が多くて読みやすい" (onDay 2026 11 1) False
    secondNote = Note (OnEntry (EntryId 4)) "火を吐かない種もいるとは！"
                      (onDay 2026 11 3) True

-- 🙏 For GHCi and examples: take the value out of a Right we know is fine.
-- GHCi と例のため：うまくいくとわかっている Right から中身を取り出す
surely :: Either Mistake a -> a
surely (Right x) = x
surely (Left m)  = error ("surely: " ++ show m)
-- ANCHOR_END: card

-- ANCHOR: short
-- 🔢 The same numbering, in one line: walk the trees with a counter.
-- 同じ番号振りを1行で：数を持ち回りながら、木を歩く
numberedShort :: Forest (Text, u) -> Toc u
numberedShort outline = snd (mapAccumL (mapAccumL label) 1 outline)
  where
    label n (name, amount) = (n + 1, Entry (EntryId n) name amount)
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
