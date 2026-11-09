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
  , finishedDragon
  , taggedShelf
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
    -- * My own attributes and states / 自分で決める属性と状態
  , oshiDef
  , whereDef
  , boughtOnDef
  , signedDef
  , formatDef
  , libraryAttrs
  , serialFlow
  , lendingFlow
  , brokenFlow
  , libraryFlows
  , librarySchema
  , dragonForm
  , librarianForm
  , copyistForm
  , dragonFilled
  , librarianFilled
  , copyistFilled
  , filledCards
  , oshi
  , boughtOn
    -- * Fixing a work by hand / 作品を手で直す
  , dragonFix
  ) where

-- trees, time, walking with a counter, and folding with effects
-- 木、日時、数えながら歩く、失敗しうる畳み込み
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, UTCTime (..), fromGregorian)
import Data.Traversable (mapAccumL)
import Data.Tree (Forest, Tree (..), drawForest, flatten)

import Tsundoku.Card
import Tsundoku.Custom
import Tsundoku.Field
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

-- ANCHOR: tagged
-- 🏁 The dragon book, read to the end: entries 6 to 8 in November too.
-- 最後まで読んだドラゴン学：11月に、項目 6〜8 も読んだ
finishedDragon :: Card
finishedDragon = surely $ do
  logged <- foldM (flip logReading) dragonCard
    [ LogLine (nov 10) (EntryId 6)
    , LogLine (nov 11) (EntryId 7)
    , LogLine (nov 12) (EntryId 8) ]
  applyEvent Finish logged

-- 🔖 My shelf with tags (and the dragon book finished), numbered 1, 2, ...
-- タグを付けた本棚（ドラゴン学は読み終えたもの）。番号は 1, 2, …
taggedShelf :: [(WorkId, Card)]
taggedShelf = zip (map WorkId [1 ..]) (zipWith withTags tagLists cards)
  where
    cards    = finishedDragon : drop 1 myShelf
    tagLists =
      [ ["竜", "図鑑", "魔法"]
      , ["転生", "図書館", "魔法"]
      , ["図書館", "迷宮", "魔法"]
      , ["推理", "王都"]
      , ["竜", "冒険"]
      , ["迷宮", "冒険"] ]
    withTags ts c = c { tags = Set.fromList ts }
-- ANCHOR_END: tagged

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


-- ANCHOR: defs
-- 🏷️ The attributes the library decided to keep.
-- 図書室が残すことにした属性
oshiDef, whereDef, boughtOnDef, signedDef, formatDef :: AttrDef
oshiDef     = AttrDef "推し度"       NumberKind
whereDef    = AttrDef "どこで買った" TextKind
boughtOnDef = AttrDef "買った日"     DateKind
signedDef   = AttrDef "サイン本"     YesNoKind
formatDef   = AttrDef "形"           (ChoiceKind ["紙", "電子", "Web"])

libraryAttrs :: [AttrDef]
libraryAttrs = [oshiDef, whereDef, boughtOnDef, signedDef, formatDef]
-- ANCHOR_END: defs

-- ANCHOR: flows
-- 📱 Where a web serial is: going on, on a break, or finished.
-- Web 小説の連載がどこにいるか：連載中、休止、完結
serialFlow :: Flow
serialFlow = Flow
  { flowName = "連載の状態"
  , start    = "連載中"
  , others   = ["休止", "完結"]
  , arrows   = [ ("連載中", "休止"), ("休止", "連載中")
               , ("連載中", "完結"), ("休止", "完結") ]
  }

-- 📕 Where a book of the library is: on the shelf, or lent out.
-- 図書室の本がどこにあるか：本棚にあるか、貸し出し中か
lendingFlow :: Flow
lendingFlow = Flow
  { flowName = "貸し出し"
  , start    = "本棚にある"
  , others   = ["貸し出し中"]
  , arrows   = [ ("本棚にある", "貸し出し中"), ("貸し出し中", "本棚にある") ]
  }

-- 🚧 A flow with two mistakes: an arrow to a state that is not there
--    (a typo), and a state no arrow goes to.
-- まちがいが2つある流れ：ない状態への矢印（打ちまちがい）と、
-- どの矢印も行かない状態
brokenFlow :: Flow
brokenFlow = Flow
  { flowName = "配信の状態"
  , start    = "配信中"
  , others   = ["全話配信済み", "打ち切り"]
  , arrows   = [ ("配信中", "全話配信") ]
  }

libraryFlows :: [Flow]
libraryFlows = [serialFlow, lendingFlow]

-- 📒 Everything the library defined.
-- 図書室が決めたものの全部
librarySchema :: Schema
librarySchema = surely $ do
  withAttrs <- foldM (flip defineAttr) emptySchema libraryAttrs
  foldM (flip defineFlow) withAttrs libraryFlows
-- ANCHOR_END: flows

-- ANCHOR: forms
-- 📝 What I typed into the forms of three cards.
-- 3枚のカードのフォームに、打ちこんだもの
dragonForm, librarianForm, copyistForm :: Map Text Text
dragonForm = Map.fromList
  [ ("推し度", "4"), ("どこで買った", "古本市"), ("買った日", "2026-10-30")
  , ("サイン本", "はい"), ("形", "紙") ]
librarianForm = Map.fromList [ ("推し度", "5"), ("形", "Web") ]
copyistForm   = Map.fromList [ ("推し度", "3"), ("形", "Web") ]

-- 🗂️ The cards with my own attributes and states filled in: the dragon
--    book is on the shelf; the librarian serial (★5) goes on; the
--    copyist serial (★4) is on a break.
-- 自分で決めた属性と状態を書きこんだカード：ドラゴン学は本棚にある。
-- 司書の連載（★5）は連載中。写本係の連載（★4）は休止中
dragonFilled, librarianFilled, copyistFilled :: Card
dragonFilled = surely $ do
  filled <- editAttrs dragonForm librarySchema dragonCard
  moveFlow "貸し出し" "本棚にある" librarySchema filled
librarianFilled = surely $ do
  given  <- parse FiveStars "★5"
  filled <- editAttrs librarianForm librarySchema (rate given librarianCard)
  moveFlow "連載の状態" "連載中" librarySchema filled
copyistFilled = surely $ do
  given   <- parse FiveStars "★4"
  filled  <- editAttrs copyistForm librarySchema (rate given copyistCard)
  started <- moveFlow "連載の状態" "連載中" librarySchema filled
  moveFlow "連載の状態" "休止" librarySchema started

-- 📚 The three cards, numbered 1 to 3 as on a shelf.
-- 3枚のカード。本棚のように 1〜3 の番号を付ける
filledCards :: [(WorkId, Card)]
filledCards = zip (map WorkId [1 ..])
                  [dragonFilled, librarianFilled, copyistFilled]
-- ANCHOR_END: forms

-- ANCHOR: fields
-- 🔑 Two attributes the program knows, with their types.
-- プログラムが知っている属性を2つ。型つきで
oshi :: Field Int
oshi = Field "推し度" NumberK

boughtOn :: Field Day
boughtOn = Field "買った日" DateK
-- ANCHOR_END: fields

-- ANCHOR: dragonFix
-- ✏️ The dragon book, fixed by hand. 1.1 and 1.2 were one section in the
--    real book (so 1.2, number 4, is gone), a fifth chapter was missed
--    (new: no number yet), and the creator was not quite right.
-- 手で直したドラゴン学。本物の本では 1.1 と 1.2 は1つの節だった（だから
-- 番号 4 の 1.2 は消える）。第5章を入れ忘れていた（新しい行：番号はまだ
-- ない）。作者も、少しちがっていた
dragonFix :: Revision
dragonFix = Revision
  { newTitle   = "はじめてのドラゴン学"
  , newCreator = "ギルド図書室・竜の会"
  , newToc     =
      [ Node (kept 1 "第1部 ドラゴンを知る" 2)
          [ Node (kept 2 "第1章 見分け方" 4)
              [ Node (kept 3 "1.1 うろこの色と火" 18) [] ]
          , Node (kept 5 "第2章 すみかと季節" 24) []
          ]
      , Node (kept 6 "第2部 ドラゴンと話す" 2)
          [ Node (kept 7 "第3章 あいさつの作法" 16) []
          , Node (kept 8 "第4章 怒らせたときは" 12) []
          , Node (fresh  "第5章 ドラゴンと暮らす" 20) []
          ]
      ]
  }
  where
    kept i = Item (Just (EntryId i))
    fresh  = Item Nothing
-- ANCHOR_END: dragonFix
-- ANCHOR_END: all
