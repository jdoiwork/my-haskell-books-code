-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 📜 The contract: what every shelf must do, wherever it keeps cards.
-- 契約：本棚なら、カードをどこにしまっても、こう動かなければならない
module ShelfContract
  ( shelfContract
  , tornContract
  , tornWork
  , fillForSearch
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Tree (Tree (..), flatten)
import Test.Hspec

import Tsundoku.Card (Card (..), newCard)
import Tsundoku.Card qualified as Card
import Tsundoku.Custom (emptySchema, showValue)
import Tsundoku.Examples
import Tsundoku.Import (readEpubBook, shelveBook, textsFor)
import Tsundoku.Import.Sample (sampleEpub)
import Tsundoku.Import.Types (Imported (..))
import Tsundoku.Rating (score)
import Tsundoku.Search
import Tsundoku.Shelf
import Tsundoku.Types
-- ANCHOR_END: imports

-- ANCHOR: contract
-- 📜 The same tests for any shelf. Each test gets an empty shelf.
-- どの本棚にも同じテスト。テストごとに、空っぽの本棚を受け取る
shelfContract :: SpecWith Shelf
shelfContract = do

  -- 「空っぽから始まる」
  it "starts empty" $ \shelf -> do
    cards <- shelf.listCards
    map fst cards `shouldBe` []

  -- 「どの種類の作品も、しまったとおりに読み戻せる」
  forM_ examples $ \(kind, w) ->
    it ("gives back a new card of a " <> kind <> " as it was") $ \shelf -> do
      wid <- shelf.addWork w
      shelf.getCard wid `shouldReturn` Just (newCard w)

  -- 「番号は、しまった順に、前の続きから振る」
  it "numbers works and entries in order, going on" $ \shelf -> do
    first  <- shelf.addWork dragonBook
    second <- shelf.addWork librarianSerial
    [first, second] `shouldBe` [WorkId 1, WorkId 2]
    card <- fromJust <$> shelf.getCard second
    entryIds (contents (work card)) `shouldBe` map EntryId [9 .. 13]
    headingsOf (contents (work card))
      `shouldBe` headingsOf (contents librarianSerial)

  -- 「消した番号は、二度と使わない」
  it "never gives a removed number again" $ \shelf -> do
    old <- shelf.addWork dragonBook
    shelf.removeWork old `shouldReturn` True
    shelf.removeWork old `shouldReturn` False
    shelf.getCard old `shouldReturn` Nothing
    new  <- shelf.addWork dragonBook
    card <- fromJust <$> shelf.getCard new
    new `shouldBe` WorkId 2
    entryIds (contents (work card)) `shouldBe` map EntryId [9 .. 16]

  -- 「ない作品には、何もできない」
  it "refuses a work that is not on the shelf" $ \shelf -> do
    let nowhere = WorkId 42
        missing = Left (NoSuchWork nowhere)
    shelf.getCard nowhere `shouldReturn` Nothing
    shelf.applyEvent nowhere Start `shouldReturn` missing
    shelf.addNote nowhere (Note OnWork "!" (onDay 2026 11 9) False)
      `shouldReturn` missing
    shelf.logReading nowhere (LogLine (nov 9) (EntryId 1))
      `shouldReturn` missing
    shelf.rate nowhere (surely (score 80)) `shouldReturn` missing
    shelf.rateEntry nowhere (EntryId 1) (surely (score 80))
      `shouldReturn` missing

  -- 「読書の状態は、地図のとおりにだけ動く」
  it "moves only along the map of reading" $ \shelf -> do
    wid <- shelf.addWork lastDragonFilm
    shelf.applyEvent wid Start  `shouldReturn` Right Reading
    shelf.applyEvent wid Finish `shouldReturn` Right Finished
    shelf.applyEvent wid Start
      `shouldReturn` Left (NotAllowed Start Finished)
    fmap reading <$> shelf.getCard wid `shouldReturn` Just Finished

  -- 「核が断るものは、窓口も断る」
  it "refuses what the core refuses" $ \shelf -> do
    wid <- shelf.addWork dragonBook
    shelf.addNote wid (Note OnWork "  " (onDay 2026 11 9) False)
      `shouldReturn` Left EmptyNote
    shelf.addNote wid
        (Note (OnEntry (EntryId 99)) "?" (onDay 2026 11 9) False)
      `shouldReturn` Left (NoSuchEntry (EntryId 99))
    shelf.logReading wid (LogLine (nov 9) (EntryId 99))
      `shouldReturn` Left (NoSuchEntry (EntryId 99))
    shelf.rateEntry wid (EntryId 99) (surely (score 80))
      `shouldReturn` Left (NoSuchEntry (EntryId 99))
    fmap notes <$> shelf.getCard wid `shouldReturn` Just []

  -- 「付け直した評価は、新しいほう」
  it "keeps the newest rating" $ \shelf -> do
    wid <- shelf.addWork librarianSerial
    _   <- shelf.rate wid (surely (score 80))
    _   <- shelf.rate wid (surely (score 90))
    _   <- shelf.rateEntry wid (EntryId 2) (surely (score 70))
    _   <- shelf.rateEntry wid (EntryId 2) (surely (score 100))
    card <- fromJust <$> shelf.getCard wid
    rating card `shouldBe` Just (surely (score 90))
    Map.toList (entryRatings card)
      `shouldBe` [(EntryId 2, surely (score 100))]

  -- 「核が作ったカードと、窓口を通したカードは、同じ」
  it "keeps the dragon card just as the core made it" $ \shelf -> do
    wid <- addDragon shelf
    shelf.getCard wid `shouldReturn` Just dragonCard

  -- 自分で決める属性と状態（5章目）
  customContract
  -- 取り込み（6章目）
  importContract
  searchContract
  -- 作品を手で直す（11章目）
  reviseContract
  -- 読んだ印を外す（11章目）
  unreadContract
  where
    examples =
      [ ("book",   dragonBook)
      , ("serial", copyistSerial)
      , ("series", detectiveDrama)
      , ("film",   lastDragonFilm)
      , ("game",   dungeonGame)
      ]

-- 🐉 Put the dragon card on the shelf, one step at a time, as the
--    core made it.
-- ドラゴン学のカードを、核が作ったとおりに、1つずつ本棚に置く
addDragon :: Shelf -> IO WorkId
addDragon shelf = do
  wid <- shelf.addWork dragonBook
  _   <- shelf.applyEvent wid Start
  forM_ (notes dragonCard)   (shelf.addNote wid)
  forM_ (rating dragonCard)  (shelf.rate wid)
  forM_ (readLog dragonCard) (shelf.logReading wid)
  pure wid
-- ANCHOR_END: contract

-- ANCHOR: custom
-- 📜 The contract for my own attributes and states.
-- 自分で決める属性と状態の契約
customContract :: SpecWith Shelf
customContract = do

  -- 「定義は、決めた順に読み戻せる」
  it "keeps the definitions in order" $ \shelf -> do
    shelf.getSchema `shouldReturn` emptySchema
    defineAll shelf
    shelf.getSchema `shouldReturn` librarySchema

  -- 「核が断る定義は、窓口も断り、何も残さない」
  it "refuses what the core refuses in a definition" $ \shelf -> do
    defineAll shelf
    shelf.defineAttr (AttrDef "推し度" TextKind)
      `shouldReturn` Left (BadDefinition [NameTaken "推し度"])
    shelf.defineFlow brokenFlow `shouldReturn` Left (BadDefinition
      [ NoSuchState "全話配信"
      , Unreachable "全話配信済み", Unreachable "打ち切り" ])
    shelf.getSchema `shouldReturn` librarySchema

  -- 「属性と状態を書いたカードも、核が作ったとおりに読み戻せる」
  it "keeps attributes and states just as the core made them" $ \shelf -> do
    defineAll shelf
    wid <- addDragon shelf
    shelf.editAttrs wid dragonForm `shouldReturn` Right ()
    shelf.moveFlow wid "貸し出し" "本棚にある" `shouldReturn` Right ()
    shelf.getCard wid `shouldReturn` Just dragonFilled

  -- 「まずい欄は全部まとめて断り、カードはそのまま」
  it "refuses every bad field at once, and keeps the card" $ \shelf -> do
    defineAll shelf
    wid <- shelf.addWork dragonBook
    _   <- shelf.editAttrs wid (Map.fromList [("推し度", "4")])
    shelf.editAttrs wid (Map.fromList
        [("推し度", "すごく"), ("買った日", "11月1日"), ("サイン", "はい")])
      `shouldReturn` Left (BadAttrs
        [ UnknownName "サイン"
        , Unreadable "推し度" "すごく"
        , Unreadable "買った日" "11月1日" ])
    fmap attrs <$> shelf.getCard wid
      `shouldReturn` Just (Map.fromList [("推し度", NumberV 4)])

  -- 「空の欄は値を消し、フォームにない欄はそのまま」
  it "takes a value away with a blank field" $ \shelf -> do
    defineAll shelf
    wid <- shelf.addWork dragonBook
    _   <- shelf.editAttrs wid dragonForm
    _   <- shelf.editAttrs wid (Map.fromList [("推し度", " ")])
    fmap (Map.keys . attrs) <$> shelf.getCard wid
      `shouldReturn` Just ["どこで買った", "サイン本", "形", "買った日"]

  -- 「状態は、はじめの状態から、矢印に沿ってだけ動く」
  it "moves only along the arrows of a flow" $ \shelf -> do
    defineAll shelf
    wid <- shelf.addWork librarianSerial
    let move = shelf.moveFlow wid "連載の状態"
    move "完結"
      `shouldReturn` Left (NoSuchArrow "連載の状態" Nothing "完結")
    move "連載中" `shouldReturn` Right ()
    move "完結"   `shouldReturn` Right ()
    move "休止"
      `shouldReturn` Left (NoSuchArrow "連載の状態" (Just "完結") "休止")
    shelf.moveFlow wid "配信の状態" "配信中"
      `shouldReturn` Left (NoSuchFlow "配信の状態")
    shelf.moveFlow (WorkId 42) "連載の状態" "連載中"
      `shouldReturn` Left (NoSuchWork (WorkId 42))
    fmap states <$> shelf.getCard wid
      `shouldReturn` Just (Map.fromList [("連載の状態", "完結")])

  -- 「絞り込みは、通るカードだけを、番号の順に返す」
  it "finds the cards that pass a search" $ \shelf -> do
    fillShelf shelf
    let found typed =
          fmap (map (title . work . snd)) <$> shelf.findCards typed
    found "連載の状態 = 連載中 and ★ >= 4 and 推し度 >= 3"
      `shouldReturn` Right ["図書室の司書に転生した件"]
    found "★ >= 4 and 推し度 >= 4"
      `shouldReturn`
        Right ["はじめてのドラゴン学", "図書室の司書に転生した件"]
    found "連載の状態 != 完結"
      `shouldReturn` Right ["図書室の司書に転生した件", "迷宮図書館の写本係"]
    found "推し度 >= すごく and 色 = 赤"
      `shouldReturn` Left (BadAttrs
        [Unreadable "推し度" "すごく", UnknownName "色"])

-- 📒 Define everything the library decided.
-- 図書室が決めたものを、全部定義する
defineAll :: Shelf -> IO ()
defineAll shelf = do
  forM_ libraryAttrs $ \def  -> shelf.defineAttr def  `shouldReturn` Right ()
  forM_ libraryFlows $ \flow -> shelf.defineFlow flow `shouldReturn` Right ()

-- 📚 Three cards with ratings, attributes, and states, through the
--    window only.
-- 評価と属性と状態を書いたカードを3枚。窓口だけを通して
fillShelf :: Shelf -> IO ()
fillShelf shelf = do
  defineAll shelf
  forM_ filled $ \(w, card, moves) -> do
    wid <- shelf.addWork w
    forM_ (rating card) (shelf.rate wid)
    _   <- shelf.editAttrs wid (formOf card)
    forM_ moves $ \(flow, to) -> shelf.moveFlow wid flow to
  where
    filled =
      [ (dragonBook,      dragonFilled,    [("貸し出し", "本棚にある")])
      , (librarianSerial, librarianFilled, [("連載の状態", "連載中")])
      , (copyistSerial,   copyistFilled,   [ ("連載の状態", "連載中")
                                           , ("連載の状態", "休止") ])
      ]
    formOf card = fmap showValue (attrs card)
-- ANCHOR_END: custom

-- ANCHOR: import
-- 📥 Growing a serial from outside: the numbers come from the shelf.
-- 外から連載を育てる。番号は本棚が振る
importContract :: SpecWith Shelf
importContract = do

  -- 「新しい話は、最後の章の終わりに、本棚の新しい番号で足す」
  it "adds new episodes at the end, with the shelf's numbers" $ \shelf -> do
    _      <- shelf.addWork dragonBook          -- 1 to 8 / 番号 1〜8
    serial <- shelf.addWork librarianSerial     -- 9 to 13 / 番号 9〜13
    shelf.addEpisodes serial 2 `shouldReturn` Right ()
    card <- fromJust <$> shelf.getCard serial
    headingsOf (contents (work card))
      `shouldBe` headingsOf (contents librarianSerial) ++ ["第4話", "第5話"]
    entryIds (contents (work card)) `shouldBe` map EntryId [9 .. 15]
    -- a new episode is an entry like any other: it can be read
    -- 新しい話も、ほかと同じ項目：読める
    shelf.logReading serial (LogLine (nov 9) (EntryId 15))
      `shouldReturn` Right ()
    -- and the next work goes on from there
    -- 次の作品は、その続きの番号から
    next  <- shelf.addWork lastDragonFilm
    later <- shelf.addWork librarianSerial
    next `shouldBe` WorkId 3
    third <- fromJust <$> shelf.getCard later
    take 1 (entryIds (contents (work third))) `shouldBe` [EntryId 16]

  -- 「連載でない作品に、話は足せない。カードはそのまま」
  it "adds no episodes to what is not a serial" $ \shelf -> do
    book <- shelf.addWork dragonBook
    shelf.addEpisodes book 1 `shouldReturn` Left NotASerial
    shelf.getCard book `shouldReturn` Just (newCard dragonBook)
    shelf.addEpisodes (WorkId 42) 1
      `shouldReturn` Left (NoSuchWork (WorkId 42))

  -- 「0話なら、何も変わらない」
  it "changes nothing for no new episodes" $ \shelf -> do
    serial <- shelf.addWork librarianSerial
    shelf.addEpisodes serial 0 `shouldReturn` Right ()
    shelf.getCard serial `shouldReturn` Just (newCard librarianSerial)
-- ANCHOR_END: import

-- ANCHOR: search
-- 🔎 Searching what is inside. Every shelf finds the same hits, with
--    the same trails and snippets; only the order may differ.
-- 中身を探す。どの本棚も、同じものを、同じ道順と抜き出しで見つける。
-- ちがってよいのは、順番だけ
searchContract :: SpecWith Shelf
searchContract = do

  -- 「本文の言葉を見つけて、どの章のどこかを言う」
  it "finds a word in a text, and says where it is" $ \shelf -> do
    _ <- fillForSearch shelf
    shelf.searchText "怠けもの" `shouldReturn` Right
      [ Hit
          { hitWork  = WorkId 1
          , hitTitle = "はじめてのドラゴン学"
          , place    = InText (EntryId 8)
          , trail    = ["第2部 ドラゴンと話す", "第4章 怒らせたときは"]
          , snippet  =
              [ Plain "…がります。 第4章（つづき） ドラゴンは"
              , Match "怠けもの"
              , Plain "ではありません。寝ていても宝の数を覚えて…" ]
          } ]

  -- 「手で探したものと、同じものが見つかる（順番は問わない）」
  it "finds what searching by hand finds" $ \shelf -> do
    texts <- fillForSearch shelf
    cards <- shelf.listCards
    let textsOf wid = if wid == WorkId 1 then texts else Map.empty
        byHand      = [ (wid, card, textsOf wid) | (wid, card) <- cards ]
        searches    = ["火を吐", "宝", "宝 冬", "竜鱗は", "絵", "ドラゴン 雨"]
    forM_ searches $ \typed -> do
      let answer = fmap (`findIn` byHand) (parseSearch typed)
      fmap null answer `shouldBe` Right False
      found <- shelf.searchText typed
      fmap inOrder found `shouldBe` fmap inOrder answer

  -- 「記号を打っても、問い合わせの文法にはならない」
  it "reads every mark just as it is typed" $ \shelf -> do
    wid <- shelf.addWork dragonBook
    _   <- shelf.addNote wid (Note OnWork marks (onDay 2026 11 9) False)
    forM_ typedMarks $ \typed ->
      fmap (map place) <$> shelf.searchText typed
        `shouldReturn` Right [InNote OnWork False]

  -- 「探す言葉がなければ、断る」
  it "refuses a search with no words" $ \shelf ->
    shelf.searchText "  " `shouldReturn` Left EmptySearch

  -- 「本文は、目次にある項目にだけしまえる」
  it "keeps a text only for an entry in the table" $ \shelf -> do
    wid <- shelf.addWork dragonBook
    shelf.setText wid (EntryId 99) "どこ？"
      `shouldReturn` Left (NoSuchEntry (EntryId 99))
    shelf.setText (WorkId 42) (EntryId 1) "どこ？"
      `shouldReturn` Left (NoSuchWork (WorkId 42))
    shelf.searchText "どこ？" `shouldReturn` Right []

  -- 「本文を書き直すと、見つかるものも変わる」
  it "finds the new text after a text changes" $ \shelf -> do
    wid <- shelf.addWork dragonBook
    shelf.setText wid (EntryId 3) "古い本文です" `shouldReturn` Right ()
    shelf.setText wid (EntryId 3) "新しい本文です" `shouldReturn` Right ()
    shelf.searchText "古い本文" `shouldReturn` Right []
    fmap (map place) <$> shelf.searchText "新しい本文"
      `shouldReturn` Right [InText (EntryId 3)]

  -- 「作品を外すと、その作品のものは見つからない」
  it "finds nothing of a removed work" $ \shelf -> do
    _ <- fillForSearch shelf
    _ <- shelf.removeWork (WorkId 1)
    fmap (map hitWork) <$> shelf.searchText "ドラゴン"
      `shouldReturn` Right [WorkId 2]
    shelf.searchText "怠けもの" `shouldReturn` Right []

  -- 「新しい話の見出しも、見つかる」
  it "finds the headings of new episodes" $ \shelf -> do
    serial <- shelf.addWork librarianSerial
    _      <- shelf.addEpisodes serial 1
    fmap (map (\h -> (place h, trail h))) <$> shelf.searchText "第4話"
      `shouldReturn`
        Right [(InHeading (EntryId 6), ["第2章 禁書の棚", "第4話"])]
  where
    -- a note with marks that mean something to FTS5 or PGroonga
    -- FTS5 や PGroonga にとって意味のある記号を書いた感想
    marks      = "\"引用\" と (かっこ) と OR と -x と NEAR( と ab*c と \\"
    typedMarks = ["\"引用\"", "(かっこ)", "OR", "-x", "NEAR(", "ab*c", "\\"]
    inOrder = sortOn (\h -> (hitWork h, place h))

-- 📚 The dragon book taken in from the sample EPUB, texts and all, with
--    the notes of the dragon card; then a film and a serial. Gives back
--    the texts of the dragon book (the answer for searching by hand).
-- 見本の EPUB から取り込んだドラゴン学（本文も）と、ドラゴン学のカードの
-- 感想。それから映画と連載。ドラゴン学の本文を返す（手で探す答えのため）
fillForSearch :: Shelf -> IO (Map EntryId Text)
fillForSearch shelf = do
  got    <- either (fail . show) pure (readEpubBook sampleEpub)
  dragon <- shelveBook shelf "dragon" (draft got)
  forM_ (notes dragonCard) (shelf.addNote dragon)
  _      <- shelf.addWork lastDragonFilm
  _      <- shelf.addWork librarianSerial
  card   <- fromJust <$> shelf.getCard dragon
  pure (Map.fromList (textsFor (contents (work card)) (fmap snd (draft got))))
-- ANCHOR_END: search

-- ANCHOR: headings
-- 📑 The headings of a work, from top to bottom (numbers left out).
-- 作品の目次の見出しを、上から順に（番号は見ない）
headingsOf :: Contents -> [Text]
headingsOf what = case what of
  Book   toc -> fromToc toc
  Serial toc -> fromToc toc
  Series toc -> fromToc toc
  Film   _   -> []
  Game       -> []
  where
    fromToc toc = map heading (concatMap flatten toc)
-- ANCHOR_END: headings

-- ANCHOR: torn
-- 🧨 A shelf in a database: a failure in the middle of a write leaves
--    nothing behind. The third entry has -1 pages, which the table's
--    CHECK refuses, after the work and two entries are in.
-- データベースの本棚：書いている途中で失敗したら、何も残らない。
-- 3つ目の項目が -1 ページ。作品と2つの項目を入れたあとで、表の CHECK が断る
tornContract :: SpecWith Shelf
tornContract =
  -- 「途中で転んだら、全部なかったことに」
  it "keeps all of a new work, or none of it" $ \shelf -> do
    result <- try (shelf.addWork tornWork)
    isLeft (result :: Either SomeException WorkId) `shouldBe` True
    cards <- shelf.listCards
    map fst cards `shouldBe` []

-- 📕 A book whose third entry has -1 pages.
-- 3つ目の項目が -1 ページの本
tornWork :: Work
tornWork = Work "破れた本" "だれか" (Book (numbered
  [ Node ("第1章", Pages 10)
      [ Node ("1.1", Pages 5)    []
      , Node ("1.2", Pages (-1)) []
      ]
  ]))
-- ANCHOR_END: torn
-- ANCHOR: reviseContract
-- ✏️ Fixing a work by hand, on any shelf.
-- 作品を手で直す（どの本棚でも）
reviseContract :: SpecWith Shelf
reviseContract = do

  -- 「核が直したカードと、窓口で直したカードは、同じ」
  it "keeps the fixed card just as the core made it" $ \shelf -> do
    wid <- addDragon shelf
    shelf.reviseWork wid dragonFix `shouldReturn` Right ()
    shelf.getCard wid `shouldReturn` Just
      (surely (Card.revise (EntryId 9) dragonFix dragonCard))

  -- 「残る行は番号を保ち、新しい行は続きの番号。次の作品はその先から」
  it "keeps the numbers, and goes on after the new ones" $ \shelf -> do
    wid  <- shelf.addWork dragonBook
    _    <- shelf.reviseWork wid dragonFix
    card <- fromJust <$> shelf.getCard wid
    entryIds (contents (work card))
      `shouldBe` map EntryId [1, 2, 3, 5, 6, 7, 8, 9]
    next  <- shelf.addWork librarianSerial
    later <- fromJust <$> shelf.getCard next
    take 1 (entryIds (contents (work later))) `shouldBe` [EntryId 10]

  -- 「探すものも、直したとおりに変わる」
  it "finds what the fixed work says, and not what it said" $ \shelf -> do
    wid <- addDragon shelf
    _   <- shelf.setText wid (EntryId 4) "火を吐く竜の本文"
    _   <- shelf.setText wid (EntryId 3) "うろこの本文"
    _   <- shelf.reviseWork wid dragonFix
    places shelf "火を吐く竜" `shouldReturn` Right []
    places shelf "うろこの本文" `shouldReturn` Right [InText (EntryId 3)]
    places shelf "火を吐くもの" `shouldReturn` Right []
    places shelf "暮らす" `shouldReturn` Right [InHeading (EntryId 9)]
    places shelf "竜の会" `shouldReturn` Right [InCreator]
    places shelf "吐かない種" `shouldReturn` Right [InNote OnWork True]

  -- 「消すと選べば、消える行の感想も消え、探しても見つからない」
  it "drops the notes on lines that are gone, when asked" $ \shelf -> do
    wid  <- addDragon shelf
    shelf.reviseWork wid dragonFix { notesOnRemoved = DropNotes }
      `shouldReturn` Right ()
    card <- fromJust <$> shelf.getCard wid
    map target (notes card) `shouldBe` [OnWork]
    places shelf "吐かない種" `shouldReturn` Right []
    places shelf "読みやすい" `shouldReturn` Right [InNote OnWork False]

  -- 「よその番号・2回出る番号・形のまずい目次は断り、カードはそのまま」
  it "refuses bad numbers and shapes, and keeps the card" $ \shelf -> do
    wid    <- addDragon shelf
    _      <- shelf.addWork librarianSerial    -- 9 to 13 / 番号 9〜13
    before <- shelf.getCard wid
    let line i h = Node (Item (Just (EntryId i)) h 1) []
        with ls  = dragonFix { newToc = ls }
    shelf.reviseWork wid (with [line 9 "よその話"])
      `shouldReturn` Left (NoSuchEntry (EntryId 9))
    shelf.reviseWork wid (with [line 1 "あ", line 1 "い"])
      `shouldReturn` Left (EntryTwice (EntryId 1))
    shelf.reviseWork wid (with [line 1 "同じ", line 2 "同じ"])
      `shouldReturn` Left (BadToc [SameHeading [] "同じ"])
    shelf.reviseWork (WorkId 42) dragonFix
      `shouldReturn` Left (NoSuchWork (WorkId 42))
    shelf.getCard wid `shouldReturn` before

  -- 「映画は題を直せるが、目次は持てない」
  it "fixes the title of a film, but gives it no table" $ \shelf -> do
    wid <- shelf.addWork lastDragonFilm
    let renamed = Revision "最後のドラゴン（完全版）" "竜の会" [] MoveToWork
    shelf.reviseWork wid renamed { newToc = newToc dragonFix }
      `shouldReturn` Left NoTable
    shelf.reviseWork wid renamed `shouldReturn` Right ()
    fmap (title . work) <$> shelf.getCard wid
      `shouldReturn` Just "最後のドラゴン（完全版）"
  where
    places shelf typed = fmap (map place) <$> shelf.searchText typed
-- ANCHOR_END: reviseContract

-- ANCHOR: unreadContract
-- 🧽 Taking back a mark, on any shelf.
-- 読んだ印を外す（どの本棚でも）
unreadContract :: SpecWith Shelf
unreadContract = do

  -- 「核が外したカードと、窓口で外したカードは、同じ」
  it "takes back a mark just as the core does" $ \shelf -> do
    wid <- addDragon shelf
    shelf.unread wid (EntryId 4) `shouldReturn` Right ()
    shelf.getCard wid
      `shouldReturn` Just (surely (Card.unread (EntryId 4) dragonCard))

  -- 「2回読んだ項目も、1回で外れる。ほかの印は残る」
  it "drops every line of the entry, and only of it" $ \shelf -> do
    wid <- addDragon shelf
    _   <- shelf.logReading wid (LogLine (nov 9) (EntryId 3))
    _   <- shelf.unread wid (EntryId 3)
    card <- fromJust <$> shelf.getCard wid
    map readEntry (readLog card) `shouldBe` map EntryId [1, 2, 4, 5]

  -- 「読んでいない項目を外しても、何も変わらない」
  it "changes nothing for an entry not read yet" $ \shelf -> do
    wid    <- shelf.addWork dragonBook
    before <- shelf.getCard wid
    shelf.unread wid (EntryId 6) `shouldReturn` Right ()
    shelf.getCard wid `shouldReturn` before

  -- 「ない項目、ない作品は断る」
  it "refuses an entry or a work that is not there" $ \shelf -> do
    wid <- addDragon shelf
    shelf.unread wid (EntryId 99)
      `shouldReturn` Left (NoSuchEntry (EntryId 99))
    shelf.unread (WorkId 42) (EntryId 1)
      `shouldReturn` Left (NoSuchWork (WorkId 42))
    shelf.getCard wid `shouldReturn` Just dragonCard
-- ANCHOR_END: unreadContract
-- ANCHOR_END: all
