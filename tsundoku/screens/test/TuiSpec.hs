-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 The terminal screen: the model's steps (pure), the lines it draws
--    (pure), and the runner asking a real server on this machine.
-- ターミナルの画面のテスト：モデルの1歩（純粋）、描く行（純粋）、
-- このマシンの本物のサーバーに聞く、動かす側
module TuiSpec (tuiSpec) where

import Data.Time (UTCTime (..), fromGregorian)
import Graphics.Vty qualified as Vty
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client (BaseUrl (..), Scheme (..))
import Test.Hspec

import Tsundoku.Api.Client (remoteApi)
import Tsundoku.Api.Json
import Tsundoku.Examples
import Tsundoku.Html.Server (siteApp)
import Tsundoku.Rating (RatingStyle (..))
import Tsundoku.Shelf
import Tsundoku.Shelf.Memory (newMemoryShelf)
import Tsundoku.Tui.Model
import Tsundoku.Tui.Run
import Tsundoku.Tui.View

import ShelfAnswers.TuiSearch
-- ANCHOR_END: imports

-- ANCHOR: helpers
-- 📗 Two works on the shelf, as the API sends them.
-- API が送る形の、本棚の2作品
twoWorks :: [WorkSummary]
twoWorks =
  [ dragonRated 90
  , WorkSummary 5 "ダンジョン司書" "ギルド図書室" "game" "Piled"
                Nothing Nothing ]

-- 📗 The dragon book, reading, with these points.
-- 読書中のドラゴン学。点はこれ
dragonRated :: Int -> WorkSummary
dragonRated points =
  WorkSummary 1 "はじめてのドラゴン学" "ギルド図書室" "book" "Reading"
              (Just points) (Just (ProgressNumbers 48 78 "pages" 62))

-- 🗂️ A card that can be finished or stopped, rated 73.
-- 読み終えるか中断できる、73点のカード
reading73 :: CardDetail
reading73 = cardRated 73

-- 🗂️ The same card, with these points.
-- 同じカードで、点はこれ
cardRated :: Int -> CardDetail
cardRated points = CardDetail
  { work    = dragonRated points
  , next    = ["Finish", "Stop"]
  , minutes = Nothing
  , toc     = [ TocItem 1 "第1部" 2 True Nothing
                  [TocItem 2 "第1章" 4 False Nothing []] ]
  , notes   = [ NoteItem Nothing "絵が多い" "2026-11-01" False
              , NoteItem (Just 2) "犯人は司書" "2026-11-02" True ]
  }

-- 🖥️ The model with the shelf loaded.
-- 本棚を読みこんだモデル
onShelf :: Model
onShelf = arrive (Works twoWorks) initial
-- ANCHOR_END: helpers

-- ANCHOR: tuiSpec
-- 🧪 The model, the view, the runner, and the answers of the training.
-- モデル、ビュー、動かす側、トレーニングの解答
tuiSpec :: Spec
tuiSpec = do
  modelSpec
  viewSpec
  runSpec
  answersSpec
-- ANCHOR_END: tuiSpec

-- ANCHOR: modelSpec
modelSpec :: Spec
modelSpec = describe "the model" $ do
  -- 「j と k で動く。端より先へは行かない」
  it "moves with j and k, and stops at the ends" $ do
    (fst (step Down onShelf)).cursor `shouldBe` 1
    (fst (step Down (fst (step Down onShelf)))).cursor `shouldBe` 1
    (fst (step Up onShelf)).cursor `shouldBe` 0
  -- 「Enter で、選んだ作品のカードを聞く」
  it "asks for the card of the chosen work on Enter" $
    snd (step Open (fst (step Down onShelf))) `shouldBe` [LoadCard 5]
  -- 「数字は、いまできることの n 番目。聞いたら読み直す」
  it "sends the n-th thing I can do, then reads the card again" $ do
    let opened = arrive (Card reading73 []) onShelf
    snd (step (Pick 2) opened) `shouldBe` [Happen 1 "Stop", LoadCard 1]
    snd (step (Pick 3) opened) `shouldBe` []
    snd (step (Pick 0) opened) `shouldBe` []
  -- 「+ と - は、★を半分（10点）ずつ。0〜100 に収める」
  it "rates half a star more or less, within 0 to 100" $ do
    let opened = arrive (Card reading73 []) onShelf
    snd (step More opened) `shouldBe` [Rate 1 83, LoadCard 1]
    snd (step Less opened) `shouldBe` [Rate 1 63, LoadCard 1]
    let top = arrive (Card (cardRated 100) []) onShelf
    snd (step More top) `shouldBe` [Rate 1 100, LoadCard 1]
  -- 「Esc で本棚へ戻り、本棚を読み直す。q はどこでも終わる」
  it "goes back with Esc, and quits with q anywhere" $ do
    let opened = arrive (Card reading73 []) onShelf
    step Back opened `shouldBe` (opened { screen = OnShelf }, [LoadWorks])
    snd (step QuitKey opened)  `shouldBe` [Quit]
    snd (step QuitKey onShelf) `shouldBe` [Quit]
  -- 「断られたら、その理由を下の行に」
  it "shows why, when the API says no" $ do
    let refused = Refused (Problem "bad-score" ["点は 0〜100 です：101"])
    (arrive refused onShelf).message `shouldBe` "点は 0〜100 です：101"
-- ANCHOR_END: modelSpec

-- ANCHOR: viewSpec
viewSpec :: Spec
viewSpec = describe "the view" $ do
  -- 「バーは、API が送った数から」
  it "draws the bar from the numbers the API sent" $
    barText 10 (ProgressNumbers 48 78 "pages" 62) `shouldBe` "▓▓▓▓▓▓░░░░  62%"
  -- 「全角の文字は2桁と数えてそろえる」
  it "lines up the shelf, counting full-width letters as two" $
    map workLine twoWorks `shouldBe`
      [ "読書中  ★4.5  はじめてのドラゴン学"
      , "積読    —     ダンジョン司書" ]
  -- 「状態と、番号つきのできること」
  it "numbers the things I can do now" $
    nextLine "Reading" ["Finish", "Stop"]
      `shouldBe` "読書中  1 読み終えた  2 中断する"
  -- 「目次は深さで字下げ、ネタバレの感想は中身を見せない」
  it "indents the table, and hides what a spoiler says" $ do
    tocLines reading73.toc `shouldBe` ["✓ 第1部", "  ・第1章"]
    noteLines reading73.toc reading73.notes `shouldBe`
      [ "「第1章」 （ネタバレ。ブラウザーで読む）"
      , "「作品の全体」 絵が多い" ]
  -- 「属性は、定義の種類どおりに見せる」
  it "shows my own attributes as their kinds say" $
    fieldLines [ FormField "推し度" "number" [] "4"
               , FormField "サイン本" "yesno" [] "はい"
               , FormField "形" "choice" ["紙", "電子"] "" ]
      `shouldBe` ["推し度：4", "サイン本：[x]", "形：—"]
  -- 「本物のキーを、モデルのキーに」
  it "reads real keys as the model's keys" $
    map keyOf [Vty.KChar 'j', Vty.KEnter, Vty.KChar '3', Vty.KChar 'x']
      `shouldBe` [Just Down, Just Open, Just (Pick 3), Nothing]
-- ANCHOR_END: viewSpec

-- ANCHOR: runSpec
runSpec :: Spec
runSpec = describe "the runner" $
  -- 「本物のサーバー（/api の下）に、servant-client で聞く」
  it "asks a real server under /api, with servant-client" $ do
    shelf <- newMemoryShelf
    _     <- shelf.addWork dragonBook
    mapM_ shelf.defineAttr libraryAttrs
    let clock = pure (UTCTime (fromGregorian 2026 11 9) 0)
    testWithApplication (pure (siteApp FiveStars clock shelf)) $ \port -> do
      env <- remoteApi (BaseUrl Http "127.0.0.1" port "api")
      Works ws <- perform env LoadWorks
      map (.title) ws `shouldBe` ["はじめてのドラゴン学"]
      Card c fs <- perform env (LoadCard 1)
      (c.next, length fs) `shouldBe` (["Start"], 5)
      perform env (Happen 1 "Finish") `shouldReturn`
        Refused (Problem "not-allowed"
                         ["いまの読書の状態では、それはできません"])
      perform env (Rate 1 80) `shouldReturn` Done
      Card again _ <- perform env (LoadCard 1)
      again.work.rating `shouldBe` Just 80
-- ANCHOR_END: runSpec

-- ANCHOR: answersSpec
answersSpec :: Spec
answersSpec = describe "the answers" $
  -- 「見つかった語を【】で囲む」
  it "puts the words it found in 【】" $
    hitLines [ HitItem 1 "はじめてのドラゴン学" "text" (Just 8) False
                 ["第2部", "第4章"]
                 [ Snip "ドラゴンは" False, Snip "怠けもの" True
                 , Snip "では" False ] ]
      `shouldBe` [ "1. はじめてのドラゴン学 > 第2部 > 第4章"
                 , "   ドラゴンは【怠けもの】では" ]
-- ANCHOR_END: answersSpec
-- ANCHOR_END: all
