-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-- 🧪 MCP: the checks of the tree the AI sends (pure), the key, and the
--    tools through JSON-RPC, on a shelf in memory.
-- MCP のテスト：AI が送る木の確かめ（純粋）、鍵、JSON-RPC を通した道具
-- （メモリの本棚で）
module McpSpec (mcpSpec) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Test.Hspec

import Tsundoku.Api.Json (detailOf)
import Tsundoku.Card (Card (..), newCard)
import Tsundoku.Examples
import Tsundoku.Mcp.Toc
import Tsundoku.Mcp.Tools
import Tsundoku.Mcp.Wire
import Tsundoku.Shelf
import Tsundoku.Shelf.Memory (newMemoryShelf)
import Tsundoku.Types

import ShelfAnswers.TocDiff
import ShelfAnswers.Unnoted
-- ANCHOR_END: imports

-- ANCHOR: helpers
-- 📗 A table the AI might send, and the same with one change.
-- AI が送りそうな目次と、1か所だけちがうもの
eggs :: TocDraft
eggs = TocDraft "竜の飼い方入門" "竜田まもる" 120
  [ Chapter "第1章 卵" 1 [Chapter "1.1 温め方" 3 []]
  , Chapter "第2章 ひな" 20 []
  , Chapter "第3章 巣立ち" 60 [] ]

-- 📗 eggs, with another table.
-- eggs の目次だけを取り替えたもの
withToc :: [Chapter] -> TocDraft
withToc chapters = TocDraft eggs.title eggs.creator eggs.lastPage chapters

-- 📮 Call a tool through JSON-RPC, and read the text and isError.
-- JSON-RPC で道具を呼んで、文字と isError を読む
callTool :: [Tool] -> Text -> Value -> IO (Bool, Text)
callTool tools name arguments = do
  Just answer <- handle "test" tools $ object
    [ "jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int)
    , "method"  .= ("tools/call" :: Text)
    , "params"  .= object ["name" .= name, "arguments" .= arguments] ]
  let read' = parseMaybe $ withObject "answer" $ \o -> do
        r <- o .: "result"
        [c] <- r .: "content"
        (,) <$> r .: "isError" <*> c .: "text"
  pure (fromMaybe (True, "unreadable") (read' answer))

-- 🧾 The arguments of add_toc for eggs.
-- eggs の add_toc の引数
eggsArgs :: [(Key, Value)]
eggsArgs =
  [ "title" .= ("竜の飼い方入門" :: Text), "creator" .= ("竜田まもる" :: Text)
  , "last_page" .= (120 :: Int)
  , "toc" .= [ chapter "第1章 卵" 1 [chapter "1.1 温め方" 3 []]
             , chapter "第2章 ひな" 20 [], chapter "第3章 巣立ち" 60 [] ] ]
  where
    chapter :: Text -> Int -> [Value] -> Value
    chapter h p cs = object ["heading" .= h, "page" .= p, "children" .= cs]

clock :: IO UTCTime
clock = pure (UTCTime (fromGregorian 2026 11 9) 0)
-- ANCHOR_END: helpers

-- ANCHOR: mcpSpec
mcpSpec :: Spec
mcpSpec = do
  tocSpec
  toolSpec
  answersSpec
-- ANCHOR_END: mcpSpec

-- ANCHOR: tocSpec
tocSpec :: Spec
tocSpec = describe "the tree from the AI" $ do
  -- 「まずい所を全部、道順つきで」
  it "finds every problem, with the path to it" $
    tocProblems (withToc
      [ Chapter "第1章" 1 [Chapter "" 2 [], Chapter "1.1" 5 []]
      , Chapter "第2章" 30 [Chapter "1.1" 31 [], Chapter "1.1" 32 []]
      , Chapter "第3章" 12 [] ])
      `shouldBe`
        [ EmptyHeading ["第1章"]
        , SameHeading ["第2章"] "1.1"
        , PageBefore ["第3章"] 12 "1.1" 32 ]
  -- 「空の目次、深すぎ、本の外のページ」
  it "says when the table is empty, too deep, or off the book" $ do
    tocProblems (withToc []) `shouldBe` [NoChapters]
    let deep = Chapter "a" 1 [Chapter "b" 1 [Chapter "c" 1
                 [Chapter "d" 1 [Chapter "e" 1 []]]]]
    tocProblems (withToc [deep]) `shouldBe` [TooDeep ["a", "b", "c", "d", "e"]]
    tocProblems (withToc [Chapter "a" 121 []]) `shouldBe` [PageOutside ["a"] 121]
  -- 「直し方がわかる言葉で」
  it "says what to fix" $
    explainToc (PageBefore ["第3章"] 12 "第2章" 30)
      `shouldBe` "「第3章」の始まりのページ 12 が、前の「第2章」の 30 より前です。\
                 \ページを見直してください"
  -- 「始まりのページから、その項目だけのページ数を」
  it "makes a book with pages from where chapters start" $
    fmap preview (draftWork eggs) `shouldBe` Right
      [ "竜の飼い方入門（竜田まもる）"
      , "・第1章 卵（2ページ）", "  ・1.1 温め方（17ページ）"
      , "・第2章 ひな（40ページ）", "・第3章 巣立ち（61ページ）" ]
  -- 「作者がわからなければ、空のかっこは出さない」
  it "shows no empty brackets when the creator is not known" $
    fmap (take 1 . preview)
         (draftWork (TocDraft "竜の飼い方入門" "" 120 [Chapter "第1章 卵" 1 []]))
      `shouldBe` Right ["竜の飼い方入門"]
  -- 「鍵は、本棚が振った番号に左右されない」
  it "keys a work by its shape, not by the numbers the shelf gave" $ do
    shelf <- newMemoryShelf
    _     <- shelf.works.addWork dragonBook
    Right w <- pure (draftWork eggs)
    wid   <- shelf.works.addWork w
    Just c <- shelf.works.getCard wid
    workKey c.work `shouldBe` workKey w
    workKey w `shouldNotBe` workKey (Work "竜の飼い方入門 第2版" "竜田まもる" w.contents)
  -- 「同じ目次は Already、題が空なら核が断る」
  it "plans: already there, or refused by the core" $ do
    Right w <- pure (draftWork eggs)
    planToc [(WorkId 7, newCard w)] eggs `shouldBe` Already (WorkId 7)
    planToc [] (TocDraft " " "" 120 eggs.toc)
      `shouldBe` Problems ["題が空っぽです"]
-- ANCHOR_END: tocSpec

-- ANCHOR: toolSpec
toolSpec :: Spec
toolSpec = describe "the tools" $ do
  -- 「見るだけのサーバーには、見る道具しかない」
  it "gives only the looking tools when read-only" $ do
    shelf <- newMemoryShelf
    Just answer <- handle "test" (shelfTools ReadOnly clock shelf) $ object
      [ "jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int)
      , "method" .= ("tools/list" :: Text) ]
    let names = parseMaybe (withObject "a" $ \o ->
                  o .: "result" >>= (.: "tools") >>= mapM (.: "name")) answer
    names `shouldBe` Just (["list_works", "show_work", "search"] :: [Text])
    -- A tool it does not have is a protocol error (-32602; the MCP spec).
    -- 持っていない道具は、MCP の仕様のとおりプロトコルのエラー（-32602）
    Just refused <- handle "test" (shelfTools ReadOnly clock shelf) $ object
      [ "jsonrpc" .= ("2.0" :: Text), "id" .= (2 :: Int)
      , "method"  .= ("tools/call" :: Text)
      , "params"  .= object [ "name"      .= ("add_toc" :: Text)
                            , "arguments" .= object eggsArgs ] ]
    let code = parseMaybe (withObject "a" $ \o ->
                 o .: "error" >>= (.: "code")) refused
    code `shouldBe` Just (-32602 :: Int)
  -- 「下書き → commit → もう一度でも1回分」
  it "drafts, commits with the key, and never adds twice" $ do
    shelf <- newMemoryShelf
    let tools = shelfTools ReadWrite clock shelf
    (_, draft) <- callTool tools "add_toc" (object eggsArgs)
    let key = T.drop 5 (head' [ l | l <- T.lines draft, "key: " `T.isPrefixOf` l ])
    (False, added) <- callTool tools "add_toc"
      (object (eggsArgs ++ ["commit" .= True, "key" .= key]))
    added `shouldBe` "登録しました：作品 1"
    (False, again) <- callTool tools "add_toc"
      (object (eggsArgs ++ ["commit" .= True, "key" .= key]))
    again `shouldBe` "もう登録してあります：作品 1。二重には登録しません"
    length <$> shelf.works.listCards `shouldReturn` 1
  -- 「下書きとちがう key では登録しない」
  it "refuses a commit whose key does not match the draft" $ do
    shelf <- newMemoryShelf
    (failed, _) <- callTool (shelfTools ReadWrite clock shelf) "add_toc"
      (object (eggsArgs ++ ["commit" .= True, "key" .= ("0000" :: Text)]))
    failed `shouldBe` True
    length <$> shelf.works.listCards `shouldReturn` 0
  -- 「出版社は、作者の欄ではなく、属性「出版社」に」
  it "keeps the publisher in its attribute, not as the creator" $ do
    shelf <- newMemoryShelf
    let tools = shelfTools ReadWrite clock shelf
        args  = eggsArgs ++ ["publisher" .= ("王都出版" :: Text)]
    (_, draft) <- callTool tools "add_toc" (object args)
    draft `shouldSatisfy` T.isInfixOf "出版社：王都出版"
    let key = T.drop 5 (head' [ l | l <- T.lines draft, "key: " `T.isPrefixOf` l ])
    (False, _) <- callTool tools "add_toc"
      (object (args ++ ["commit" .= True, "key" .= key]))
    Just card <- shelf.works.getCard (WorkId 1)
    Map.lookup "出版社" card.attrs `shouldBe` Just (TextV "王都出版")
    card.work.creator `shouldBe` "竜田まもる"
  -- 「ネタバレは、頼まれたときだけ見せる」
  it "hides spoilers unless asked" $ do
    shelf <- newMemoryShelf
    wid <- shelf.works.addWork dragonBook
    let WorkId n = wid
        tools = shelfTools ReadWrite clock shelf
    _ <- callTool tools "add_note"
           (object ["id" .= n, "body" .= ("犯人は司書" :: Text), "spoiler" .= True])
    (_, hidden) <- callTool tools "show_work" (object ["id" .= n])
    (_, shown)  <- callTool tools "show_work" (object ["id" .= n, "spoilers" .= True])
    (T.isInfixOf "犯人" hidden, T.isInfixOf "犯人" shown) `shouldBe` (False, True)
  -- 「AI 向けのカードは、名前：値の行。押すキーの番号は出さない」
  it "shows the AI a card of named lines, with no keys to press" $ do
    shelf <- newMemoryShelf
    wid <- shelf.works.addWork dragonBook
    let WorkId n = wid
    (False, text) <- callTool (shelfTools ReadOnly clock shelf) "show_work"
                              (object ["id" .= n])
    take 4 (T.lines text) `shouldBe`
      [ "作品 1：はじめてのドラゴン学", "作者：ギルド図書室", "種類：本"
      , "状態：積読（次にできること：読み始める）" ]
    T.isInfixOf "1 読み" text `shouldBe` False
  -- 「お知らせには答えない。版は話し合って決める」
  it "answers no notification, and agrees on a version" $ do
    none <- handle "test" [] (object [ "jsonrpc" .= ("2.0" :: Text)
                                     , "method" .= ("notifications/initialized" :: Text) ])
    none `shouldBe` Nothing
    Just hi <- handle "test" [] (object
      [ "jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int)
      , "method" .= ("initialize" :: Text)
      , "params" .= object ["protocolVersion" .= ("2025-06-18" :: Text)] ])
    parseMaybe (withObject "a" $ \o -> o .: "result" >>= (.: "protocolVersion")) hi
      `shouldBe` Just ("2025-06-18" :: Text)
  where
    head' (x : _) = x
    head' []      = ""
-- ANCHOR_END: toolSpec

-- ANCHOR: answersSpec
answersSpec :: Spec
answersSpec = describe "the answers" $ do
  -- 「新しい版で増えた章の道順」
  it "finds the chapters a new edition added" $
    addedChapters eggs.toc
      (eggs.toc ++ [Chapter "第4章 旅立ち" 90 [Chapter "4.1 地図" 91 []]])
      `shouldBe` [["第4章 旅立ち"], ["第4章 旅立ち", "4.1 地図"]]
  -- 「感想のない項目を言う道具は、Glance だけで作れる」
  it "says which entries have no note, from a Glance" $ do
    shelf <- newMemoryShelf
    wid <- shelf.works.addWork dragonBook
    Just card <- shelf.works.getCard wid
    take 2 (unnoted (detailOf (wid, card)))
      `shouldBe` ["第1部 ドラゴンを知る", "第1章 見分け方"]
    let WorkId n = wid
    (False, text) <- callTool [unnotedTool (glance shelf)] "unnoted_entries"
                              (object ["id" .= n])
    length (T.lines text) `shouldBe` 8
-- ANCHOR_END: answersSpec
-- ANCHOR_END: all
