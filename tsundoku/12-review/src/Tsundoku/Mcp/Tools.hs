-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-- | The tools the AI can use. Each one only calls the window, like the
--   handlers of the API. Which windows a set of tools gets decides what
--   the AI can do: the looking tools get only a Glance.
-- AI が使える道具。どれも、API のハンドラーと同じく、窓口を呼ぶだけ。
-- 道具の組がどの窓口をもらうかで、AI にできることが決まる。見る道具が
-- もらうのは Glance だけ
module Tsundoku.Mcp.Tools
  ( Mode (..)
  , shelfTools
  , lookTools
  , editTools
  ) where

-- JSON, text, and time
-- JSON、文字列、日時
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)

import Tsundoku.Api.Json
import Tsundoku.Form (explain)
import Tsundoku.Mcp.Toc
import Tsundoku.Mcp.Wire
import Tsundoku.Shelf
import Tsundoku.Tui.View (barText, nextLine, tocLines, workLine)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: mode
-- 🔐 How much the AI may do: only look, or also write.
-- AI にどこまで許すか：見るだけか、書くことも
data Mode = ReadOnly | ReadWrite
  deriving (Show, Eq)

-- 🧰 The tools for a mode. Read-only gets the looking tools, made from
--    a Glance; there is no window here that could change the shelf.
-- モードの道具。見るだけなら、Glance から作った見る道具だけ。ここには、
-- 本棚を変えられる窓口が1つもない
shelfTools :: Mode -> IO UTCTime -> Shelf -> [Tool]
shelfTools mode clock shelf = case mode of
  ReadOnly  -> lookTools (glance shelf)
  ReadWrite -> lookTools (glance shelf)
               ++ editTools clock shelf.works shelf.reading
-- ANCHOR_END: mode

-- ANCHOR: look
-- 👀 Tools that only look: the list, one work, and search.
-- 見るだけの道具：一覧、作品1つ、検索
lookTools :: Glance -> [Tool]
lookTools g =
  [ Tool "list_works" "本棚の作品の一覧（番号・状態・評価・題）"
         (objectSchema [] [])
         (\_ -> Answer . T.unlines . map line <$> g.listCards)
  , Tool "show_work"
         "作品1つ：進み具合・読書の状態・目次（✓は読了）・感想"
         (objectSchema [ ("id",       intP  "作品の番号")
                       , ("spoilers", boolP "ネタバレの感想も見せる") ]
                       ["id"])
         showWork
  , Tool "search" "本棚の中身（題・見出し・感想・本文）を探す"
         (objectSchema [("words", textP "探す言葉（空白で区切る）")]
                       ["words"])
         searchWords
  ]
  where
    line (WorkId n, card) =
      T.pack (show n) <> ". " <> workLine (summaryOf (WorkId n, card))
    showWork args = case arg "id" args of
      Nothing -> pure (Refuse "id（作品の番号）がありません")
      Just n  -> do
        found <- g.getCard (WorkId n)
        pure $ case found of
          Nothing   -> Refuse (T.unlines (explain (NoSuchWork (WorkId n))))
          Just card -> Answer (cardText spoilers (detailOf (WorkId n, card)))
        where spoilers = fromMaybe False (arg "spoilers" args)
    searchWords args = do
      found <- g.searchText (fromMaybe "" (arg "words" args))
      pure $ case found of
        Left mistake -> Refuse (T.unlines (explain mistake))
        Right hits   -> Answer (T.unlines (map (hitLine . hitItem) hits))
-- ANCHOR_END: look

-- ANCHOR: text
-- 📝 One card as text for the AI: the same lines the terminal shows.
--    Spoilers only when asked.
-- カード1枚を、AI に渡す文字に。ターミナルの画面と同じ行。ネタバレは、
-- 頼まれたときだけ
cardText :: Bool -> CardDetail -> Text
cardText spoilers c = T.unlines $
  [ c.work.title <> "（" <> c.work.creator <> "）"
  , nextLine c.work.reading c.next
  , maybe "（進み具合なし）" (barText 20) c.work.progress ]
  ++ tocLines c.toc
  ++ [ "感想：" <> shown n | n <- reverse c.notes ]
  where
    shown n
      | n.spoiler && not spoilers = "（ネタバレなので伏せています）"
      | n.spoiler                 = "（ネタバレ）" <> n.body
      | otherwise                 = n.body

-- 🔎 One hit as a line: where, and the snippet with 【】.
-- ヒット1つを1行に：どこか、と【】つきの抜き出し
hitLine :: HitItem -> Text
hitLine h = T.pack (show h.work) <> ". "
         <> T.intercalate " > " (h.title : h.trail) <> "："
         <> foldMap piece h.snippet
  where
    piece s = if s.match then "【" <> s.text <> "】" else s.text
-- ANCHOR_END: text

-- ANCHOR: edit
-- ✍️ Tools that write: add a table of contents (first a draft, then
--    commit), and add a note. They get only the windows they need.
-- 書く道具：目次を足す（まず下書き、それから commit）、感想を足す。
-- もらうのは、要る窓口だけ
editTools :: IO UTCTime -> WorkWindow -> ReadingWindow -> [Tool]
editTools clock works reading =
  [ Tool "add_toc" addTocHelp tocSchema addToc
  , Tool "add_note" "作品に感想を1つ書く"
         (objectSchema [ ("id",      intP  "作品の番号")
                       , ("body",    textP "感想")
                       , ("spoiler", boolP "ネタバレなら true") ]
                       ["id", "body"])
         addNote
  ]
  where
    addToc args = case parseEither parseJSON (Object args) of
      Left why    -> pure (Refuse ("目次の形が読めません：" <> T.pack why))
      Right draft -> do
        shelf <- works.listCards
        decideToc works args (planToc shelf draft)
    addNote args = case (arg "id" args, arg "body" args) of
      (Just n, Just body) -> do
        now <- clock
        let note = Note OnWork body now (fromMaybe False (arg "spoiler" args))
        done <- reading.addNote (WorkId n) note
        pure $ case done of
          Left mistake -> Refuse (T.unlines (explain mistake))
          Right ()     -> Answer "感想を書きました"
      _ -> pure (Refuse "id と body がいります")
-- ANCHOR_END: edit

-- ANCHOR: decide
-- 🧭 The two steps of add_toc. Without commit: show the book that would
--    be added, and its key. With commit and that same key: add it. The
--    same table again: say it is there already (nothing is added twice).
-- add_toc の2段階。commit なしなら、足される本と、その鍵を見せる。commit と
-- 同じ鍵があれば、足す。同じ目次がもう一度来たら、もうあると言う（二重には
-- 足さない）
decideToc :: WorkWindow -> Object -> Plan -> IO Reply
decideToc works args plan = case plan of
  Problems why -> pure (Refuse (T.unlines ("目次を直してください：" : why)))
  Already wid  -> pure (Answer ("もう登録してあります：作品 " <> num wid
                               <> "。二重には登録しません"))
  Ready w key
    | not commit -> pure (Answer (T.unlines (
        "下書きです。まだ登録していません。" : preview w
        ++ [ "key: " <> key
           , "よければ、同じ引数に commit: true と、この key を付けて"
             <> "送ってください" ])))
    | arg "key" args /= Just key -> pure (Refuse
        "key が、この目次の下書きと合いません。commit を付けずに送り直して、\
        \新しい key をもらってください")
    | otherwise -> do
        added <- addChecked works w
        pure $ case added of
          Left mistake -> Refuse (T.unlines (explain mistake))
          Right wid    -> Answer ("登録しました：作品 " <> num wid)
  where
    commit = fromMaybe False (arg "commit" args)
    num (WorkId n) = T.pack (show n)
-- ANCHOR_END: decide

-- ANCHOR: schema
-- 📐 The JSON Schema of add_toc's arguments, written by hand. A chapter
--    holds chapters, so it refers to itself ($ref to $defs).
-- add_toc の引数の JSON Schema。手で書いた。章は章を持つので、自分を
-- 指す（$defs への $ref）
tocSchema :: Value
tocSchema = object
  [ "type"       .= ("object" :: Text)
  , "properties" .= object
      [ "title"     .= textP "題"
      , "creator"   .= textP "作者"
      , "last_page" .= intP  "本の最後のページ"
      , "toc"       .= arrayOf chapterRef
      , "commit"    .= boolP "true なら登録する（まず false で下書きを見る）"
      , "key"       .= textP "下書きのときにもらった key"
      ]
  , "required"   .= (["title", "last_page", "toc"] :: [Text])
  , "$defs"      .= object
      [ "chapter" .= object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "heading"  .= textP "見出し"
              , "page"     .= intP  "始まるページ"
              , "children" .= arrayOf chapterRef ]
          , "required"   .= (["heading", "page"] :: [Text]) ] ]
  ]
  where
    chapterRef = object ["$ref" .= ("#/$defs/chapter" :: Text)]

addTocHelp :: Text
addTocHelp = T.unwords
  [ "本の目次を、木の形で登録する。page は各章の始まるページ。"
  , "まず commit なしで送ると、登録される本と key が返る。"
  , "よければ commit: true と key を付けて、もう一度送る。" ]

-- 🧱 Small pieces of JSON Schema.
-- JSON Schema の小さな部品
objectSchema :: [(Key, Value)] -> [Text] -> Value
objectSchema props required = object
  [ "type"       .= ("object" :: Text)
  , "properties" .= object (map (uncurry (.=)) props)
  , "required"   .= required ]

textP, intP, boolP :: Text -> Value
textP = typed "string"
intP  = typed "integer"
boolP = typed "boolean"

typed :: Text -> Text -> Value
typed t what = object ["type" .= t, "description" .= what]

arrayOf :: Value -> Value
arrayOf item = object ["type" .= ("array" :: Text), "items" .= item]
-- ANCHOR_END: schema
-- ANCHOR_END: all
