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
import Control.Monad (mfilter, unless, void)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)

import Tsundoku.Api.Json
import Tsundoku.Form (explain)
import Tsundoku.Import (publisherDef)
import Tsundoku.Mcp.Toc
import Tsundoku.Mcp.Wire
import Tsundoku.Shelf
import Tsundoku.Card (Card (..))
import Tsundoku.Custom (showValue)
import Tsundoku.Labels (eventLabel, stateLabel)
import Tsundoku.Tui.View (workLine)
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
                            (keepPublisher shelf.custom)
-- ANCHOR_END: mode

-- ANCHOR: look
-- 👀 Tools that only look: the list, one work, and search.
-- 見るだけの道具：一覧、作品1つ、検索
lookTools :: Glance -> [Tool]
lookTools g =
  [ Tool "list_works" "本棚の作品の一覧（番号・状態・評価・題）"
         (objectSchema [] [])
         (\_ -> Answer . shelfText <$> g.listCards)
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
    -- an empty shelf says so: "" alone looks like no answer at all
    -- 空の本棚は、そう言う（"" だけだと、答えがないように見える）
    shelfText []    = "本棚に作品がありません"
    shelfText cards = T.unlines (map line cards)
    line (WorkId n, card) =
      T.pack (show n) <> ". " <> workLine (summaryOf (WorkId n, card))
    showWork args = case argOf "id" args of
      Missing -> pure (Refuse "id（作品の番号）がありません")
      Wrong   -> pure (Refuse "id は数で送ってください（\"1\" ではなく 1）")
      Got n   -> do
        found <- g.getCard (WorkId n)
        pure $ case found of
          Nothing   -> Refuse (T.unlines (explain (NoSuchWork (WorkId n))))
          Just card -> Answer (cardText spoilers (detailOf (WorkId n, card))
                                        card)
        where spoilers = fromMaybe False (arg "spoilers" args)
    searchWords args = do
      found <- g.searchText (fromMaybe "" (arg "words" args))
      pure $ case found of
        Left mistake -> Refuse (T.unlines (explain mistake))
        Right hits   -> Answer (T.unlines (map (hitLine . hitItem) hits))
-- ANCHOR_END: look

-- ANCHOR: text
-- 📝 One card as text for the AI. Not the terminal's lines (a bar to
--    look at, keys to press): the AI reads words and cannot press keys.
--    So "name: value" lines whose meaning is plain, with the events by
--    name. Spoilers only when asked.
-- カード1枚を、AI に渡す文字に。ターミナルの画面の行（目で見るバー、
-- 押すキー）は使わない。AI は文字を読み、キーは押せない。だから、意味が
-- はっきりした「名前：値」の行にし、次にできることも言葉で。ネタバレは、
-- 頼まれたときだけ
cardText :: Bool -> CardDetail -> Card -> Text
cardText spoilers c card = T.unlines $
  [ "作品 " <> num c.work.id <> "：" <> c.work.title ]
  ++ [ "作者：" <> c.work.creator | not (T.null c.work.creator) ]
  ++ [ "種類：" <> kindWord c.work.kind
     , "状態：" <> word stateLabel c.work.reading <> nextPart
     , "進み具合：" <> maybe "なし" howFar c.work.progress
     , "評価：" <> maybe "まだ" (\n -> num n <> " 点（100 点満点）") c.work.rating ]
  ++ [ "属性：" <> listed (Map.map showValue card.attrs)
     | not (Map.null card.attrs) ]
  ++ [ "自分で決めた状態：" <> listed card.states | not (Map.null card.states) ]
  ++ [ "目次（✓ は読み終えた項目）：" | not (null c.toc) ]
  ++ concatMap (entryLines 1) c.toc
  ++ [ "感想：" | not (null c.notes) ]
  ++ [ "  ・" <> shown n | n <- reverse c.notes ]
  where
    num :: Int -> Text
    num = T.pack . show
    word label name = maybe name label (fromName name)
    listed m = T.intercalate "、" [ k <> "＝" <> v | (k, v) <- Map.toList m ]
    nextPart
      | null c.next = ""
      | otherwise   = "（次にできること："
                      <> T.intercalate "・" (map (word eventLabel) c.next) <> "）"
    howFar p = num p.done <> "／" <> num p.total <> unitWord p.unit
               <> "（" <> num p.percent <> "%）"
    entryLines depth e =
      (T.replicate depth "  " <> "・" <> e.heading <> if e.done then " ✓" else "")
        : concatMap (entryLines (depth + 1)) e.children
    shown n
      | n.spoiler && not spoilers = "（ネタバレなので伏せています）"
      | n.spoiler                 = "（ネタバレ）" <> n.body
      | otherwise                 = n.body

-- 🏷️ The words of the API ("book", "pages", ...) in Japanese.
-- API の言葉（"book"・"pages" など）を日本語に
kindWord, unitWord :: Text -> Text
kindWord k = case k of
  "book"   -> "本"
  "serial" -> "連載"
  "series" -> "シリーズ（ドラマ・アニメ）"
  "film"   -> "映画"
  "game"   -> "ゲーム"
  other    -> other
unitWord u = case u of
  "pages"    -> "ページ"
  "episodes" -> "話"
  "minutes"  -> "分"
  other      -> other

-- 🔎 One hit as a line: where, and the snippet with 【】.
-- ヒット1つを1行に：どこか、と【】つきの抜き出し
hitLine :: HitItem -> Text
hitLine h = T.pack (show h.work) <> ". "
         <> T.intercalate " > " (h.title : h.trail) <> "："
         <> snippet
  where
    -- a spoiler is hidden here too, as in show_work: the AI would read
    -- it out in the chat
    -- ネタバレは、show_work と同じく、ここでも伏せる（AI がチャットで
    -- 読み上げてしまう）
    snippet
      | h.spoiler = "（ネタバレの感想に当たりました。伏せています）"
      | otherwise = foldMap piece h.snippet
    piece s = if s.match then "【" <> s.text <> "】" else s.text
-- ANCHOR_END: text

-- ANCHOR: edit
-- ✍️ Tools that write: add a table of contents (first a draft, then
--    commit), and add a note. They get only what they need: two windows,
--    and one function that writes the publisher (not the custom window).
-- 書く道具：目次を足す（まず下書き、それから commit）、感想を足す。
-- もらうのは、要るものだけ：窓口2つと、出版社を書く関数1つ（属性の窓口
-- そのものは、もらわない）
editTools :: IO UTCTime -> WorkWindow -> ReadingWindow
          -> (WorkId -> Text -> IO (Checked ())) -> [Tool]
editTools clock works reading keep =
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
      Left why    -> pure (Refuse (T.unlines
        [ "目次の形が読めません：" <> T.pack why
        , "page は数で（引用符なしで）送ってください。ページの書いていない"
          <> "見出し（部の扉など）にも、その下の最初の章と同じページを"
          <> "入れてください" ]))
      Right draft -> do
        shelf <- works.listCards
        let same = [ wid | (wid, c) <- shelf
                         , c.work.title   == draft.title
                         , c.work.creator == draft.creator ]
        decideToc works keep args same (planToc shelf draft)
    addNote args = case (arg "id" args, arg "body" args) of
      _ | Wrong <- (argOf "id" args :: Arg Int) ->
            pure (Refuse "id は数で送ってください（\"1\" ではなく 1）")
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
decideToc :: WorkWindow -> (WorkId -> Text -> IO (Checked ())) -> Object
          -> [WorkId] -> Plan -> IO Reply
decideToc works keep args same plan = case plan of
  _ | Wrong <- commitArg -> pure (Refuse
        "commit は true か false で送ってください（引用符なしで）")
  Problems why -> pure (Refuse (T.unlines ("目次を直してください：" : why)))
  Already wid  -> pure (Answer ("もう登録してあります：作品 " <> num wid
                               <> "。二重には登録しません"))
  Ready w key
    | not commit -> pure (Answer (T.unlines (
        "下書きです。まだ登録していません。" : preview w
        ++ [ "出版社：" <> p | Just p <- [publisher] ]
        ++ [ "同じ題と作者の作品 " <> T.intercalate "・" (map num same)
             <> " が、もう本棚にあります（目次がちがいます）。"
             <> "直したいなら、画面の「直す」で直してください"
           | not (null same) ]
        ++ [ "key: " <> key
           , "よければ、同じ引数に commit: true と、この key を付けて"
             <> "送ってください" ])))
    | arg "key" args /= Just key -> pure (Refuse
        "key が、この目次の下書きと合いません。commit を付けずに送り直して、\
        \新しい key をもらってください")
    | otherwise -> do
        added <- addChecked works w
        case added of
          Left mistake -> pure (Refuse (T.unlines (explain mistake)))
          Right wid    -> do
            kept <- maybe (pure (Right ())) (keep wid) publisher
            pure (Answer (T.intercalate "\n" (
              ("登録しました：作品 " <> num wid)
              : either (\m -> "出版社は書けませんでした：" : explain m)
                       (const []) kept)))
  where
    commitArg = argOf "commit" args :: Arg Bool
    commit    = case commitArg of
      Got c -> c
      _     -> False
    publisher = mfilter (not . T.null) (arg "publisher" args)
    num (WorkId n) = T.pack (show n)

-- 🏢 Write the publisher in the attribute 「出版社」, defining the
--    attribute first if the shelf does not have it yet.
-- 出版社を属性「出版社」に書く。本棚にまだその属性がなければ、先に決める
keepPublisher :: CustomWindow -> WorkId -> Text -> IO (Checked ())
keepPublisher custom wid name = do
  schema <- custom.getSchema
  unless (attrName publisherDef `elem` map attrName (attrDefs schema))
    (void (custom.defineAttr publisherDef))
  custom.editAttrs wid (Map.singleton (attrName publisherDef) name)
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
      , "creator"   .= textP "作者（人の名前。出版社は publisher に）"
      , "publisher" .= textP "出版社（わかれば）"
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
              [ "heading"  .= object
                  [ "type"        .= ("string" :: Text)
                  , "minLength"   .= (1 :: Int)
                  , "description" .= ("見出し" :: Text) ]
              , "page"     .= object
                  [ "type"        .= ("integer" :: Text)
                  , "minimum"     .= (1 :: Int)
                  , "description" .= ("始まるページ" :: Text) ]
              , "children" .= arrayOf chapterRef ]
          , "required"   .= (["heading", "page"] :: [Text]) ] ]
  ]
  where
    chapterRef = object ["$ref" .= ("#/$defs/chapter" :: Text)]

addTocHelp :: Text
addTocHelp = T.unwords
  [ "本の目次を、木の形で登録する。page は各章の始まるページ。"
  , "出版社がわかれば publisher に書く（作者の欄には入れない）。"
  , "まず commit なしで送ると、登録される本と key が返る。"
  , "その下書きを、そのまま人に見せて、よいと言われてから、"
  , "commit: true と key を付けて、もう一度送る。" ]

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
