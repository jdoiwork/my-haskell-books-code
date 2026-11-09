-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🚪 A tiny way in from the command line. It knows the window and the
--    views; it does not know where the cards are kept.
-- コマンドからの小さな入り口。窓口と見せ方は知っている。
-- カードがどこにしまわれているかは、知らない
module Main (main) where

import Control.Monad (forM_, unless, void)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs, lookupEnv)
import System.Exit (die, exitFailure)
import System.FilePath (takeBaseName)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Text.Read (readMaybe)

-- HTTPS for openBD and the なろう小説API
-- openBD となろう小説API のための HTTPS
import Network.HTTP.Client.TLS (newTlsManager)

-- the clock, and the HTTP server for the API
-- 時計と、API のための HTTP サーバー
import Data.Time (getCurrentTime)
import Network.Wai.Handler.Warp qualified as Warp

import Tsundoku.Html.Server (siteApp)
import Tsundoku.Bar (cardSummary, showAmount)
import Tsundoku.Card (Card (..))
import Tsundoku.Examples
import Tsundoku.Form (explain)
import Tsundoku.Import
import Tsundoku.Import.Narou
import Tsundoku.Import.OpenBd
import Tsundoku.Import.Sample
import Tsundoku.Import.Types
import Tsundoku.Isbn
import Tsundoku.Labels (stateLabel)
import Tsundoku.Mcp.Tools (Mode (..), shelfTools)
import Tsundoku.Mcp.Wire (serveStdio)
import Tsundoku.Outline (outline)
import Tsundoku.Rating
import Tsundoku.Review (Month (..), MonthStat (..), monthly, tagCloud, toCsv,
                        toMarkdown)
import Tsundoku.Search
import Tsundoku.Shelf
import Tsundoku.Shelf.Config
import Tsundoku.Types
-- ANCHOR_END: imports

-- ANCHOR: main
-- 🚪 TSUNDOKU_DB chooses the store (SQLite's tsundoku.db if unset).
-- TSUNDOKU_DB でしまう場所を選ぶ（なければ SQLite の tsundoku.db）
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  setting <- fromMaybe "sqlite:tsundoku.db" <$> lookupEnv "TSUNDOKU_DB"
  config  <- either die pure (parseStoreConfig setting)
  args    <- getArgs
  case args of
    ["samples"] -> writeSamples
    _           -> withShelf config $ \shelf -> case args of
      ["add-examples"]   -> addExamples shelf
      ["list"]           -> listShelf shelf
      ["find", typed]    -> findCards shelf (T.pack typed)
      ["import", path]   -> importInto shelf path
      ["isbn", typed]    -> isbnInto shelf (T.pack typed)
      ["updates"]        -> checkUpdates shelf
      "search" : typed   -> searchShelf shelf (T.pack (unwords typed))
      -- ANCHOR: serveArgs
      ["serve"]          -> serveShelf shelf 8080
      ["serve", port]    -> maybe (die usage) (serveShelf shelf) (readMaybe port)
      -- ANCHOR_END: serveArgs
      -- ANCHOR: mcpArgs
      "mcp" : opts       -> maybe (die usage) (`serveMcp` shelf) (mcpMode opts)
      -- ANCHOR_END: mcpArgs
      -- ANCHOR: exportArgs
      ["export", "md"]   -> shelf.works.listCards >>= T.putStr . toMarkdown
      ["export", "csv"]  -> shelf.works.listCards >>= T.putStr . toCsv
      ["stats"]          -> showStats shelf
      -- ANCHOR_END: exportArgs
      _                  -> die usage
  where
    usage = unlines
      [ "usage: shelf add-examples | shelf list | shelf find SEARCH"
      , "       shelf samples | shelf import FILE | shelf isbn ISBN"
      , "       shelf updates | shelf search WORDS..."
      , "       shelf serve [PORT] | shelf mcp [--read-only]"
      , "       shelf export md|csv | shelf stats" ]

-- 📚 Put five works on the shelf, through the window only: first what
--    the library defined, then the works and what I wrote on them.
-- 作品を5つ、本棚に置く。窓口だけを通して：まず図書室が決めたもの、
-- それから作品と、そこに書いたもの
addExamples :: Shelf -> IO ()
addExamples shelf = do
  mapM_ shelf.custom.defineAttr (libraryAttrs ++ [ncodeDef, publisherDef])
  mapM_ shelf.custom.defineFlow libraryFlows
  dragon <- shelf.works.addWork dragonBook
  void (shelf.reading.applyEvent dragon Start)
  forM_ (notes dragonCard) (shelf.reading.addNote dragon)
  forM_ (parse FiveStars "★4.5") (shelf.reading.rate dragon)
  void (shelf.custom.editAttrs dragon dragonForm)
  void (shelf.custom.moveFlow dragon "貸し出し" "本棚にある")
  serial <- shelf.works.addWork librarianSerial
  void (shelf.reading.applyEvent serial Start)
  forM_ (parse FiveStars "★5") (shelf.reading.rate serial)
  void (shelf.custom.editAttrs serial librarianForm)
  void (shelf.custom.moveFlow serial "連載の状態" "連載中")
  void (shelf.custom.editAttrs serial (Map.singleton "Nコード" "n0000xx"))
  copyist <- shelf.works.addWork copyistSerial
  void (shelf.reading.applyEvent copyist Start)
  forM_ (parse FiveStars "★4") (shelf.reading.rate copyist)
  void (shelf.custom.editAttrs copyist copyistForm)
  void (shelf.custom.moveFlow copyist "連載の状態" "連載中")
  void (shelf.custom.moveFlow copyist "連載の状態" "休止")
  void (shelf.custom.editAttrs copyist (Map.singleton "Nコード" "n0000yy"))
  film <- shelf.works.addWork lastDragonFilm
  void (shelf.reading.applyEvent film Start)
  void (shelf.reading.applyEvent film Finish)
  void (shelf.works.addWork dungeonGame)
  putStrLn "added 5 works"

-- 📋 One line for each card.
-- カード1枚に1行
listShelf :: Shelf -> IO ()
listShelf shelf = do
  cards <- shelf.works.listCards
  mapM_ (T.putStrLn . cardLine) cards

-- 🔎 The cards a search finds, or why it could not search.
-- 絞り込みで見つかったカード。絞り込めなければ、その理由
findCards :: Shelf -> Text -> IO ()
findCards shelf typed = do
  found <- shelf.custom.findCards typed
  case found of
    Left why    -> refuse why
    Right cards -> mapM_ (T.putStrLn . cardLine) cards

-- 🛑 Say why the core said no, and stop with exit code 1.
-- 核が断った理由を言って、終了コード 1 で止まる
refuse :: Mistake -> IO a
refuse why = do
  mapM_ (T.hPutStrLn stderr) (explain why)
  exitFailure

-- 📋 A card in one line: number, state, rating, and title.
-- カードを1行に：番号、状態、評価、題
cardLine :: (WorkId, Card) -> Text
cardLine (WorkId n, card) = T.intercalate "  "
  [ T.pack (show n) <> "."
  , stateLabel (reading card)
  , maybe "  -  " (render FiveStars) (rating card)
  , title (work card)
  ]
-- ANCHOR_END: main

-- ANCHOR: import
-- 📚 Write the sample books into this folder, to try `import` on.
-- 見本の本を、このフォルダーに書き出す（import を試すため）
writeSamples :: IO ()
writeSamples = do
  LBS.writeFile "dragon.epub"  sampleEpub
  LBS.writeFile "dragon2.epub" sampleEpub2
  LBS.writeFile "alice.pdf"    samplePdf
  LBS.writeFile "plain.pdf"    (pdfBook False)
  putStrLn "wrote dragon.epub, dragon2.epub, alice.pdf, plain.pdf"

-- 📥 Take in an EPUB or a PDF, and put it on the shelf. What we could
--    not take in goes to the standard error; it does not stop us.
-- EPUB か PDF を取り込んで、本棚に置く。取り込めなかったことは標準
-- エラーへ。それで止まりはしない
importInto :: Shelf -> FilePath -> IO ()
importInto shelf path = do
  result <- importFile path
  got    <- either stop pure result
  mapM_ (T.hPutStrLn stderr . explainImport) (missed got)
  wid    <- shelveBook shelf (T.pack (takeBaseName path)) (draft got)
  added shelf wid

-- 🔢 Look up an ISBN on openBD, and put the book on the shelf.
--    TSUNDOKU_OPENBD can point somewhere else (to try without the
--    network, for example).
-- ISBN を openBD で調べて、本を本棚に置く。TSUNDOKU_OPENBD で、聞く先を
-- 変えられる（ネットなしで試すときなど）
isbnInto :: Shelf -> Text -> IO ()
isbnInto shelf typed = do
  asked   <- either (stop . BadIsbn) pure (parseIsbn typed)
  base    <- fromMaybe openBd <$> lookupEnv "TSUNDOKU_OPENBD"
  manager <- newTlsManager
  book    <- either stop pure =<< fetchBook manager base asked
  let got = bookDraft book
  mapM_ (T.hPutStrLn stderr . explainImport) (missed got)
  wid     <- shelf.works.addWork (bookWork (isbnText asked) (draft got))
  added shelf wid
  keepPublisher shelf wid (publisher book)

-- 🔔 Ask the なろう小説API about every serial with an Nコード, in one
--    go, and grow the tables of the ones with new episodes.
-- Nコードのある連載の全部を、なろう小説API に1回で聞き、新しい話が出た
-- ものの目次を育てる
checkUpdates :: Shelf -> IO ()
checkUpdates shelf = do
  serials <- narouSerials <$> shelf.works.listCards
  base    <- fromMaybe narou <$> lookupEnv "TSUNDOKU_NAROU"
  manager <- newTlsManager
  found   <- either stop pure
               =<< fetchNovels manager base [ code | (_, code, _) <- serials ]
  forM_ (updatesFor found serials) $ \(wid, answer) -> case answer of
    Left why          -> T.hPutStrLn stderr (explainImport why)
    Right (novel, 0)  ->
      T.putStrLn (novelTitle novel <> "：新しい話はありません")
    Right (novel, n)  -> do
      grown <- shelf.intake.addEpisodes wid n
      case grown of
        Left why -> mapM_ (T.hPutStrLn stderr) (explain why)
        Right () -> T.putStrLn (novelTitle novel <> "：新しく "
                                <> T.pack (show n) <> "話")

-- 📗 Show a work just put on the shelf: its line and its table.
-- 本棚に置いたばかりの作品を見せる：その1行と目次
added :: Shelf -> WorkId -> IO ()
added shelf wid = do
  found <- shelf.works.getCard wid
  forM_ found $ \card -> do
    T.putStrLn ("added " <> cardLine (wid, card))
    forM_ (cardSummary 10 card) T.putStrLn
    case contents (work card) of
      Book toc -> forM_ (outline toc) $ \(depth, entry) ->
        T.putStrLn (T.replicate (2 * depth + 2) " " <> heading entry
                    <> "  " <> showAmount (size entry))
      _        -> pure ()

-- 🏢 Keep the publisher openBD told us, in the attribute 「出版社」. If
--    the shelf does not have that attribute yet, define it first.
-- openBD が教えてくれた出版社を、属性「出版社」に書いておく。本棚に、
-- まだその属性がなければ、先に決める
keepPublisher :: Shelf -> WorkId -> Text -> IO ()
keepPublisher shelf wid name
  | T.null name = pure ()
  | otherwise   = do
      schema <- shelf.custom.getSchema
      let known = map attrName (attrDefs schema)
      unless (attrName publisherDef `elem` known)
        (void (shelf.custom.defineAttr publisherDef))
      kept <- shelf.custom.editAttrs wid (Map.singleton (attrName publisherDef) name)
      case kept of
        Left why -> mapM_ (T.hPutStrLn stderr) (explain why)
        Right () -> T.putStrLn ("  出版社：" <> name)

-- 🛑 Say what went wrong, and stop with exit code 1.
-- 何がまずかったかを言って、終了コード 1 で止まる
stop :: ImportProblem -> IO a
stop why = do
  T.hPutStrLn stderr (explainImport why)
  exitFailure
-- ANCHOR_END: import

-- ANCHOR: search
-- 🔎 What a search found, two lines for each hit.
-- 探して見つかったもの。1件を2行で
searchShelf :: Shelf -> Text -> IO ()
searchShelf shelf typed = do
  found <- shelf.inside.searchText typed
  case found of
    Left why   -> refuse why
    Right []   -> T.putStrLn "見つかりませんでした"
    Right hits -> mapM_ (mapM_ T.putStrLn . hitLines) hits

-- 🔦 A hit: the work (its number and title), the trail down to the
--    entry, and the place; then the snippet, with the words in 【】.
-- ヒット1件：作品（番号と題）と、項目までの道順と、場所。それから
-- 抜き出し。探した語は【】で囲む
hitLines :: Hit -> [Text]
hitLines hit =
  [ number <> ". " <> T.intercalate " > " (hitTitle hit : trail hit)
      <> "（" <> placeLabel (place hit) <> "）"
  , "  " <> T.concat (map showPiece (snippet hit))
  ]
  where
    WorkId n = hitWork hit
    number   = T.pack (show n)
    showPiece (Plain t) = t
    showPiece (Match t) = "【" <> t <> "】"

-- 🏷️ How the command line names a place.
-- コマンドの画面での、場所の呼び方
placeLabel :: Place -> Text
placeLabel found = case found of
  InTitle        -> "題"
  InCreator      -> "作者"
  InHeading _    -> "見出し"
  InNote _ False -> "感想"
  InNote _ True  -> "感想・ネタバレ"
  InText _       -> "本文"
-- ANCHOR_END: search

-- ANCHOR: mcp
-- 🤖 Serve the shelf to an AI over MCP (JSON-RPC on stdin and stdout).
--    With --read-only, the AI gets only the tools that look.
-- 本棚を MCP で AI に出す（標準入力と標準出力の JSON-RPC）。
-- --read-only なら、AI がもらうのは見る道具だけ
serveMcp :: Mode -> Shelf -> IO ()
serveMcp mode shelf =
  serveStdio "tsundoku" (shelfTools mode getCurrentTime shelf)

-- 🔐 The mode from the options: nothing, or --read-only.
-- オプションからモードを：なにもなしか、--read-only
mcpMode :: [String] -> Maybe Mode
mcpMode opts = case opts of
  []              -> Just ReadWrite
  ["--read-only"] -> Just ReadOnly
  _               -> Nothing
-- ANCHOR_END: mcp

-- ANCHOR: stats
-- 📊 The statistics in the terminal: month by month, and the tags.
-- 端末で統計を：月ごとと、タグ
showStats :: Shelf -> IO ()
showStats shelf = do
  cards <- map snd <$> shelf.works.listCards
  forM_ (Map.toList (monthly cards)) $ \(Month y m, MonthStat n (Pages p)) ->
    T.putStrLn (T.pack (show y <> "-" <> show m) <> "  " <> T.pack (show n)
                <> "冊  " <> T.pack (show p) <> "ページ")
  T.putStrLn (T.unwords [ "#" <> t <> "(" <> T.pack (show k) <> ")"
                        | (t, k) <- tagCloud cards ])
-- ANCHOR_END: stats

-- ANCHOR: serve
-- 🌐 Serve the shelf until stopped (Ctrl-C): the pages at /, the JSON
--    API at /api. Only this machine may come in (127.0.0.1).
--    TSUNDOKU_RATING chooses how ratings look (stars if unset).
-- 本棚を出す。止めるまで（Ctrl-C）：ページは / に、JSON の API は /api に。
-- 入ってこられるのは、このマシンだけ（127.0.0.1）。
-- TSUNDOKU_RATING で評価の見せ方を選ぶ（なければ ★）
serveShelf :: Shelf -> Int -> IO ()
serveShelf shelf port = do
  setting <- fromMaybe "stars" <$> lookupEnv "TSUNDOKU_RATING"
  style   <- maybe (die ("unknown TSUNDOKU_RATING: " <> setting)) pure
                   (lookup setting ratingStyles)
  putStrLn ("serving the shelf on http://localhost:" <> show port)
  Warp.runSettings settings (siteApp style getCurrentTime shelf)
  where
    settings = Warp.setHost "127.0.0.1"
                 (Warp.setPort port Warp.defaultSettings)

-- ⭐ The names of the rating styles, for TSUNDOKU_RATING.
-- 評価の見せ方の名前（TSUNDOKU_RATING のため）
ratingStyles :: [(String, RatingStyle)]
ratingStyles =
  [ ("hundred", Hundred)
  , ("stars",   FiveStars)
  , ("ten",     OutOfTen NoDecimals)
  , ("ten.1",   OutOfTen OneDecimal) ]
-- ANCHOR_END: serve

-- ANCHOR_END: all
