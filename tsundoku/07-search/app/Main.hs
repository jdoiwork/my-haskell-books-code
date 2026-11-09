-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🚪 A tiny way in from the command line. It knows the window and the
--    views; it does not know where the cards are kept.
-- コマンドからの小さな入り口。窓口と見せ方は知っている。
-- カードがどこにしまわれているかは、知らない
module Main (main) where

import Control.Monad (forM_, void)
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

-- HTTPS for openBD and the なろう小説API
-- openBD となろう小説API のための HTTPS
import Network.HTTP.Client.TLS (newTlsManager)

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
import Tsundoku.Outline (outline)
import Tsundoku.Rating
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
      ["add-examples"] -> addExamples shelf
      ["list"]         -> listShelf shelf
      ["find", typed]  -> findCards shelf (T.pack typed)
      ["import", path] -> importInto shelf path
      ["isbn", typed]  -> isbnInto shelf (T.pack typed)
      ["updates"]      -> checkUpdates shelf
      "search" : typed -> searchShelf shelf (T.pack (unwords typed))
      _                -> die usage
  where
    usage = unlines
      [ "usage: shelf add-examples | shelf list | shelf find SEARCH"
      , "       shelf samples | shelf import FILE | shelf isbn ISBN"
      , "       shelf updates | shelf search WORDS..." ]

-- 📚 Put five works on the shelf, through the window only: first what
--    the library defined, then the works and what I wrote on them.
-- 作品を5つ、本棚に置く。窓口だけを通して：まず図書室が決めたもの、
-- それから作品と、そこに書いたもの
addExamples :: Shelf -> IO ()
addExamples shelf = do
  mapM_ shelf.defineAttr (libraryAttrs ++ [ncodeDef])
  mapM_ shelf.defineFlow libraryFlows
  dragon <- shelf.addWork dragonBook
  void (shelf.applyEvent dragon Start)
  forM_ (notes dragonCard) (shelf.addNote dragon)
  forM_ (parse FiveStars "★4.5") (shelf.rate dragon)
  void (shelf.editAttrs dragon dragonForm)
  void (shelf.moveFlow dragon "貸し出し" "本棚にある")
  serial <- shelf.addWork librarianSerial
  void (shelf.applyEvent serial Start)
  forM_ (parse FiveStars "★5") (shelf.rate serial)
  void (shelf.editAttrs serial librarianForm)
  void (shelf.moveFlow serial "連載の状態" "連載中")
  void (shelf.editAttrs serial (Map.singleton "Nコード" "n0000xx"))
  copyist <- shelf.addWork copyistSerial
  void (shelf.applyEvent copyist Start)
  forM_ (parse FiveStars "★4") (shelf.rate copyist)
  void (shelf.editAttrs copyist copyistForm)
  void (shelf.moveFlow copyist "連載の状態" "連載中")
  void (shelf.moveFlow copyist "連載の状態" "休止")
  void (shelf.editAttrs copyist (Map.singleton "Nコード" "n0000yy"))
  film <- shelf.addWork lastDragonFilm
  void (shelf.applyEvent film Start)
  void (shelf.applyEvent film Finish)
  _ <- shelf.addWork dungeonGame
  putStrLn "added 5 works"

-- 📋 One line for each card.
-- カード1枚に1行
listShelf :: Shelf -> IO ()
listShelf shelf = do
  cards <- shelf.listCards
  mapM_ (T.putStrLn . cardLine) cards

-- 🔎 The cards a search finds, or why it could not search.
-- 絞り込みで見つかったカード。絞り込めなければ、その理由
findCards :: Shelf -> Text -> IO ()
findCards shelf typed = do
  found <- shelf.findCards typed
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
  wid     <- shelf.addWork (bookWork (isbnText asked) (draft got))
  added shelf wid

-- 🔔 Ask the なろう小説API about every serial with an Nコード, in one
--    go, and grow the tables of the ones with new episodes.
-- Nコードのある連載の全部を、なろう小説API に1回で聞き、新しい話が出た
-- ものの目次を育てる
checkUpdates :: Shelf -> IO ()
checkUpdates shelf = do
  serials <- narouSerials <$> shelf.listCards
  base    <- fromMaybe narou <$> lookupEnv "TSUNDOKU_NAROU"
  manager <- newTlsManager
  found   <- either stop pure
               =<< fetchNovels manager base [ code | (_, code, _) <- serials ]
  forM_ (updatesFor found serials) $ \(wid, answer) -> case answer of
    Left why          -> T.hPutStrLn stderr (explainImport why)
    Right (novel, 0)  ->
      T.putStrLn (novelTitle novel <> "：新しい話はありません")
    Right (novel, n)  -> do
      grown <- shelf.addEpisodes wid n
      case grown of
        Left why -> mapM_ (T.hPutStrLn stderr) (explain why)
        Right () -> T.putStrLn (novelTitle novel <> "：新しく "
                                <> T.pack (show n) <> "話")

-- 📗 Show a work just put on the shelf: its line and its table.
-- 本棚に置いたばかりの作品を見せる：その1行と目次
added :: Shelf -> WorkId -> IO ()
added shelf wid = do
  found <- shelf.getCard wid
  forM_ found $ \card -> do
    T.putStrLn ("added " <> cardLine (wid, card))
    forM_ (cardSummary 10 card) T.putStrLn
    case contents (work card) of
      Book toc -> forM_ (outline toc) $ \(depth, entry) ->
        T.putStrLn (T.replicate (2 * depth + 2) " " <> heading entry
                    <> "  " <> showAmount (size entry))
      _        -> pure ()

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
  found <- shelf.searchText typed
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

-- ANCHOR: label
-- 🏷️ How the command line shows a reading state.
-- コマンドの画面での、読書の状態の見せ方
stateLabel :: ReadingState -> Text
stateLabel state = case state of
  Piled     -> "積読"
  Reading   -> "読書中"
  Finished  -> "読了"
  Paused    -> "中断"
  Rereading -> "再読"
-- ANCHOR_END: label
-- ANCHOR_END: all
