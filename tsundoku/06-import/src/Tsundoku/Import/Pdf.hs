-- ANCHOR: all
-- ANCHOR: header
-- | Read a PDF with poppler's tools: pdfinfo (the title, the author,
--   how many pages) and pdftohtml -xml (the bookmarks, with the page
--   each one starts on). Reading a PDF by ourselves would be a big job,
--   so we ask the tools, and read what they write.
-- poppler の道具で PDF を読む：pdfinfo（題・作者・ページ数）と、
-- pdftohtml -xml（しおりと、それぞれが始まるページ）。PDF を自分で読むの
-- は大仕事なので、道具に頼んで、道具が書いたものを読む
module Tsundoku.Import.Pdf
  ( Poppler (..)
  , poppler
  , readPdf
  , readPdfWith
  , runTool
  , infoFields
  , outlineOf
  , pdfDraft
  ) where

-- exceptions, running a program, and reading what it writes
-- 例外、プログラムを動かす、書いたものを読む
import Control.Exception (displayException, try)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Tree (Forest, Tree (..))
import System.Exit (ExitCode (..))
import System.IO.Error (isDoesNotExistError)
import System.Process.Typed (proc, readProcess)
import Text.Read (readMaybe)

-- the XML that pdftohtml writes
-- pdftohtml が書く XML
import Text.XML (Element (..), Name (..), Node (..), def, parseLBS)
import Text.XML.Cursor
  (Cursor, content, fromDocument, laxAttribute, laxElement, node, ($/),
   ($//))

import Tsundoku.Import.Types
import Tsundoku.Types (Pages (..))
-- ANCHOR_END: header

-- ANCHOR: runTool
-- 🧰 Which programs to run. Usually the ones on the PATH.
-- どのプログラムを動かすか。ふつうは PATH にあるもの
data Poppler = Poppler
  { infoTool :: FilePath
  , xmlTool  :: FilePath
  }

poppler :: Poppler
poppler = Poppler { infoTool = "pdfinfo", xmlTool = "pdftohtml" }

-- 🏃 Run a program, and take what it writes to the standard output.
--    Not installed, or it failed: say which, as a value.
-- プログラムを動かして、標準出力に書いたものを受け取る。入っていない、
-- 失敗した、のどちらかなら、それを値で返す
runTool :: FilePath -> [String] -> IO (Either ImportProblem LBS.ByteString)
runTool tool args = do
  result <- try (readProcess (proc tool args))
  pure $ case result of
    Left err
      | isDoesNotExistError err   -> Left (NoTool tool)
      | otherwise                 -> Left (ToolFailed tool (-1) (shown err))
    Right (ExitSuccess,   out, _) -> Right out
    Right (ExitFailure n, _, err) -> Left (ToolFailed tool n (utf8 err))
  where
    shown = T.pack . displayException
-- ANCHOR_END: runTool

-- ANCHOR: readPdf
-- 📕 Read a PDF: ask pdfinfo, then pdftohtml, then put the answers
--    together (that part is pure: pdfDraft).
-- PDF を読む：pdfinfo に聞き、pdftohtml に聞き、答えを組み立てる
-- （組み立ては純粋：pdfDraft）
readPdf :: FilePath -> IO (Either ImportProblem (Imported Pages))
readPdf = readPdfWith poppler

-- 🔧 The same, with the tools you choose. ExceptT stops at the first
--    problem: if pdfinfo is not there, pdftohtml is never run.
-- 同じことを、選んだ道具で。ExceptT は、最初の問題で止まる：pdfinfo が
-- なければ、pdftohtml は動かさない
readPdfWith :: Poppler -> FilePath
            -> IO (Either ImportProblem (Imported Pages))
readPdfWith tools path = runExceptT $ do
  info    <- ExceptT (runTool (infoTool tools) ["-enc", "UTF-8", path])
  xml     <- ExceptT (runTool (xmlTool tools)
                        ["-xml", "-i", "-stdout", "-q", path])
  outline <- except (outlineOf xml)
  let fields = infoFields (utf8 info)
      field  = (`Map.lookup` fields)
  total   <- case readMaybe . T.unpack =<< field "Pages" of
    Just n  -> pure n
    Nothing -> throwE (Unexpected "pdfinfo が Pages を書いていない")
  pure (pdfDraft (field "Title") (field "Author") total outline)
-- ANCHOR_END: readPdf

-- ANCHOR: parse
-- 🗒️ pdfinfo writes "Title:          Alice's ...": a name, a colon,
--    spaces, and the value.
-- pdfinfo は「Title:   Alice's ...」と書く：名前、コロン、空白、値
infoFields :: Text -> Map Text Text
infoFields text = Map.fromList
  [ (name, T.strip value)
  | line <- T.lines text
  , let (name, rest) = T.breakOn ":" line
  , Just value <- [T.stripPrefix ":" rest]
  , not (T.null (T.strip value)) ]

-- 🔖 The bookmarks in pdftohtml's XML: in <outline>, each <item> may be
--    followed by an <outline> of its children.
--    <outline><item page="2">I. ...</item><outline>...</outline></outline>
-- pdftohtml の XML のしおり。<outline> の中で、<item> の次に <outline>
-- が来たら、それがその item の子
outlineOf :: LBS.ByteString -> Either ImportProblem (Forest (Text, Int))
outlineOf bytes =
  case parseLBS def bytes of
    Left err  -> Left (BrokenXml "pdftohtml -xml"
                                 (T.pack (displayException err)))
    Right doc -> Right (concatMap level
                                  (fromDocument doc $/ laxElement "outline"))
  where
    level :: Cursor -> Forest (Text, Int)
    level outline = items (outline $/ anyElement)

    items (item : rest)
      | named "item" item =
          let (kids, after) = case rest of
                next : more | named "outline" next -> (level next, more)
                _                                  -> ([], rest)
          in  case pageOf item of
                Just p  -> Node (tidyText item, p) kids : items after
                -- no page: keep its children / ページがない：子は残す
                Nothing -> kids ++ items after
      | otherwise = items rest
    items [] = []

    anyElement c = [ c | NodeElement _ <- [node c] ]
    named n c    = case node c of
      NodeElement e -> nameLocalName (elementName e) == n
      _             -> False
    pageOf item  = case laxAttribute "page" item of
      [p] -> readMaybe (T.unpack p)
      _   -> Nothing
    tidyText c   = T.unwords (T.words (T.concat (c $// content)))
-- ANCHOR_END: parse

-- ANCHOR: pdfDraft
-- 📏 Put the answers together. Each bookmark's pages come from where
--    it starts (fromStarts). No bookmarks: one heading for the whole
--    book, and say so in `missed`.
-- 答えを組み立てる。しおりごとのページ数は、始まるページから出す
-- （fromStarts）。しおりがなければ、本全体で1つの見出しにして、そのことを
-- missed に書く
pdfDraft :: Maybe Text -> Maybe Text -> Int -> Forest (Text, Int)
         -> Imported Pages
pdfDraft title author total outline
  | null outline = Imported (book [Node (whole, Pages total) []]) [NoOutline]
  | otherwise    = Imported (book (fromStarts total outline))
                            (outOfOrder outline)
  where
    book  = Draft title author
    whole = maybe "本文" id title
-- ANCHOR_END: pdfDraft

utf8 :: LBS.ByteString -> Text
utf8 = decodeUtf8With lenientDecode . LBS.toStrict
-- ANCHOR_END: all
