{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: module
-- 🏰 The guild's vault: a ledger of gold, kept safe from accidents.
-- ギルドの宝物庫：金貨の帳簿を、事故から守る
module Vault where

-- Exceptions, and telling IO accidents apart.
-- 例外と、IO の事故の見分け方
import Control.Exception
import System.IO.Error (isDoesNotExistError)

-- Files.
-- ファイル
import System.Directory (removeFile, renameFile)
import System.IO

-- Text.
-- テキスト
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Read (readMaybe)
-- ANCHOR_END: module

-- ANCHOR: entry
-- 💰 One line of the ledger: who keeps how much gold.
-- 帳簿の1行：誰が、金貨をいくら預けているか
data Entry = Entry
  { owner :: Text
  , gold  :: Int
  }
  deriving (Show, Eq)

-- 📖 "Lambda,120" → Just (Entry "Lambda" 120). Broken lines: Nothing.
-- "Lambda,120" → Just (Entry "Lambda" 120)。壊れた行は Nothing
parseEntry :: Text -> Maybe Entry
parseEntry line =
  case T.splitOn "," line of
    [who, amount] ->
      case readMaybe (T.unpack (T.strip amount)) of
        Just n  -> Just (Entry (T.strip who) n)
        Nothing -> Nothing
    _ -> Nothing

-- 📚 The whole ledger, line by line. A broken line: Left that line.
--    (We do not drop it quietly: a dropped line is lost gold.)
-- 帳簿の全体を、1行ずつ読む。壊れた行があれば、その行を Left で返す
-- （黙って捨てない。捨てた行の金貨は、消えてしまう）
parseLedger :: Text -> Either Text [Entry]
parseLedger text =
  case [ line | line <- rows, isNothing (parseEntry line) ] of
    (broken : _) -> Left broken
    []           -> Right (mapMaybe parseEntry rows)
  where
    rows = T.lines text

-- 📒 Text → entries, in IO. A broken line becomes an accident.
-- テキストから帳簿へ（IO の中で）。壊れた行は、事故にする
entriesOf :: Text -> IO [Entry]
entriesOf text =
  case parseLedger text of
    Right entries -> pure entries
    Left line     -> throwIO (BrokenLine line)

-- ✍️ Entry "Lambda" 120 → "Lambda,120"
renderEntry :: Entry -> Text
renderEntry e = e.owner <> "," <> T.pack (show e.gold)
-- ANCHOR_END: entry

-- ANCHOR: careless
-- 🙈 Read the ledger. Could not read it? Start with an empty one.
--    (Careless: "no permission" also becomes an empty ledger.)
-- 帳簿を読む。読めなかったら、空の帳簿から始める
-- （うっかり版：「読む権限がない」まで、空の帳簿になってしまう）
loadLedgerCareless :: FilePath -> IO [Entry]
loadLedgerCareless path = do
  result <- try @IOException (TIO.readFile path)
  case result of
    Left _     -> pure []
    Right text -> entriesOf text
-- ANCHOR_END: careless

-- ANCHOR: load
-- 📂 Read the ledger. No file yet? Start with an empty ledger.
--    Any other accident (no permission, ...) goes up, unhidden.
-- 帳簿を読む。まだファイルがなければ、空の帳簿から始める
-- それ以外の事故（読む権限がない、など）は、隠さずに上へ伝える
loadLedger :: FilePath -> IO [Entry]
loadLedger path = do
  result <- tryJust missing (TIO.readFile path)
  case result of
    Left ()    -> pure []
    Right text -> entriesOf text
  where
    -- Pick out only "the file does not exist".
    -- 「ファイルがない」だけを選び出す
    missing :: IOException -> Maybe ()
    missing e
      | isDoesNotExistError e = Just ()
      | otherwise             = Nothing
-- ANCHOR_END: load

-- ANCHOR: accident
-- 🚨 Accidents in the vault.
-- 宝物庫の事故
data VaultAccident
  -- Goblins in the hall: drop everything and run.
  -- 広間にゴブリン。手を止めて駆けつける
  = GoblinAlarm
  -- The vault door does not move.
  -- 宝物庫の扉が動かない
  | DoorJammed
  -- Someone keeps less than zero gold: the books are broken.
  -- 金貨がマイナスの人がいる。帳簿が壊れている
  | NegativeGold Text Int
  -- A line of the ledger we cannot read.
  -- 帳簿に、読めない行がある
  | BrokenLine Text
  deriving Show

instance Exception VaultAccident
-- ANCHOR_END: accident

-- ANCHOR: catches
-- 🧯 Run one task of the shift. Known accidents are written down,
--    and the shift goes on.
-- 当番の仕事を1つ動かす。知っている事故は書き留めて、当番を続ける
runTask :: Text -> IO () -> IO ()
runTask name task =
  task `catches`
    [ Handler (\ (e :: IOException)   -> note (T.pack (displayException e)))
    , Handler (\ (e :: VaultAccident) -> note (T.pack (show e)))
    ]
  where
    note message = TIO.hPutStrLn stderr (name <> ": " <> message)
-- ANCHOR_END: catches

-- ANCHOR: journal
-- 📓 Open the journal, hand it to the work, and always close it.
-- 日誌を開いて仕事に渡し、何があっても閉じる
withJournal :: FilePath -> (Handle -> IO a) -> IO a
withJournal path work =
  bracket (openFile path AppendMode) hClose work
-- ANCHOR_END: journal

-- ANCHOR: write
-- ✍️ Write the entries, one line each. A negative gold stops it.
-- 1人1行で書く。マイナスの金貨があったら、そこで止める
writeEntries :: Handle -> [Entry] -> IO ()
writeEntries h entries = mapM_ writeOne entries
  where
    writeOne e
      | e.gold < 0 = throwIO (NegativeGold e.owner e.gold)
      | otherwise  = TIO.hPutStrLn h (renderEntry e)

-- 🙈 Write straight over the ledger. (Careless: a stop halfway
--    leaves a ledger cut in half.)
-- 帳簿に直接、上書きする（うっかり版：途中で止まると、
-- 半分だけの帳簿が残る）
saveLedgerCareless :: FilePath -> [Entry] -> IO ()
saveLedgerCareless path entries =
  withFile path WriteMode (\ h -> writeEntries h entries)
-- ANCHOR_END: write

-- ANCHOR: save
-- 💾 Write a draft first, then swap it in. If anything goes wrong
--    (swapping included), throw the draft away: the old ledger stays
--    as it was.
-- まず下書きに書いて、できたら差し替える。途中で何かあったら
-- （差し替えも含めて）、下書きを捨てる。前の帳簿は、そのまま残る
saveLedger :: FilePath -> [Entry] -> IO ()
saveLedger path entries =
  writeAndSwap `onException` throwDraftAway
  where
    draft = path <> ".draft"

    -- Write everything to the draft, then give it the ledger's name.
    -- 下書きに全部書いてから、帳簿の名前に付け替える
    writeAndSwap = do
      withFile draft WriteMode (\ h -> writeEntries h entries)
      renameFile draft path

    -- Throw the draft away, if it is there. If even that fails, keep
    --    quiet: the first accident is the one to report.
    -- 下書きがあれば捨てる。それすら失敗しても黙っておく。
    -- 知らせるべきなのは、最初の事故のほう
    throwDraftAway = do
      _ <- try @IOException (removeFile draft)
      pure ()
-- ANCHOR_END: save
-- ANCHOR_END: all
