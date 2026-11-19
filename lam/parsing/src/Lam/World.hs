-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Two worlds for the runner: the real one (the screen, the keyboard
--   and the disk), and a fake one for the tests (lines and files kept
--   in a Haskell value).
-- 実行役の2つの世界：本物（画面、キーボード、ディスク）と、
-- テスト用の偽物（行とファイルを、Haskell の値に持つ）。
module Lam.World
  ( realWorld
  , Dice
  , newDice
  , Desk
  , newDesk
  , FakeWorld (..)
  , emptyWorld
  , textFiles
  , fakeWorld
  , runFake
  , outputOf
  , errorsOf
  , fakeClock
  ) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Text.Encoding (encodeUtf8)
import System.IO (Handle, IOMode (..), hClose, hIsEOF, openBinaryFile)
import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (getCurrentTime, getCurrentTimeZone, timeZoneMinutes)
import System.Directory (copyFile, createDirectoryIfMissing,
                         doesDirectoryExist, doesPathExist, getFileSize,
                         getModificationTime, listDirectory,
                         removeDirectory, removeFile, renamePath)
import System.Exit (ExitCode (..))
import System.FilePath (joinPath, normalise, splitDirectories)
import qualified System.FilePath.Glob as G
import System.Process (proc, readCreateProcessWithExitCode)
import System.Random (StdGen, initStdGen, mkStdGen, uniformR)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.IO (hFlush, isEOF, stderr, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)

import Lam.Builtins (con, maybeValue)
import Lam.Clock (millisOf)
import Lam.Eval
import Lam.Files (fileInfoValue, outcomeValue, timeValue, zoneValue)
import Lam.Bytes (handleValue, notUtf8)
import Lam.Utf8 (Decoding (..), decodeWith)
-- ANCHOR_END: head

-- ANCHOR: real
-- 🌍 The real world: here, at last, Haskell's IO does the work. It
--    holds the dice, so that Random.int can roll them again and again.
-- 本物の世界：ここで、やっと Haskell の IO が仕事をする。さいころを
-- 持っていて、Random.int のたびに振る
realWorld :: Dice -> Desk -> Request -> IO Value
realWorld dice desk request = case request of
  PutStr s -> do
    TIO.putStr s
    hFlush stdout                -- show "name? " before waiting
    pure unit
  GetLine -> do
    end <- isEOF
    if end
      then pure (con "Nothing" [])
      else do
        line <- TIO.getLine
        pure (con "Just" [VString line])
  -- ANCHOR: realReadFile
  -- Read the bytes, then make them text: a file that is not UTF-8 is
  -- an Err, too (not a crash).
  -- バイト列を読んでから、文字にする。UTF-8 でないファイルも Err
  -- （止まらない）
  ReadFile path -> do
    result <- try (B.readFile (T.unpack path))
    pure $ case result of
      Left e -> resultOf path (Left e)
      Right b -> textOf path b
  -- ANCHOR_END: realReadFile
  WriteFile path text -> do
    result <- try (Utf8.writeFile (T.unpack path) text)
    pure (resultOf path (unit <$ result))
  -- ANCHOR: realArgs
  -- The arguments after the file name: Main puts them with withArgs.
  -- ファイルの名前より後ろの引数。Main が withArgs で置いておく
  GetArgs -> do
    args <- getArgs
    pure (VList (map (VString . T.pack) args))
  LookupEnv key -> do
    found <- lookupEnv (T.unpack key)
    pure (maybeValue (VString . T.pack <$> found))
  GetEnvironment -> do
    pairs <- getEnvironment
    pure (VMap (Map.fromList
      [ (Key (VString (T.pack k)), VString (T.pack v)) | (k, v) <- pairs ]))
  -- ANCHOR_END: realArgs
  -- ANCHOR: realStdio
  GetContents -> VString <$> Utf8.getContents
  PutErr s -> do
    hFlush stdout                -- the screen first, in the right order
    TIO.hPutStr stderr s
    pure unit
  -- ANCHOR_END: realStdio
  -- ANCHOR: realFilesReq
  -- Files and folders, with Haskell's directory. Each failure is an Err.
  -- ファイルとフォルダー。Haskell の directory で。失敗はどれも Err
  ListDir dir -> do
    result <- try (listDirectory (T.unpack dir))
    pure (resultOf dir (strings . sort <$> result))
  Glob pattern -> do
    result <- try (G.glob (T.unpack pattern))
    pure (resultOf pattern (strings . sort . map normalise <$> result))
  Exists path -> VBool <$> doesPathExist (T.unpack path)
  IsDir path  -> VBool <$> doesDirectoryExist (T.unpack path)
  FileInfo path -> do
    result <- try $ do
      size <- getFileSize (T.unpack path)
      time <- getModificationTime (T.unpack path)
      pure (fileInfoValue size (millisOf time))
    pure (resultOf path result)
  MakeDir dir -> done dir (createDirectoryIfMissing True (T.unpack dir))
  Remove path -> done path $ do
    folder <- doesDirectoryExist (T.unpack path)
    (if folder then removeDirectory else removeFile) (T.unpack path)
  Rename from to -> done from (renamePath (T.unpack from) (T.unpack to))
  CopyFile from to -> done from (copyFile (T.unpack from) (T.unpack to))
  AppendFile path text -> done path (Utf8.appendFile (T.unpack path) text)
  -- ANCHOR_END: realFilesReq
  -- ANCHOR: realProcess
  -- A command, with no shell between. A command that is not there
  -- cannot start; one that ends with 1 is still an answer.
  -- シェルを通さずにコマンドを動かす。ないコマンドは始まれない。
  -- 1 で終わったコマンドは、それでも答え
  RunProcess command args input -> do
    result <- try (readCreateProcessWithExitCode
                     (proc (T.unpack command) (map T.unpack args))
                     (T.unpack input))
    pure $ outcomeValue $ case result of
      Left e -> Left (command <> ": " <> whyNot e)
      Right (code, out, err) ->
        Right (codeOf code, T.pack out, T.pack err)
  -- ANCHOR_END: realProcess
  -- ANCHOR: realClock
  Now -> timeValue . millisOf <$> getCurrentTime
  Here -> zoneValue . timeZoneMinutes <$> getCurrentTimeZone
  RandomInt lo hi -> roll dice lo hi
  -- ANCHOR_END: realClock
  -- ANCHOR: realStreams
  -- A real handle of Haskell, kept on the desk under a number. The
  -- file is opened as bytes; each line is made text here.
  -- Haskell の本物のハンドルを、番号を付けて机に置いておく。ファイルは
  -- バイト列として開き、1行ずつ、ここで文字にする
  OpenFile path how -> do
    result <- try (openBinaryFile (T.unpack path) ReadMode)
    case result of
      Left e  -> pure (resultOf path (Left e))
      Right h -> con "Ok" . (: []) . handleValue <$> putOnDesk desk path how h
  ReadLine n -> onDesk desk n $ \open -> do
    end <- hIsEOF (openHandle open)
    if end then pure (con "Ok" [con "Nothing" []]) else do
      line <- B8.hGetLine (openHandle open)
      pure (lineValue open (dropCR line))
  ReadChunk n size -> onDesk desk n $ \open -> do
    piece <- B.hGetSome (openHandle open) size
    pure (con "Ok" [if B.null piece then con "Nothing" []
                    else con "Just" [VBytes piece]])
  CloseFile n -> do
    taken <- atomicModifyIORef' (deskFiles desk) $ \m ->
      (Map.delete n m, Map.lookup n m)
    mapM_ (hClose . openHandle) taken
    pure unit
  -- ANCHOR_END: realStreams
  -- ANCHOR: realBytes
  ReadBytes path -> do
    result <- try (B.readFile (T.unpack path))
    pure (resultOf path (VBytes <$> result))
  WriteBytes path b -> done path (B.writeFile (T.unpack path) b)
  -- ANCHOR_END: realBytes

-- ANCHOR: dice
-- 🎲 The dice of the real world: from a seed (--seed=618, or
--    LAM_SEED=618) if one is given, or else from the system.
-- 本物の世界のさいころ：種（--seed=618 か LAM_SEED=618）があれば
-- そこから、なければシステムから
newtype Dice = Dice (IORef StdGen)

newDice :: Maybe Int -> IO Dice
newDice seed = do
  gen <- maybe initStdGen (pure . mkStdGen) seed
  Dice <$> newIORef gen

-- 🎲 Roll: a number from lo to hi (both in), and the dice move on.
-- 振る：lo から hi まで（両端を含む）の数。さいころは次へ進む
roll :: Dice -> Integer -> Integer -> IO Value
roll (Dice ref) lo hi = atomicModifyIORef' ref $ \g ->
  let (n, g') = uniformR (lo, hi) g in (g', VInt n)
-- ANCHOR_END: dice

-- ANCHOR: desk
-- 🗄️ The desk of the real world: the files open now, under the
--    numbers the world gave, and the next number to give.
-- 本物の世界の机：いま開いているファイルを、世界がくれた番号で。
-- それと、次に渡す番号
data Desk = Desk
  { deskFiles :: IORef (Map Int (Opened Handle))
  , deskNext  :: IORef Int
  }

-- 📖 One open file: its path (for the messages), how to read broken
--    bytes, the lines read so far, and the handle (Haskell's in the
--    real world, the bytes still to be read in the fake one).
-- 開いているファイル1つ：道（知らせの文のため）、壊れたバイトの読み方、
-- ここまでに読んだ行の数、ハンドル（本物の世界では Haskell のもの、
-- 偽物の世界では、まだ読んでいないバイト列）
data Opened h = Opened
  { openPath   :: !Text
  , openHow    :: !Decoding
  , openLines  :: !Int      -- strict: no chain of +1 for each line
  , openHandle :: !h
  }
  deriving (Eq, Show)

newDesk :: IO Desk
newDesk = Desk <$> newIORef Map.empty <*> newIORef 1

putOnDesk :: Desk -> Text -> Decoding -> Handle -> IO Int
putOnDesk desk path how h = do
  n <- atomicModifyIORef' (deskNext desk) (\i -> (i + 1, i))
  atomicModifyIORef' (deskFiles desk) $ \m ->
    (Map.insert n (Opened path how 0 h) m, ())
  pure n

-- 🗄️ Do something with an open file, and count one more line. A
--    closed one is an Err.
-- 開いているファイルで何かをして、読んだ行を1つ数える。閉じたものは Err
onDesk :: Desk -> Int -> (Opened Handle -> IO Value) -> IO Value
onDesk desk n act = do
  files <- readIORef (deskFiles desk)
  case Map.lookup n files of
    Nothing -> pure (closedHandle n)
    Just open -> do
      let counted = open { openLines = openLines open + 1 }
      atomicModifyIORef' (deskFiles desk) (\m -> (Map.insert n counted m, ()))
      act counted
-- ANCHOR_END: desk

-- ANCHOR: lineValue
-- 📜 A line as Ok (Just text), or, for broken bytes read Strict, an
--    Err that says where: "big.log: line 3: not UTF-8 from byte 17".
-- 1行を Ok (Just 文字) に。Strict で読んだ壊れたバイトなら、場所を
-- 言う Err に
lineValue :: Opened h -> ByteString -> Value
lineValue open line = case decodeWith (openHow open) line of
  Right text -> con "Ok" [con "Just" [VString text]]
  Left at    -> con "Err" [VString (openPath open <> ": line "
                  <> T.pack (show (openLines open)) <> ": " <> notUtf8 at)]

-- "a\r" is "a": a file written on Windows has the same lines.
-- Windows で書いたファイルも、同じ行になる
dropCR :: ByteString -> ByteString
dropCR line = if "\r" `B.isSuffixOf` line then B.init line else line

-- 「閉じたハンドル」
closedHandle :: Int -> Value
closedHandle n = con "Err" [VString ("handle " <> T.pack (show n)
                                     <> " is closed")]

-- 📖 The bytes of a whole file as text, or an Err.
-- ファイル全部のバイト列を文字に。だめなら Err
textOf :: Text -> ByteString -> Value
textOf path b = case decodeWith Strict b of
  Right t -> con "Ok" [VString t]
  Left at -> con "Err" [VString (path <> ": " <> notUtf8 at)]
-- ANCHOR_END: lineValue

-- ✅ Ok () when the action did it, or an Err.
-- やれたら Ok ()、だめなら Err
done :: Text -> IO () -> IO Value
done path act = do
  result <- try act
  pure (resultOf path (unit <$ result))

strings :: [FilePath] -> Value
strings = VList . map (VString . T.pack)

codeOf :: ExitCode -> Int
codeOf code = case code of
  ExitSuccess   -> 0
  ExitFailure n -> n

-- 「そういうコマンドはない」
whyNot :: IOException -> Text
whyNot e
  | isDoesNotExistError e = "no such command"
  | isPermissionError e   = "permission denied"
  | otherwise             = T.pack (show e)

-- 📦 Ok v, or Err with a short message for people.
-- Ok v か、人が読む短い文を入れた Err
resultOf :: Text -> Either IOException Value -> Value
resultOf path result = case result of
  Right v -> con "Ok" [v]
  Left e  -> con "Err" [VString (path <> ": " <> reason e)]
  where
    reason e
      | isDoesNotExistError e = "no such file"           -- 「ファイルがない」
      | isPermissionError e   = "permission denied"      -- 「許されていない」
      | otherwise             = T.pack (show e)

unit :: Value
unit = VTuple []
-- ANCHOR_END: real

-- ANCHOR: fake
-- 🎭 A fake world: the lines to type, the files, and the screen.
--    For scripts: the arguments, the environment, and the standard
--    error.
-- 偽物の世界：打ちこむ行、ファイル、画面。
-- スクリプトのため：引数、環境変数、標準エラー
data FakeWorld = FakeWorld
  { worldInput  :: [Text]          -- the lines still to be read
  -- ANCHOR: fakeBytesFiles
  -- The name and the bytes of each file (text is kept as UTF-8, as on
  -- the disk, so a file of the fake world can be broken, too).
  -- ファイルの名前とバイト列（文字はディスクと同じく UTF-8 で持つ。
  -- だから偽物の世界のファイルも、壊れていられる）
  , worldFiles  :: Map Text ByteString
  -- ANCHOR_END: fakeBytesFiles
  , worldOutput :: [Text]          -- what was written, the newest first
  -- ANCHOR: fakeScriptFields
  , worldArgs   :: [Text]          -- the arguments
  , worldEnv    :: Map Text Text   -- the environment variables
  , worldErrors :: [Text]          -- the standard error, the newest first
  -- ANCHOR_END: fakeScriptFields
  -- ANCHOR: fakeFilesFields
  -- Folders (the ones above a file are there, too), the time each file
  -- was changed, the commands that answer, a clock that never moves,
  -- the zone, and the dice (always the same seed).
  -- フォルダー（ファイルの上のフォルダーもある）、ファイルを変えた時刻、
  -- 答えるコマンド、止まった時計、ゾーン、さいころ（いつも同じ種）
  , worldDirs     :: Set Text
  , worldTimes    :: Map Text Integer             -- ms since 1970
  , worldCommands :: Map [Text] (Int, Text, Text) -- code, out, err
  , worldClock    :: Integer                      -- ms since 1970
  , worldZone     :: Int                          -- minutes from UTC
  , worldRandom   :: StdGen
  -- ANCHOR_END: fakeFilesFields
  -- ANCHOR: fakeHandlesField
  -- The files open now: the bytes each one has still to give.
  -- いま開いているファイル：それぞれが、まだ渡していないバイト列
  , worldHandles  :: Map Int (Opened ByteString)
  , worldNextHandle :: Int
  -- ANCHOR_END: fakeHandlesField
  }
  deriving (Eq, Show)

-- ANCHOR: emptyWorld
emptyWorld :: FakeWorld
emptyWorld = FakeWorld
  { worldInput = [], worldFiles = Map.empty, worldOutput = []
  , worldArgs = [], worldEnv = Map.empty, worldErrors = []
  , worldDirs = Set.empty, worldTimes = Map.empty
  , worldCommands = Map.empty, worldClock = fakeClock, worldZone = 540
  , worldRandom = mkStdGen 618
  , worldHandles = Map.empty, worldNextHandle = 1 }

-- 📄 Files of text, as the fake world keeps them: UTF-8 bytes.
-- 文字のファイルを、偽物の世界の持ち方（UTF-8 のバイト列）に
textFiles :: [(Text, Text)] -> Map Text ByteString
textFiles pairs = Map.fromList [ (p, encodeUtf8 t) | (p, t) <- pairs ]

-- ⏰ The fake clock: 2026-11-17 12:00 in Tokyo (03:00 in UTC).
-- 偽物の時計：東京の 2026-11-17 12:00（UTC の 03:00）
fakeClock :: Integer
fakeClock = 1794884400000
-- ANCHOR_END: emptyWorld

-- 🎭 The fake handler: the same requests, the same answers, but every
--    one is only a change to a Haskell value. No disk, no keyboard.
-- 偽物のハンドラー：同じ頼みごとに、同じ形の答え。でも、どれも
-- Haskell の値を書き換えるだけ。ディスクにもキーボードにも触れない
fakeWorld :: Request -> State FakeWorld Value
fakeWorld request = case request of
  PutStr s -> do
    modify' (\w -> w { worldOutput = s : worldOutput w })
    pure unit
  GetLine -> do
    input <- gets worldInput
    case input of
      [] -> pure (con "Nothing" [])
      line : rest -> do
        modify' (\w -> w { worldInput = rest })
        pure (con "Just" [VString line])
  ReadFile path -> do
    found <- gets (Map.lookup (cleanPath path) . worldFiles)
    pure $ case found of
      Just b  -> textOf path b
      Nothing -> con "Err" [VString (path <> ": no such file")]
  WriteFile path text -> do
    modify' (putFile (cleanPath path) (encodeUtf8 text))
    pure (con "Ok" [unit])
  -- ANCHOR: fakeArgs
  GetArgs -> do
    args <- gets worldArgs
    pure (VList (map VString args))
  LookupEnv key -> do
    found <- gets (Map.lookup key . worldEnv)
    pure (maybeValue (VString <$> found))
  GetEnvironment -> do
    env <- gets worldEnv
    pure (VMap (Map.fromList
      [ (Key (VString k), VString v) | (k, v) <- Map.toList env ]))
  -- ANCHOR_END: fakeArgs
  -- ANCHOR: fakeStdio
  -- All the lines still to be read, each with its line break.
  -- まだ読んでいない行を全部。どの行にも改行を付ける
  GetContents -> do
    input <- gets worldInput
    modify' (\w -> w { worldInput = [] })
    pure (VString (T.concat [ line <> "\n" | line <- input ]))
  PutErr s -> do
    modify' (\w -> w { worldErrors = s : worldErrors w })
    pure unit
  -- ANCHOR_END: fakeStdio
  -- ANCHOR: fakeFilesReq
  ListDir dir -> withWorld $ \w -> case childrenOf w (cleanPath dir) of
    Just names -> ok (VList (map VString names))
    Nothing    -> bad dir "no such folder"
  Glob pattern -> withWorld $ \w ->
    let wanted = G.compile (T.unpack (cleanPath pattern))
    in ok (VList [ VString p | p <- Set.toAscList (allPaths w)
                             , G.match wanted (T.unpack p) ])
  Exists path -> withWorld $ \w ->
    VBool (isFile w (cleanPath path) || isFolder w (cleanPath path))
  IsDir path -> withWorld $ \w -> VBool (isFolder w (cleanPath path))
  FileInfo path -> withWorld $ \w ->
    let p = cleanPath path
    in case Map.lookup p (worldFiles w) of
         Just b -> ok (fileInfoValue (toInteger (B.length b)) (timeOf w p))
         Nothing | isFolder w p -> ok (fileInfoValue 0 (worldClock w))
                 | otherwise    -> bad path "no such file"
  MakeDir dir -> change $ \w ->
    let d = cleanPath dir
    in if isFile w d then Left (bad dir "a file is there")
       else Right w { worldDirs = Set.insert d (worldDirs w) }
  Remove path -> change $ \w ->
    let p = cleanPath path
    in if isFile w p then
         Right (keepParents p w { worldFiles = Map.delete p (worldFiles w)
                                , worldTimes = Map.delete p (worldTimes w) })
       else case childrenOf w p of
         Just [] -> Right w { worldDirs = Set.delete p (worldDirs w) }
         Just _  -> Left (bad path "the folder is not empty")
         Nothing -> Left (bad path "no such file")
  Rename from to -> change $ \w ->
    let (f, t) = (cleanPath from, cleanPath to)
        moved k = if k == f then t
                  else maybe k (t <>) (T.stripPrefix f k >>= slashed)
        slashed rest = if "/" `T.isPrefixOf` rest then Just rest else Nothing
        kept = keepParents f w
    in if isFile w f || isFolder w f
         then Right kept
                { worldFiles = Map.mapKeys moved (worldFiles kept)
                , worldTimes = Map.mapKeys moved (worldTimes kept)
                , worldDirs  = Set.map moved (worldDirs kept) }
         else Left (bad from "no such file")
  CopyFile from to -> change $ \w ->
    case Map.lookup (cleanPath from) (worldFiles w) of
      Just text -> Right (putFile (cleanPath to) text w)
      Nothing   -> Left (bad from "no such file")
  AppendFile path text -> change $ \w ->
    let p = cleanPath path
    in Right (putFile p (Map.findWithDefault "" p (worldFiles w)
                           <> encodeUtf8 text) w)
  -- ANCHOR_END: fakeFilesReq
  -- ANCHOR: fakeProcess
  -- A command answers only if the world knows it: no shell, no disk.
  -- コマンドは、世界が知っているものだけが答える。シェルもディスクもない
  RunProcess command args _ -> withWorld $ \w ->
    outcomeValue $ case Map.lookup (command : args) (worldCommands w) of
      Just answer -> Right answer
      Nothing     -> Left (command <> ": no such command")
  -- ANCHOR_END: fakeProcess
  -- ANCHOR: fakeClock
  -- The clock never moves, and the dice start from the same seed: the
  -- same program gives the same answer, every time.
  -- 時計は動かず、さいころはいつも同じ種から。同じプログラムは、
  -- いつも同じ答えを出す
  Now  -> withWorld (timeValue . worldClock)
  Here -> withWorld (zoneValue . worldZone)
  RandomInt lo hi -> do
    (n, g) <- gets (uniformR (lo, hi) . worldRandom)
    modify' (\w -> w { worldRandom = g })
    pure (VInt n)
  -- ANCHOR_END: fakeClock
  -- ANCHOR: fakeStreams
  -- A handle of the fake world is the bytes not yet read: a line cuts
  -- them at the next \n, and a chunk takes some from the front.
  -- 偽物の世界のハンドルは、まだ読んでいないバイト列。1行は次の \n で
  -- 切り、1かたまりは前から何バイトか取る
  OpenFile path how -> do
    found <- gets (Map.lookup (cleanPath path) . worldFiles)
    case found of
      Nothing -> pure (bad path "no such file")
      Just b  -> do
        n <- gets worldNextHandle
        modify' (\w -> w
          { worldHandles = Map.insert n (Opened path how 0 b) (worldHandles w)
          , worldNextHandle = n + 1 })
        pure (ok (handleValue n))
  ReadLine n -> withHandle n $ \open -> case openHandle open of
    rest | B.null rest -> (ok (con "Nothing" []), open)
    rest ->
      let (line, after) = B.break (== 10) rest       -- 10 is \n
          counted = open { openLines = openLines open + 1
                         , openHandle = B.drop 1 after }
      in (lineValue counted (dropCR line), counted)
  ReadChunk n size -> withHandle n $ \open ->
    let (piece, after) = B.splitAt size (openHandle open)
    in ( ok (if B.null piece then con "Nothing" []
             else con "Just" [VBytes piece])
       , open { openHandle = after } )
  CloseFile n -> do
    modify' (\w -> w { worldHandles = Map.delete n (worldHandles w) })
    pure unit
  -- ANCHOR_END: fakeStreams
  ReadBytes path -> withWorld $ \w ->
    maybe (bad path "no such file") (ok . VBytes)
      (Map.lookup (cleanPath path) (worldFiles w))
  WriteBytes path b -> do
    modify' (putFile (cleanPath path) b)
    pure (ok unit)
  where
    withWorld f = gets f
    ok v = con "Ok" [v]
    bad path why = con "Err" [VString (path <> ": " <> why)]
    -- Change the world, or answer an Err and leave it as it was.
    -- 世界を変えるか、Err を答えて、そのままにする
    change f = do
      w <- gets id
      case f w of
        Left err -> pure err
        Right w' -> do
          modify' (const w')
          pure (ok unit)
    timeOf w p = Map.findWithDefault (worldClock w) p (worldTimes w)
    -- Read from an open file, and keep what is left. A closed one is
    -- an Err.
    -- 開いているファイルから読み、残りをしまう。閉じたものは Err
    withHandle n f = do
      found <- gets (Map.lookup n . worldHandles)
      case found of
        Nothing -> pure (closedHandle n)
        Just open -> do
          let (answer, open') = f open
          modify' (\w -> w { worldHandles = Map.insert n open' (worldHandles w) })
          pure answer

-- ANCHOR: fakeFolders
-- 🧹 "./logs/../logs/a.log" is "logs/a.log", and "." is ".". (The
--    fake world has no links, so ".." can simply go up.)
-- 道を、くらべられる形にそろえる（偽物の世界にリンクはないので、".." は
-- そのまま1つ上へ）
cleanPath :: Text -> Text
cleanPath path =
  case reverse (foldl up [] (splitDirectories (normalise (T.unpack path)))) of
    []     -> "."
    pieces -> T.pack (joinPath pieces)
  where
    up acc piece = case (piece, acc) of
      (".", _)           -> acc
      ("..", p : rest) | p /= ".." -> rest
      _                  -> piece : acc

-- 📁 The folders above a path stay, even when the path goes away.
-- 道がなくなっても、その上のフォルダーは残る
keepParents :: Text -> FakeWorld -> FakeWorld
keepParents p w = w { worldDirs = worldDirs w <> Set.fromList
  [ T.intercalate "/" (take n pieces) | n <- [1 .. length pieces - 1] ] }
  where
    pieces = T.splitOn "/" p

-- ✍️ Put a file, changed now.
-- ファイルを置く。変えた時刻は、いま
putFile :: Text -> ByteString -> FakeWorld -> FakeWorld
putFile p text w = w
  { worldFiles = Map.insert p text (worldFiles w)
  , worldTimes = Map.insert p (worldClock w) (worldTimes w) }

isFile :: FakeWorld -> Text -> Bool
isFile w p = Map.member p (worldFiles w)

-- 📁 A folder is there if it was made, or if something is inside it.
-- フォルダーがあるのは、作ったときか、中に何かがあるとき
isFolder :: FakeWorld -> Text -> Bool
isFolder w p = p == "." || Set.member p (worldDirs w) || not (null inside)
  where
    inside = [ k | k <- Set.toList (allPaths w), (p <> "/") `T.isPrefixOf` k ]

-- 📁 The names just inside a folder, in order.
-- フォルダーのすぐ中の名前を、順に
childrenOf :: FakeWorld -> Text -> Maybe [Text]
childrenOf w p
  | isFolder w p = Just (Set.toAscList (Set.fromList
      [ T.takeWhile (/= '/') rest
      | k <- Set.toList (allPaths w)
      , Just rest <- [if p == "." then Just k
                      else T.stripPrefix (p <> "/") k] ]))
  | otherwise = Nothing

-- 🌳 Every file and every folder of the world (the ones above a file
--    too), as paths.
-- 世界のファイルとフォルダー全部（ファイルの上のフォルダーも）
allPaths :: FakeWorld -> Set Text
allPaths w = Set.fromList (concatMap withParents
  (Map.keys (worldFiles w) <> Set.toList (worldDirs w)))
  where
    withParents k =
      let pieces = T.splitOn "/" k
      in [ T.intercalate "/" (take n pieces) | n <- [1 .. length pieces] ]
-- ANCHOR_END: fakeFolders

-- ▶️ Run something in a fake world, and see the world afterwards.
-- 偽物の世界で動かし、そのあとの世界を見る
runFake :: FakeWorld -> State FakeWorld a -> (a, FakeWorld)
runFake world m = runState m world

-- 🖥️ Everything written to the screen, in order.
-- 画面に書いたもの全部を、順に
outputOf :: FakeWorld -> Text
outputOf = T.concat . reverse . worldOutput

-- 🖥️ Everything written to the standard error, in order.
-- 標準エラーに書いたもの全部を、順に
errorsOf :: FakeWorld -> Text
errorsOf = T.concat . reverse . worldErrors
-- ANCHOR_END: fake
-- ANCHOR_END: all
