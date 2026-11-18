-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | The tools of scripts: paths (pure), files and folders, commands,
--   the clock, and dice. The pure ones compute here; the others only
--   make requests, and the runner does them.
-- スクリプトの道具：道（純粋）、ファイルとフォルダー、コマンド、時計、
-- さいころ。純粋なものはここで計算する。ほかは頼みごとを作るだけで、
-- やるのは実行役。
module Lam.Files
  ( filesBuiltins
  , timeValue
  , zoneValue
  , millisOf
  , fileInfoValue
  , outcomeValue
  , globRoot
  , Files (..)
  , noFiles
  ) where

import Data.Time (Day, UTCTime (..), ZonedTime, LocalTime (..),
                  TimeOfDay (..), toGregorian, utcToLocalTime,
                  minutesToTimeZone, zonedTimeToUTC, formatTime,
                  defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (joinPath, splitDirectories, takeBaseName,
                        takeDirectory, takeExtension, takeFileName,
                        replaceExtension, (</>))

import Lam.Builtins
import Lam.Eval
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
filesBuiltins :: [Builtin]
filesBuiltins =
  pathBuiltins <> fileBuiltins <> processBuiltins <> timeBuiltins
    <> randomBuiltins
-- ANCHOR_END: table

-- ANCHOR: path
-- 🛤️ Path: only text, no disk. Path.join "logs" "a.log" is
--    "logs/a.log", made by Haskell's filepath.
-- Path：文字だけで、ディスクには触れない。中身は Haskell の filepath
pathBuiltins :: [Builtin]
pathBuiltins =
  [ prim2 "Path.join" (tString ~> tString ~> tString) $ \a b ->
      onPaths (\x y -> T.pack (T.unpack x </> T.unpack y)) a b
  , onePath "Path.basename"  takeFileName      -- "logs/a.log" -> "a.log"
  , onePath "Path.dirname"   takeDirectory     -- "logs/a.log" -> "logs"
  , onePath "Path.extension" takeExtension     -- "logs/a.log" -> ".log"
  , onePath "Path.stem"      takeBaseName      -- "logs/a.log" -> "a"
  , prim2 "Path.withExtension" (tString ~> tString ~> tString) $ \e p ->
      onPaths (\x y -> T.pack (replaceExtension (T.unpack y) (T.unpack x)))
        e p
  ]
  where
    onePath n f = prim1 n (tString ~> tString) $ \v ->
      VString . T.pack . f . T.unpack <$> string v
    onPaths f a b = VString <$> (f <$> string a <*> string b)
-- ANCHOR_END: path

-- ANCHOR: file
-- 📂 File: each one is a request. A failure is a value (Err).
-- File：どれも頼みごと。失敗は値（Err）で返す
fileBuiltins :: [Builtin]
fileBuiltins =
  [ ask1 "File.list" (tResult tString (tList tString)) ListDir
  , ask1 "File.glob" (tResult tString (tList tString)) Glob
  , ask1 "File.exists" TBool Exists
  , ask1 "File.isDir" TBool IsDir
  , ask1 "File.info" (tResult tString tFileInfo) FileInfo
  , ask1 "File.makeDir" (tResult tString tUnit) MakeDir
  , ask1 "File.remove" (tResult tString tUnit) Remove
  , ask2 "File.rename" (tResult tString tUnit) Rename
  , ask2 "File.copy" (tResult tString tUnit) CopyFile
  , ask2 "File.append" (tResult tString tUnit) AppendFile
  ]
  where
    -- ANCHOR: tFileInfo
    -- type alias FileInfo = { size : Int, modified : Time }: the type
    -- itself, since an alias is only a name.
    -- 別名は名前だけなので、型そのものを書く
    tFileInfo = TRecord (Map.fromList [ ("size", TInt)
                                      , ("modified", TCon "Time" []) ])
                        Nothing
    -- ANCHOR_END: tFileInfo
    ask1 n t request = prim1 n (tString ~> tIO t) $ \v ->
      VIO . Ask . request <$> string v
    ask2 n t request = prim2 n (tString ~> tString ~> tIO t) $ \a b ->
      VIO . Ask <$> (request <$> string a <*> string b)
-- ANCHOR_END: file

-- ANCHOR: process
-- 🏭 Process.pipe "sort" ["-u"] text: run a command with text for its
--    standard input. Process.run (in Lam) gives it "".
-- コマンドを、標準入力に文字を渡して動かす。Process.run（Lam で書く）は
-- "" を渡す
processBuiltins :: [Builtin]
processBuiltins =
  [ prim3 "Process.pipe"
      (tString ~> tList tString ~> tString ~> tIO tOutcome) $ \c as i -> do
      command <- string c
      args    <- list as >>= traverse string
      input   <- string i
      Right (VIO (Ask (RunProcess command args input)))
  ]
  where
    tOutcome = TCon "Outcome" []
-- ANCHOR_END: process

-- ANCHOR: time
-- ⏰ Time: now and here are requests; the rest only computes.
-- Time：now と here は頼みごと。ほかは計算するだけ
timeBuiltins :: [Builtin]
timeBuiltins =
  [ Builtin "Time.now" (closeOver (tIO tTime)) (VIO (Ask Now))
  , Builtin "Time.here" (closeOver (tIO tZone)) (VIO (Ask Here))
  -- "2026-11-17T03:04:05Z": always in UTC.
  -- いつも UTC で
  , prim1 "Time.toIso" (tTime ~> tString) $ \t ->
      VString . isoOf <$> millis t
  , prim1 "Time.fromIso" (tString ~> tResult tString tTime) $ \v -> do
      text <- string v
      Right $ case readIso (T.unpack text) of
        Just utc -> con "Ok" [timeValue (millisOf utc)]
        -- 「ISO 8601 の時刻ではない」
        Nothing  -> con "Err" [VString ("not an ISO 8601 time: " <> text)]
  -- "2026-11-17 12:04:05 +09:00": in the zone given.
  -- 渡したゾーンで
  , inZone "Time.format" tString $ \local offset ->
      VString (T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" local)
                 <> " " <> offsetText offset)
  , inZone "Time.toDate" tString $ \local _ ->
      VString (T.pack (formatTime defaultTimeLocale "%Y-%m-%d" local))
  , part "Time.toYear"   (\(y, _, _) _ -> y)
  , part "Time.toMonth"  (\(_, m, _) _ -> toInteger m)
  , part "Time.toDay"    (\(_, _, d) _ -> toInteger d)
  , part "Time.toHour"   (\_ tod -> toInteger (todHour tod))
  , part "Time.toMinute" (\_ tod -> toInteger (todMin tod))
  , part "Time.toSecond" (\_ tod -> floor (todSec tod))
  ]
  where
    tTime = TCon "Time" []
    tZone = TCon "Zone" []
    -- Turn the Time into the clock on the wall of the zone.
    -- Time を、そのゾーンの壁の時計に直す
    inZone n t f = prim2 n (tZone ~> tTime ~> t) $ \z v -> do
      offset <- minutes z
      ms     <- millis v
      let local = utcToLocalTime (minutesToTimeZone offset) (utcOf ms)
      Right (f local offset)
    part n f = inZone n TInt $ \local _ ->
      VInt (f (toGregorian (localDay local)) (localTimeOfDay local))
    offsetText m =
      let sign = if m < 0 then "-" else "+"
          (h, mm) = abs m `divMod` 60
      in sign <> two h <> ":" <> two mm
    two n = T.justifyRight 2 '0' (T.pack (show n))

-- 🕰️ The ISO 8601 text of a time, in UTC, with the milliseconds only
--    when there are some.
-- 時刻の ISO 8601 の文字。UTC で。ミリ秒は、あるときだけ
isoOf :: Integer -> Text
isoOf ms =
  let utc = utcOf ms
      main = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" utc
      rest = ms `mod` 1000
  in T.pack main
       <> (if rest == 0 then ""
           else "." <> T.justifyRight 3 '0' (T.pack (show rest)))
       <> "Z"

-- 📖 Read "2026-11-17T03:04:05Z", "2026-11-17T12:04:05+09:00", or a
--    day alone ("2026-11-17", the start of the day in UTC).
-- 3つの形を読む。日だけなら、UTC のその日の始まり
readIso :: String -> Maybe UTCTime
readIso s = case iso8601ParseM s :: Maybe UTCTime of
  Just t  -> Just t
  Nothing -> case iso8601ParseM s :: Maybe ZonedTime of
    Just z  -> Just (zonedTimeToUTC z)
    Nothing -> (\d -> UTCTime d 0) <$> (iso8601ParseM s :: Maybe Day)
-- ANCHOR_END: time

-- ANCHOR: random
-- 🎲 Random.int 1 6: a request, because it gives a new answer each
--    time (pick and shuffle are Lam, made of it).
-- 頼みごと。呼ぶたびに答えが変わるから（pick と shuffle は、これで
-- 作った Lam）
randomBuiltins :: [Builtin]
randomBuiltins =
  [ prim2 "Random.int" (TInt ~> TInt ~> tIO TInt) $ \a b -> case (a, b) of
      (VInt lo, VInt hi) -> Right (VIO (Ask (RandomInt lo hi)))
      _ -> Left (BuiltinFailed "Random.int" "needs two Ints")
  ]
-- ANCHOR_END: random

-- ANCHOR: values
-- 🧱 Lam values made from Haskell ones, and back.
-- Haskell の値から作る Lam の値と、その逆
timeValue :: Integer -> Value
timeValue ms = con "Time" [VInt ms]

zoneValue :: Int -> Value
zoneValue m = con "Zone" [VInt (toInteger m)]

-- ANCHOR: recordValues
-- 🧱 { size = ..., modified = ... }
fileInfoValue :: Integer -> Integer -> Value
fileInfoValue size ms =
  VRecord (Map.fromList [("size", VInt size), ("modified", timeValue ms)])

-- 🧱 How a command ended: the exit code, the output, the errors.
--    Left: it could not start, and why. The fields have names now:
--    Failed { code = 1, stdout = "", stderr = "..." }.
-- コマンドの終わり方：終了コード、出力、エラー。Left は始まれなかった
-- わけ。いまはフィールドに名前がある
outcomeValue :: Either Text (Int, Text, Text) -> Value
outcomeValue r = case r of
  Left why              -> con "CannotStart" [VString why]
  Right (0, out, err)   -> con "Succeeded" [outputs out err []]
  Right (code, out, err) ->
    con "Failed" [outputs out err [("code", VInt (toInteger code))]]
  where
    outputs out err more = VRecord (Map.fromList
      ([("stdout", VString out), ("stderr", VString err)] <> more))
-- ANCHOR_END: recordValues

-- ⏱️ The milliseconds since 1970 of a UTC time (cut down to the ms).
-- 1970年からのミリ秒（ミリ秒より細かいところは切り捨て）
millisOf :: UTCTime -> Integer
millisOf t = floor (utcTimeToPOSIXSeconds t * 1000)

utcOf :: Integer -> UTCTime
utcOf ms = posixSecondsToUTCTime (fromInteger ms / 1000)

millis :: Value -> Either EvalError Integer
millis v = case v of
  VCon "Time" _ [VInt ms] -> Right ms
  _ -> Left (BuiltinFailed "Time" "needs a Time")

minutes :: Value -> Either EvalError Int
minutes v = case v of
  VCon "Zone" _ [VInt m] -> Right (fromInteger m)
  _ -> Left (BuiltinFailed "Time" "needs a Zone")
-- ANCHOR_END: values

-- ANCHOR: globRoot
-- 🌳 The folder a glob pattern looks in: the pieces before the first
--    one with * ? or [. "logs/2026-*/*.log" looks in "logs". It is
--    what --allow-read must cover.
-- glob の型が探すフォルダー：* ? [ のある最初のかけらより前。
-- --allow-read が及ばなければならないのは、ここ
globRoot :: Text -> Text
globRoot pattern =
  let fixed = takeWhile (not . any (`elem` ("*?[" :: String)))
                (splitDirectories (T.unpack pattern))
  in if null fixed then "." else T.pack (joinPath fixed)
-- ANCHOR_END: globRoot

tResult :: Type -> Type -> Type
tResult e x = TCon "Result" [e, x]

-- ANCHOR: files
-- 📂 How to read the files of modules: the real disk, or the fake
--    world's files in the tests. searchPath is the folders of LAM_PATH.
-- モジュールのファイルの読み方：本物のディスクか、テストなら偽物の世界
-- のファイル。searchPath は LAM_PATH のフォルダー
data Files m = Files
  { readSource :: FilePath -> m (Maybe Text)
  , searchPath :: [FilePath]
  }

-- 🚫 No files at all (a program without imports of its own).
-- ファイルは1つもない（自分の import がないプログラム）
noFiles :: Applicative m => Files m
noFiles = Files (const (pure Nothing)) []
-- ANCHOR_END: files
-- ANCHOR_END: all
