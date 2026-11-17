-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | SQLite for the runner, with direct-sqlite (the library under
--   sqlite-simple). The open databases are kept in a table, by number;
--   Lam only ever holds the number.
-- 実行役のための SQLite。direct-sqlite（sqlite-simple の下の
-- ライブラリ）を使う。開いたデータベースは番号で表に持ち、Lam が
-- 持つのは番号だけ。
module Lam.Sqlite
  ( Databases
  , realDatabases
  , memoryDatabases
  , sqlRequest
  , closeDatabases
  , runSql
  ) where

import Control.Exception (Handler (..), catches, onException)
import Data.Maybe (fromMaybe)
import System.IO.Error (ioeGetErrorString)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific (Scientific, coefficient, base10Exponent,
                        fromFloatDigits, normalize, toRealFloat, scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite3
import Database.SQLite3.Direct (totalChanges)

import Lam.Builtins (con)
import Lam.Eval
import Lam.Outside (cellValue)
-- ANCHOR_END: head

-- ANCHOR: databases
-- 🗄️ The open databases, and how to open one.
-- 開いているデータベースと、開き方
data Databases = Databases
  { openWith :: Text -> OpenMode -> IO Database
  , closeWith :: Database -> IO ()
  , opened   :: IORef (Map Int Database)
  -- The last number given. A number is never given twice, so a Db
  -- that was closed can never reach another database.
  -- 最後に配った番号。同じ番号は2度配らないので、閉じた Db が、
  -- 別のデータベースに届くことはない
  , lastNumber :: IORef Int
  }

-- 🗄️ The real one: a file on the disk.
-- 本物：ディスクの上のファイル
realDatabases :: IO Databases
realDatabases = do
  table <- newIORef Map.empty
  count <- newIORef 0
  pure Databases
    { openWith = \file mode -> open2 file (flags mode) SQLVFSDefault
    , closeWith = close
    , opened = table
    , lastNumber = count }
  where
    flags ReadWrite = [SQLOpenReadWrite, SQLOpenCreate]
    flags ReadOnly  = [SQLOpenReadOnly]

-- 🎭 For the fake world: every file is a database in memory, one for
--    each name. Opening "weather.db" again gives the same one, so a
--    test can run a script twice and see what the first run saved.
-- 偽物の世界のため：どのファイルも、名前ごとに1つのメモリーの中の
-- データベース。"weather.db" をもう一度開くと同じものなので、テストは
-- スクリプトを2回動かして、1回目が貯めたものを見られる
memoryDatabases :: IO Databases
memoryDatabases = do
  table  <- newIORef Map.empty
  count  <- newIORef 0
  byName <- newIORef Map.empty
  let openByName file _ = do
        known <- Map.lookup file <$> readIORef byName
        case known of
          Just db -> pure db
          Nothing -> do
            db <- open ":memory:"
            modifyIORef' byName (Map.insert file db)
            pure db
  pure Databases
    { openWith = openByName
    , closeWith = \_ -> pure ()          -- keep it, for the next run
    , opened = table
    , lastNumber = count }
-- ANCHOR_END: databases

-- ANCHOR: sqlRequest
-- 🙋 Do one SQL request. Nothing if it is not about SQL.
-- SQL の頼みごとを1つやる。SQL の話でなければ Nothing
sqlRequest :: Databases -> Request -> Maybe (IO Value)
sqlRequest dbs request = case request of
  SqlOpen file mode -> Just $ do
    result <- trying (openWith dbs file mode)
    case result of
      Left e -> pure (failed file e)
      Right db -> do
        n <- atomicModifyIORef' (lastNumber dbs) (\k -> (k + 1, k + 1))
        modifyIORef' (opened dbs) (Map.insert n db)
        pure (con "Ok" [con "Db" [VInt (toInteger n)]])
  SqlQuery n sql cells -> Just $ withDb n $ \db -> do
    result <- runSql db sql cells
    pure $ case result of
      Left e -> failed sql e
      Right (names, rows) ->
        con "Ok" [VList [ VMap (Map.fromList (zip names (map cellValue row)))
                        | row <- rows ]]
  SqlExec n sql cells -> Just $ withDb n $ \db -> do
    -- The rows changed by this statement only. (changes would keep the
    -- count of the last INSERT after a CREATE.)
    -- この文で変わった行だけを数える（changes は、CREATE のあとも、
    -- 前の INSERT の数のまま）
    before <- totalChanges db
    result <- runSql db sql cells
    case result of
      Left e -> pure (failed sql e)
      Right _ -> do
        after <- totalChanges db
        pure (con "Ok" [VInt (toInteger (after - before))])
  SqlClose n -> Just $ do
    found <- Map.lookup n <$> readIORef (opened dbs)
    mapM_ (closeWith dbs) found
    modifyIORef' (opened dbs) (Map.delete n)
    pure (VTuple [])
  _ -> Nothing
  where
    withDb n k = do
      found <- Map.lookup n <$> readIORef (opened dbs)
      case found of
        Just db -> k db
        -- 「閉じたデータベースです」
        Nothing -> pure (con "Err" [VString "Sql: this database is closed"])
    failed what e = con "Err" [VString (what <> ": " <> e)]

-- 🔒 Close every database still open (at the end of a run).
-- まだ開いているデータベースを、全部閉じる（動かし終えたとき）
closeDatabases :: Databases -> IO ()
closeDatabases dbs = do
  table <- readIORef (opened dbs)
  mapM_ (closeWith dbs) table
  writeIORef (opened dbs) Map.empty
-- ANCHOR_END: sqlRequest

-- ANCHOR: runSql
-- 🏃 Run one statement: put the cells into its ?, and read every row.
--    The answer is the names of the columns, and the rows.
-- 文を1つ動かす：? にマスを入れ、行を全部読む。答えは、列の名前と行
runSql :: Database -> Text -> [Cell] -> IO (Either Text ([Text], [[Cell]]))
runSql db sql cells = trying $ do
  stmt <- prepare db sql
  let go acc = do
        result <- step stmt
        case result of
          Done -> pure (reverse acc)
          Row  -> do
            row <- columns stmt
            go (map fromSql row : acc)
  -- If something fails on the way, finalize the statement, and then
  -- let the error go on to try.
  -- 途中で失敗したら、文を片付けてから、エラーを try へ通す
  rows <- (bind stmt (map toSql cells) >> go []) `onException` finalize stmt
  count <- columnCount stmt
  names <- mapM (fmap (fromMaybe "?") . columnName stmt) [0 .. count - 1]
  finalize stmt
  pure (names, rows)

-- 🪤 SQLite's errors, and the few that direct-sqlite gives with fail
--    (a wrong number of ?), as a short text.
-- SQLite のエラーと、direct-sqlite が fail で出すもの（? の数の
-- まちがい）を、短い文にする
trying :: IO a -> IO (Either Text a)
trying act =
  (Right <$> act) `catches`
    [ Handler (\e -> pure (Left (sqlErrorDetails e)))
    , Handler (\e -> pure (Left (T.pack (ioeGetErrorString e)))) ]
-- ANCHOR_END: runSql

-- ANCHOR: convert
-- 🔁 A cell and SQLite's own value, both ways. A REAL is a Double in
--    SQLite, so 17.4 comes back as the shortest decimal that is it.
-- マスと SQLite の値を、行き来する。SQLite の REAL は Double なので、
-- 17.4 は、それを表すいちばん短い小数で戻ってくる
toSql :: Cell -> SQLData
toSql c = case c of
  CNull    -> SQLNull
  CInt i   -> SQLInteger (fromInteger i)   -- cellOf keeps it in 64 bits
  CDec n d -> SQLFloat (toRealFloat (scientific n (negate d)))
  CText s  -> SQLText s

fromSql :: SQLData -> Cell
fromSql d = case d of
  SQLNull      -> CNull
  SQLInteger i -> CInt (toInteger i)
  SQLFloat x
    | isNaN x || isInfinite x -> CText (T.pack (show x))
    | otherwise -> decimal (normalize (fromFloatDigits x))
  SQLText s    -> CText s
  SQLBlob _    -> CText "<blob>"
  where
    decimal :: Scientific -> Cell
    decimal s = CDec (coefficient s) (negate (base10Exponent s))
-- ANCHOR_END: convert
-- ANCHOR_END: all
