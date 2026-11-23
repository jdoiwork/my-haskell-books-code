-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | The builtins that reach outside: HTTP and SQLite. Like the other
--   IO builtins, they only make requests; the runner does them.
-- 外に手をのばす組み込みの関数：HTTP と SQLite。ほかの IO の組み込みと
-- 同じく、頼みごとを作るだけ。やるのは実行役
module Lam.Outside
  ( outsideBuiltins
  , responseValue
  , cellOf
  , cellValue
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Scientific (scientific)
import Data.Text (Text)

import Lam.Builtins
import Lam.Eval
import Lam.Json (fromScientific)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
outsideBuiltins :: [Builtin]
outsideBuiltins =
  -- ANCHOR: httpRow
  -- Http.send "GET" url {} "": one request, one Response.
  -- Err only when no answer came (no network, a wrong host).
  -- 頼みごと1つで、Response 1つ。Err は答えが来なかったときだけ
  [ prim4 "Http.send"
      (tString ~> tString ~> tMap tString tString ~> tString
         ~> tIO (tResult tString tResponse)) $ \m u hs b -> do
      method  <- string m
      url     <- string u
      headers <- mapOf hs >>= traverse string
      body    <- string b
      Right (ask (HttpSend method url
                   [ (k, v) | (Key (VString k), v) <- Map.toList headers ]
                   body))
  -- ANCHOR_END: httpRow
  -- ANCHOR: sqlRows
  -- Sql.open "weather.db": a Db, to give to query and exec.
  -- Sql.open で Db をもらい、query と exec に渡す
  , prim1 "Sql.open" (tString ~> tIO (tResult tString tDb)) $ \v ->
      ask . (`SqlOpen` ReadWrite) <$> string v
  , prim1 "Sql.openReadOnly" (tString ~> tIO (tResult tString tDb)) $ \v ->
      ask . (`SqlOpen` ReadOnly) <$> string v
  -- The rows of a SELECT: each one is a Map from the column's name.
  -- SELECT の行：どれも、列の名前から引く Map
  , prim3 "Sql.query"
      (tDb ~> tString ~> tList tJson
         ~> tIO (tResult tString (tList (tMap tString tJson)))) $
      withCells SqlQuery
  -- CREATE, INSERT, UPDATE, DELETE: the answer is how many rows changed.
  -- CREATE・INSERT・UPDATE・DELETE：答えは、変わった行の数
  , prim3 "Sql.exec"
      (tDb ~> tString ~> tList tJson ~> tIO (tResult tString TInt)) $
      withCells SqlExec
  , prim1 "Sql.close" (tDb ~> tIO tUnit) $ \db ->
      ask . SqlClose <$> dbNumber db
  ]
  -- ANCHOR_END: sqlRows
  where
    ask = VIO . Ask
    tResult e x = TCon "Result" [e, x]
    tDb = TCon "Db" []
    tJson = TCon "Json" []
    -- ANCHOR: responseType
    -- An answer is a record: the status (200, 404, ...), the headers
    -- (their names in small letters), and the body, as bytes.
    -- 答えはレコード：状態コード（200、404 など）、ヘッダー（名前は
    -- 小文字）、本文（バイト列）
    tResponse = TRecord (Map.fromList
      [ ("status", TInt)
      , ("headers", tMap tString tString)
      , ("body", TCon "Bytes" []) ]) Nothing
    -- ANCHOR_END: responseType
-- ANCHOR_END: table

-- 🧱 The record of an answer, as the runner makes it.
-- 答えのレコード。実行役が作る
responseValue :: Int -> [(Text, Text)] -> ByteString -> Value
responseValue status headers body = VRecord (Map.fromList
  [ ("status", VInt (toInteger status))
  , ("headers", VMap (Map.fromList
      [ (Key (VString k), VString v) | (k, v) <- headers ]))
  , ("body", VBytes body) ])

-- ANCHOR: withCells
-- 🧱 The SQL and its arguments, as a request. A Json that cannot be a
--    cell (an array, an object) is an Err, before anything is asked.
-- SQL と引数を、頼みごとにする。マスにならない Json（配列、
-- オブジェクト）は、何も頼まずに Err にする
withCells
  :: (Int -> Text -> [Cell] -> Request)
  -> Value -> Value -> Value -> Either EvalError Value
withCells request db s args = do
  n   <- dbNumber db
  sql <- string s
  js  <- list args
  Right $ case traverse cellOf js of
    Right cells -> VIO (Ask (request n sql cells))
    Left why    -> VIO (Pure (con "Err" [VString why]))

-- 🔢 The number inside a Db (the constructor Db is hidden in Lam).
-- Db の中の番号（Lam では、構成子 Db は隠してある）
dbNumber :: Value -> Either EvalError Int
dbNumber v = case v of
  VCon "Db" _ [VInt n] -> Right (fromInteger n)
  _                    -> Left (BuiltinFailed "Sql" "needs a Db")
-- ANCHOR_END: withCells

-- ANCHOR: cells
-- 🧱 A Lam Json, as a cell. true and false are 1 and 0 (SQLite has no
--    booleans).
-- Lam の Json を、マスに。true・false は 1・0（SQLite に真偽値はない）
cellOf :: Value -> Either Text Cell
cellOf v = case v of
  VCon "JNull" _ []               -> Right CNull
  VCon "JBool" _ [VBool b]        -> Right (CInt (if b then 1 else 0))
  -- SQLite keeps an integer in 64 bits: a larger one is an Err, not
  -- a number that wraps around.
  -- SQLite の整数は 64 ビット。それより大きい数は、輪を回った数ではなく Err
  VCon "JInt"  _ [VInt i]
    | i < toInteger (minBound :: Int64) || i > toInteger (maxBound :: Int64)
                                  -> Left "Sql: the integer is too large"
    | otherwise                   -> Right (CInt i)
  VCon "JDec"  _ [VInt c, VInt d] -> Right (CDec c (fromInteger d))
  VCon "JStr"  _ [VString s]      -> Right (CText s)
  -- 「配列とオブジェクトは入れられない。Json.show で文字にして」
  _ -> Left "Sql: an array or an object cannot be a cell; use Json.show"

-- 🧱 A cell, back as a Lam Json. A decimal is made the same way as
--    Json.parse makes it: 25.0 is JInt 25, 17.40 is JDec 174 1.
-- マスを、Lam の Json に戻す。小数は Json.parse と同じ形にする：
-- 25.0 は JInt 25、17.40 は JDec 174 1
cellValue :: Cell -> Value
cellValue c = case c of
  CNull     -> con "JNull" []
  CInt i    -> con "JInt" [VInt i]
  CDec n d  -> fromScientific (scientific n (negate d))
  CText s   -> con "JStr" [VString s]
-- ANCHOR_END: cells
-- ANCHOR_END: all
