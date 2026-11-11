-- ANCHOR: all
-- | SELECT, UPDATE and DELETE, built from typed pieces.
-- 型の付いた部品から組み立てる SELECT・UPDATE・DELETE
module MiniSql.Query
  ( Table (..)
  , Select
  , Order (..)
  , selectFrom
  , where_
  , orderBy
  , limit
  , compileSelect
  , runSelect
  , Assign
  , (.=)
  , compileUpdate
  , runUpdate
  , compileDelete
  , runDelete
  ) where

-- ANCHOR: imports
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple

import MiniSql.Column
import MiniSql.Expr
-- ANCHOR_END: imports

-- ANCHOR: select
-- 🗃️ A table: its name, and its columns in the order of the record.
-- 表：名前と、レコードと同じ順の列
data Table t = Table { tableName :: Text, tableColumns :: [Text] }

data Order = Asc | Desc

-- 📋 A SELECT, still being built.
-- 組み立て中の SELECT
data Select t = Select
  { selTable  :: Table t
  , selWhere  :: [Expr t Bool]
  , selOrder  :: [(Text, Order)]
  , selLimit  :: Maybe Int
  }

selectFrom :: Table t -> Select t
selectFrom table = Select table [] [] Nothing

where_ :: Expr t Bool -> Select t -> Select t
where_ cond s = s { selWhere = selWhere s ++ [cond] }

orderBy :: Col t a -> Order -> Select t -> Select t
orderBy c dir s = s { selOrder = selOrder s ++ [(colName c, dir)] }

-- At most n rows. A negative n would mean "no limit" to SQLite, so it
-- becomes 0.
-- n 行まで。負の数は SQLite では「上限なし」になってしまうので、0 にする
limit :: Int -> Select t -> Select t
limit n s = s { selLimit = Just (max 0 n) }
-- ANCHOR_END: select

-- ANCHOR: compileSelect
-- 🖨️ To SQL. Every condition is joined with AND. The limit is a value
--    too, so it goes into a ? hole.
-- SQL に。条件は全部 AND でつなぐ。人数も値なので、? の穴に入れる
compileSelect :: Select t -> (Text, [SQLData])
compileSelect s =
  ( T.unwords (concat [selectPart, wherePart, orderPart, limitPart])
  , params ++ limitParams )
  where
    selectPart = [ "SELECT", T.intercalate ", " (tableColumns (selTable s))
                 , "FROM", tableName (selTable s) ]
    (conds, params) = unzip' (map render (selWhere s))
    wherePart
      | null conds = []
      | otherwise  = ["WHERE", T.intercalate " AND " conds]
    orderPart
      | null (selOrder s) = []
      | otherwise = ["ORDER BY", T.intercalate ", " (map order (selOrder s))]
    order (name, Asc)  = name <> " ASC"
    order (name, Desc) = name <> " DESC"
    limitPart   = maybe [] (const ["LIMIT", "?"]) (selLimit s)
    limitParams = maybe [] (\n -> [SQLInteger (fromIntegral n)]) (selLimit s)
    unzip' pairs = (map fst pairs, concatMap snd pairs)

-- ▶️ Run it, and read the rows as t.
-- 動かして、行を t として読む
runSelect :: FromRow t => Connection -> Select t -> IO [t]
runSelect conn s =
  let (sql, params) = compileSelect s
  in  query conn (Query sql) params
-- ANCHOR_END: compileSelect

-- ANCHOR: update
-- ✏️ One assignment: a column, and a new value of the same type.
--    Assign t hides the column's type, so a list can mix columns.
-- 代入1つ：列と、同じ型の新しい値。
-- Assign t は列の型を隠すので、いろいろな列を1つのリストにできる
data Assign t where
  Assign :: Col t a -> Expr t a -> Assign t

(.=) :: Col t a -> Expr t a -> Assign t
(.=) = Assign
infix 1 .=

compileUpdate :: Table t -> [Assign t] -> Expr t Bool -> (Text, [SQLData])
compileUpdate table assigns cond =
  ( "UPDATE " <> tableName table <> " SET " <> T.intercalate ", " sets
      <> " WHERE " <> condText
  , concat setParams ++ condParams )
  where
    (sets, setParams) = unzip (map one assigns)
    one (Assign c e) =
      let (t, ps) = render e
      in  (colName c <> " = " <> t, ps)
    (condText, condParams) = render cond

compileDelete :: Table t -> Expr t Bool -> (Text, [SQLData])
compileDelete table cond =
  let (condText, ps) = render cond
  in  ("DELETE FROM " <> tableName table <> " WHERE " <> condText, ps)

-- ▶️ Run them, and say how many rows changed.
-- 動かして、何行変わったかを答える
runUpdate :: Connection -> Table t -> [Assign t] -> Expr t Bool -> IO Int
runUpdate conn table assigns cond = do
  let (sql, ps) = compileUpdate table assigns cond
  execute conn (Query sql) ps
  changes conn

runDelete :: Connection -> Table t -> Expr t Bool -> IO Int
runDelete conn table cond = do
  let (sql, ps) = compileDelete table cond
  execute conn (Query sql) ps
  changes conn
-- ANCHOR_END: update
-- ANCHOR_END: all
