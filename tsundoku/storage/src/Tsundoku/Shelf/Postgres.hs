-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE MultilineStrings #-}
-- | The shelf in PostgreSQL, through hasql 2.
-- PostgreSQL の本棚。hasql 2 を使う
module Tsundoku.Shelf.Postgres
  ( withPostgresShelf
  , PostgresError (..)
  ) where

-- exceptions, loops, text, time, and trees
-- 例外、くり返し、文字列、日時、木
import Control.Exception (Exception, bracket, throw, throwIO)
import Control.Monad (forM_, when)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, UTCTime)
import Data.Traversable (for)
import Data.Tree (Tree (..))

-- hasql: connections, statements, and how values go in and come out
-- hasql：接続、文、値の入れ方と取り出し方
import Hasql.Connection (Connection)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Errors (ConnectionError, SessionError)
import Hasql.Statement (Statement, preparable)
import Hasql.Transaction (Transaction, statement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions
  (IsolationLevel (..), Mode (..), transaction)
import Pqi.Ffi qualified as Pqi

import Tsundoku.Card qualified as Card
import Tsundoku.Card (Card (..), newCard)
import Tsundoku.Rating (Score, scoreValue)
import Tsundoku.Shelf
import Tsundoku.Shelf.Rows
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: withPostgresShelf
-- 🐘 Connect, build the tables if needed, and lend the shelf.
-- つないで、表がなければ作って、本棚を貸す
withPostgresShelf :: Text -> (Shelf -> IO a) -> IO a
withPostgresShelf url action = bracket open Connection.release $ \conn -> do
  migrate conn
  action (postgresShelf conn)
  where
    open = Connection.acquire Pqi.adapter (Settings.connectionString url)
             >>= either (throwIO . CannotConnect) pure

-- 🚨 hasql's errors are values. Turn them into exceptions here.
-- hasql のエラーは値。ここで例外にする
data PostgresError
  = CannotConnect ConnectionError
  | Failed        SessionError
  deriving Show

instance Exception PostgresError

-- 🔒 Run a transaction: all of it, or none of it. Serializable: if
--    another program changed what we read before we wrote, PostgreSQL
--    says so (error 40001), and hasql-transaction runs the job again.
-- トランザクションを動かす。全部か、何もなしか。Serializable：読んでから
-- 書くまでに、ほかのプログラムが同じものを変えていたら、PostgreSQL が
-- そう言い（エラー 40001）、hasql-transaction が仕事をやり直す
inTx :: Connection -> Transaction a -> IO a
inTx conn tx = do
  result <- Connection.use conn (transaction Serializable Write tx)
  either (throwIO . Failed) pure result
-- ANCHOR_END: withPostgresShelf

-- ANCHOR: postgresShelf
-- 🪟 The same window as SQLite's, with PostgreSQL behind it.
-- SQLite と同じ窓口。向こうにいるのが PostgreSQL
postgresShelf :: Connection -> Shelf
postgresShelf conn = Shelf
  { -- 📚 The works on the shelf
    -- 本棚の作品
    addWork    = \w       -> run (insertWork w)
  , removeWork = \wid     -> run (deleteWork wid)
  , getCard    = \wid     -> run (loadCard   wid)
  , listCards  =             run  loadAll

    -- 📖 Reading, notes, and ratings
    -- 読む・感想・評価
  , applyEvent = \wid e   -> fmap reading <$>
      edit  wid (Card.applyEvent e)   (updateReading wid)
  , addNote    = \wid n   ->
      edit_ wid (Card.addNote n)      (insertNote    wid n)
  , logReading = \wid l   ->
      edit_ wid (Card.logReading l)   (insertLog     wid l)
  , rate       = \wid s   ->
      edit_ wid (Right . Card.rate s) (updateRating  wid s)
  , rateEntry  = \wid i s ->
      edit_ wid (Card.rateEntry i s)  (upsertRating  wid i s)
  }
  where
    -- Run a job in a transaction on this connection.
    -- この接続で、仕事をトランザクションの中で動かす
    run :: Transaction a -> IO a
    run = inTx conn
    -- Read, decide, write: the same as SQLite's.
    -- 読む → 決める → 書く。SQLite と同じ
    edit  :: WorkId -> (Card -> Checked Card)
          -> (Card -> Transaction ()) -> IO (Checked Card)
    edit wid core write = run $ do
      found <- loadCard wid
      case decide wid core found of
        Left why  -> pure (Left why)
        Right new -> do
          write new
          pure (Right new)
    edit_ :: WorkId -> (Card -> Checked Card)
          -> Transaction () -> IO (Checked ())
    edit_ wid core write = do
      result <- edit wid core (\_ -> write)
      pure (fmap (\_ -> ()) result)

-- 📚 Every card on the shelf, in the order the works were added.
-- 本棚のカードを全部、作品を足した順に
loadAll :: Transaction [(WorkId, Card)]
loadAll = do
  ids   <- statement () selectWorkIds
  pairs <- for ids $ \w -> do
    card <- loadCard (WorkId w)
    pure (WorkId w, card)
  pure [ (wid, card) | (wid, Just card) <- pairs ]
-- ANCHOR_END: postgresShelf

-- ANCHOR: codecs
-- 🔢 Our numbers are Int; PostgreSQL's bigint is a 64-bit Int64.
-- わたしたちの数は Int。PostgreSQL の bigint は 64 ビットの Int64
int :: E.Value Int
int = fromIntegral >$< E.int8

intColumn :: D.Value Int
intColumn = fromIntegral <$> (D.int8 :: D.Value Int64)

-- 📨 A parameter that is always there, or one that may be NULL.
-- いつもある引数と、NULL かもしれない引数
param :: E.Value a -> E.Params a
param = E.param . E.nonNullable

nullableParam :: E.Value a -> E.Params (Maybe a)
nullableParam = E.param . E.nullable

-- 📬 A column that is always there, or one that may be NULL.
-- いつもある列と、NULL かもしれない列
column :: D.Value a -> D.Row a
column = D.column . D.nonNullable

nullableColumn :: D.Value a -> D.Row (Maybe a)
nullableColumn = D.column . D.nullable

-- 🪪 The numbers of a work and an entry, as parameters.
-- 作品と項目の番号を、引数に
workId :: E.Value WorkId
workId = (\(WorkId w) -> w) >$< int

entryNo :: E.Value EntryId
entryNo = (\(EntryId i) -> i) >$< int
-- ANCHOR_END: codecs

-- ANCHOR: insertWork
-- 📥 Put a new card in the tables. PostgreSQL gives every number.
-- 新しいカードを表にしまう。番号は全部 PostgreSQL が振る
insertWork :: Work -> Transaction WorkId
insertWork w = do
  let (kind, minutes, toc) = toPlain (contents w)
      card                 = newCard w
  wid <- statement (title w, creator w, kind, minutes, reading card) newWork
  insertForest wid Nothing toc
  pure (WorkId wid)

-- 🌳 Each tree, root first: the children need the root's number.
-- 木を1本ずつ、根から。子には、根の番号が要るから
insertForest :: Int -> Maybe Int -> Toc Int -> Transaction ()
insertForest wid parent forest =
  forM_ (zip [1 ..] forest) $ \(position, Node entry children) -> do
    eid <- statement (wid, parent, position, entry) newEntry
    insertForest wid (Just eid) children

newWork :: Statement (Text, Text, Text, Maybe Int, ReadingState) Int
newWork = preparable sql encoder (D.singleRow (column intColumn))
  where
    sql = """
      INSERT INTO works (title, creator, kind, minutes, reading)
      VALUES ($1, $2, $3, $4, $5)
      RETURNING id
      """
    encoder = mconcat
      [ (\(t, _, _, _, _) -> t)         >$< param E.text
      , (\(_, c, _, _, _) -> c)         >$< param E.text
      , (\(_, _, k, _, _) -> k)         >$< param E.text
      , (\(_, _, _, m, _) -> m)         >$< nullableParam int
      , (\(_, _, _, _, r) -> stateName r) >$< param E.text
      ]

newEntry :: Statement (Int, Maybe Int, Int, Entry Int) Int
newEntry = preparable sql encoder (D.singleRow (column intColumn))
  where
    sql = """
      INSERT INTO entries (work_id, parent_id, position, heading, size)
      VALUES ($1, $2, $3, $4, $5)
      RETURNING id
      """
    encoder = mconcat
      [ (\(w, _, _, _) -> w)         >$< param int
      , (\(_, p, _, _) -> p)         >$< nullableParam int
      , (\(_, _, n, _) -> n)         >$< param int
      , (\(_, _, _, e) -> heading e) >$< param E.text
      , (\(_, _, _, e) -> size e)    >$< param int
      ]
-- ANCHOR_END: insertWork

-- ANCHOR: changes
-- ✏️ The jobs that write one change. Each one runs its statement:
--    the arguments, and the statement (SQL, encoder, decoder).
-- 変更を1つ書く仕事。どれも、文を1つ動かす：引数と、文
-- （SQL、引数の入れ方、答えの取り出し方）
updateReading :: WorkId -> Card -> Transaction ()
updateReading wid card =
  statement (wid, stateName (reading card)) $ preparable
    "UPDATE works SET reading = $2 WHERE id = $1"
    ((fst >$< param workId) <> (snd >$< param E.text))
    D.noResult

updateRating :: WorkId -> Score -> Transaction ()
updateRating wid s =
  statement (wid, s) $ preparable
    "UPDATE works SET rating = $2 WHERE id = $1"
    ((fst >$< param workId) <> ((scoreValue . snd) >$< param int))
    D.noResult

insertNote :: WorkId -> Note -> Transaction ()
insertNote wid n = statement (wid, n) (preparable sql encoder D.noResult)
  where
    sql = """
      INSERT INTO notes (work_id, entry_id, body, written_at, spoiler)
      VALUES ($1, $2, $3, $4, $5)
      """
    encoder = mconcat
      [ fst                          >$< param workId
      , (targetEntry . target . snd) >$< nullableParam int
      , (body . snd)                 >$< param E.text
      , (writtenAt . snd)            >$< param E.timestamptz
      , (spoiler . snd)              >$< param E.bool
      ]

insertLog :: WorkId -> LogLine -> Transaction ()
insertLog wid l = statement (wid, l) (preparable sql encoder D.noResult)
  where
    sql = """
      INSERT INTO reading_log (work_id, entry_id, read_on)
      VALUES ($1, $2, $3)
      """
    encoder = mconcat
      [ fst               >$< param workId
      , (readEntry . snd) >$< param entryNo
      , (readOn . snd)    >$< param E.date
      ]

upsertRating :: WorkId -> EntryId -> Score -> Transaction ()
upsertRating wid i s =
  statement (wid, i, s) (preparable sql encoder D.noResult)
  where
    sql = """
      INSERT INTO entry_ratings (entry_id, work_id, score) VALUES ($2, $1, $3)
      ON CONFLICT (entry_id) DO UPDATE SET score = excluded.score
      """
    encoder = mconcat
      [ (\(w, _, _) -> w)            >$< param workId
      , (\(_, e, _) -> e)            >$< param entryNo
      , (\(_, _, v) -> scoreValue v) >$< param int
      ]

-- 🗑️ Remove a work; the database removes the rest. Was it there?
-- 作品を消す。残りはデータベースが消す。その作品はあった？
deleteWork :: WorkId -> Transaction Bool
deleteWork (WorkId w) = do
  removed <- statement w $ preparable
    "DELETE FROM works WHERE id = $1"
    (param int)
    D.rowsAffected
  pure (removed > 0)
-- ANCHOR_END: changes

-- ANCHOR: loadCard
-- 📖 Read a card back from its tables, if the work is there.
-- 作品があれば、表からカードを読み戻す
loadCard :: WorkId -> Transaction (Maybe Card)
loadCard wid = do
  found <- about selectWork
  for found $ \row -> do
    toc     <- about selectToc
    notes'  <- about selectNotes
    log'    <- about selectLog
    ratings <- about selectRatings
    case assemble row toc notes' log' ratings of
      Left broken -> throw broken
      Right card  -> pure card
  where
    -- Every query here is about this one work.
    -- ここの問い合わせは、どれもこの作品について
    about :: Statement WorkId r -> Transaction r
    about query = statement wid query

selectWorkIds :: Statement () [Int]
selectWorkIds = preparable
  "SELECT id FROM works ORDER BY id"
  mempty
  (D.rowList (column intColumn))

selectWork :: Statement WorkId (Maybe WorkRow)
selectWork = preparable sql (param workId) (D.rowMaybe row)
  where
    sql = """
      SELECT title, creator, kind, minutes, reading, rating
        FROM works WHERE id = $1
      """
    row = WorkRow
      <$> column D.text
      <*> column D.text
      <*> column D.text
      <*> nullableColumn intColumn
      <*> column D.text
      <*> nullableColumn intColumn

selectNotes :: Statement WorkId [(Maybe Int, Text, UTCTime, Bool)]
selectNotes = preparable sql (param workId) (D.rowList row)
  where
    sql = """
      SELECT entry_id, body, written_at, spoiler
        FROM notes WHERE work_id = $1 ORDER BY id
      """
    row = (,,,)
      <$> nullableColumn intColumn
      <*> column D.text
      <*> column D.timestamptz
      <*> column D.bool

selectLog :: Statement WorkId [(Day, Int)]
selectLog = preparable sql (param workId) (D.rowList row)
  where
    sql = """
      SELECT read_on, entry_id
        FROM reading_log WHERE work_id = $1 ORDER BY id
      """
    row = (,) <$> column D.date <*> column intColumn

selectRatings :: Statement WorkId [(Int, Int)]
selectRatings = preparable
  "SELECT entry_id, score FROM entry_ratings WHERE work_id = $1"
  (param workId)
  (D.rowList ((,) <$> column intColumn <*> column intColumn))
-- ANCHOR_END: loadCard

-- ANCHOR: selectToc
-- 🌲 The whole table of contents, top to bottom, with depths. The path
--    is an array of positions: PostgreSQL sorts arrays as they are.
-- 目次の全部を、上から順に、深さつきで。道順は位置の配列。
-- PostgreSQL は、配列をそのまま並べ替えられる
selectToc :: Statement WorkId [(Int, Int, Text, Int)]
selectToc = preparable sql (param workId) (D.rowList row)
  where
    sql = """
      WITH RECURSIVE toc (id, depth, heading, size, path) AS
        ( SELECT id, 0::bigint, heading, size, ARRAY[position]
            FROM entries
           WHERE work_id = $1 AND parent_id IS NULL
          UNION ALL
          SELECT e.id, toc.depth + 1, e.heading, e.size,
                 toc.path || e.position
            FROM entries AS e
            JOIN toc ON e.parent_id = toc.id
        )
      SELECT id, depth, heading, size FROM toc ORDER BY path
      """
    row = (,,,)
      <$> column intColumn
      <*> column intColumn
      <*> column D.text
      <*> column intColumn
-- ANCHOR_END: selectToc

-- ANCHOR: migrations
-- 🧱 The steps that build the tables, in order (as in SQLite's).
-- 表を作る手順を、順番に（SQLite と同じく）
migrations :: [Text]
migrations =
  [ -- 1: works and their tables of contents, notes, log, and ratings
    -- 1: 作品と、その目次・感想・読書ログ・評価
    """
    CREATE TABLE works
      ( id      bigint  GENERATED ALWAYS AS IDENTITY PRIMARY KEY
      , title   text    NOT NULL
      , creator text    NOT NULL
      , kind    text    NOT NULL
          CHECK (kind IN ('book', 'serial', 'series', 'film', 'game'))
      , minutes bigint
      , reading text    NOT NULL
          CHECK (reading IN ('Piled', 'Reading', 'Finished', 'Paused'))
      , rating  bigint  CHECK (rating BETWEEN 0 AND 100)
      );
    CREATE TABLE entries
      ( id        bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY
      , work_id   bigint NOT NULL REFERENCES works   ON DELETE CASCADE
      , parent_id bigint          REFERENCES entries ON DELETE CASCADE
      , position  bigint NOT NULL
      , heading   text   NOT NULL
      , size      bigint NOT NULL CHECK (size >= 0)
      );
    CREATE TABLE notes
      ( id         bigint      GENERATED ALWAYS AS IDENTITY PRIMARY KEY
      , work_id    bigint      NOT NULL REFERENCES works   ON DELETE CASCADE
      , entry_id   bigint               REFERENCES entries ON DELETE CASCADE
      , body       text        NOT NULL
      , written_at timestamptz NOT NULL
      , spoiler    boolean     NOT NULL
      );
    CREATE TABLE reading_log
      ( id       bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY
      , work_id  bigint NOT NULL REFERENCES works   ON DELETE CASCADE
      , entry_id bigint NOT NULL REFERENCES entries ON DELETE CASCADE
      , read_on  date   NOT NULL
      );
    CREATE TABLE entry_ratings
      ( entry_id bigint PRIMARY KEY REFERENCES entries ON DELETE CASCADE
      , work_id  bigint NOT NULL    REFERENCES works   ON DELETE CASCADE
      , score    bigint NOT NULL    CHECK (score BETWEEN 0 AND 100)
      );
    """
  ]

-- 🪜 Run the steps not run yet. PostgreSQL has no user_version, so we
--    keep the number in a small table of our own. All of it is one
--    transaction that first takes a lock of our own (an advisory lock):
--    when two programs start on a new database at once, the second
--    waits here, then reads the number the first wrote. ReadCommitted,
--    not Serializable: after the wait, each statement must see what the
--    first program committed.
-- まだ動かしていない手順を動かす。PostgreSQL には user_version がないので、
-- 番号は、自分で作った小さな表にしまう。全部を1つのトランザクションに
-- して、はじめに自分たちの鍵（advisory lock）を取る。2つのプログラムが
-- 同時に新しいデータベースで起きても、後のほうはここで待ち、先のほうが
-- 書いた番号を読む。Serializable ではなく ReadCommitted：待ったあとの
-- 文が、先のほうが確定したものを見られるように
migrate :: Connection -> IO ()
migrate conn = do
  result <- Connection.use conn (transaction ReadCommitted Write steps)
  either (throwIO . Failed) pure result
  where
    steps = do
      -- The lock is let go when the transaction ends.
      -- 鍵は、トランザクションが終わるときに放される
      Transaction.sql
        "SELECT pg_advisory_xact_lock(hashtext('tsundoku migrate'))"
      Transaction.sql """
        CREATE TABLE IF NOT EXISTS schema_version (version bigint NOT NULL)
        """
      done <- statement () currentVersion
      forM_ (zip [1 ..] migrations) $ \(n, step) ->
        when (n > done) $
          Transaction.sql (encodeUtf8 (step <> record n))
    record n = "INSERT INTO schema_version (version) VALUES ("
                 <> T.pack (show (n :: Int)) <> ");"

currentVersion :: Statement () Int
currentVersion = preparable
  "SELECT coalesce(max(version), 0) FROM schema_version"
  mempty
  (D.singleRow (column intColumn))
-- ANCHOR_END: migrations
-- ANCHOR_END: all
