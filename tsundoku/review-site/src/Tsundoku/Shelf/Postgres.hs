-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE MultilineStrings #-}
-- | The shelf in PostgreSQL, through hasql 2.
-- PostgreSQL の本棚。hasql 2 を使う
module Tsundoku.Shelf.Postgres
  ( withPostgresShelf
  , PostgresError (..)
  ) where

-- exceptions, loops, JSON, text, time, and trees
-- 例外、くり返し、JSON、文字列、日時、木
import Control.Exception (Exception, bracket, throw, throwIO)
import Data.Aeson (Value)
import Control.Monad (forM_, when)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
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
import Hasql.Session qualified as Session
import Hasql.Statement (Statement, preparable)
import Hasql.Transaction (Transaction, statement)
import Hasql.Transaction.Sessions
  (IsolationLevel (..), Mode (..), transaction)
import Pqi.Ffi qualified as Pqi

import Tsundoku.Card qualified as Card
import Tsundoku.Card (Card (..), newCard)
import Tsundoku.Custom qualified as Custom
import Tsundoku.Query (search)
import Tsundoku.Rating (Score, scoreValue)
import Tsundoku.Search
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
    works = WorkWindow
      { addWork    = \w       -> run (insertWork w)
      , removeWork = \wid     -> run (deleteWork wid)
      , getCard    = \wid     -> run (loadCard   wid)
      , listCards  =             run  loadAll
      , reviseWork = revise
      }

  , -- 📖 Reading, notes, and ratings
    -- 読む・感想・評価
    reading = ReadingWindow
      { applyEvent = \wid e   -> fmap reading <$>
          edit  wid (Card.applyEvent e)   (updateReading wid)
      , addNote    = \wid n   ->
          edit_ wid (Card.addNote n)      (insertNote    wid n)
      , logReading = \wid l   ->
          edit_ wid (Card.logReading l)   (insertLog     wid l)
      , unread     = \wid i   ->
          edit_ wid (Card.unread i)       (deleteLog     wid i)
      , rate       = \wid s   ->
          edit_ wid (Right . Card.rate s) (updateRating  wid s)
      , rateEntry  = \wid i s ->
          edit_ wid (Card.rateEntry i s)  (upsertRating  wid i s)
      }

  , -- 🏷️ My own attributes and states
    -- 自分で決める属性と状態
    custom = CustomWindow
      { getSchema  =             run  loadSchema
      , defineAttr = \d       ->
          define (Custom.defineAttr d)    (insertDefinition (Left  d))
      , defineFlow = \f       ->
          define (Custom.defineFlow f)    (insertDefinition (Right f))
      , editAttrs  = \wid t   ->
          custom wid (Card.editAttrs t)   (updateAttrs  wid)
      , moveFlow   = \wid f s ->
          custom wid (Card.moveFlow f s)  (updateStates wid)
      , findCards  = \q       -> run (searchAll q)
      }

  , -- 📥 Taking things in from outside
    -- 外から取り込む
    intake = IntakeWindow
      { addEpisodes = grow
      }

  , -- 🔎 Searching what is inside
    -- 中身を探す
    inside = InsideWindow
      { setText    = \wid i t ->
          edit_ wid (Card.hasEntry i)     (upsertText   wid i t)
      , searchText = searchWith (run . searchDocuments)
      }

  , -- 🔖 Tags
    -- タグ
    tagging = TagWindow
      { setTags = \wid ts ->
          edit_ wid (Card.setTags ts)     (updateTags   wid ts)
      }
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
    edit wid core write = run (change wid core write)
    edit_ :: WorkId -> (Card -> Checked Card)
          -> Transaction () -> IO (Checked ())
    edit_ wid core write = do
      result <- edit wid core (\_ -> write)
      pure (fmap (\_ -> ()) result)
    custom :: WorkId -> (Schema -> Card -> Checked Card)
           -> (Card -> Transaction ()) -> IO (Checked ())
    custom wid core write = do
      result <- run $ do
        schema <- loadSchema
        change wid (core schema) write
      pure (fmap (\_ -> ()) result)
    define :: (Schema -> Checked Schema) -> Transaction ()
           -> IO (Checked ())
    define core write = run $ do
      schema <- loadSchema
      case core schema of
        Left why -> pure (Left why)
        Right _  -> do
          write
          pure (Right ())
    -- ANCHOR: pgGrow
    -- New episodes: PostgreSQL gives the numbers, as SQLite's does.
    -- 新しい話：番号は PostgreSQL が出す。SQLite と同じ
    grow :: WorkId -> Int -> IO (Checked ())
    grow wid n = do
      result <- run $ do
        fresh <- reserveEntries n
        change wid (Card.addEpisodes fresh) (insertGrown wid fresh)
      pure (fmap (\_ -> ()) result)
    -- ANCHOR_END: pgGrow
    -- ANCHOR: pgRevise
    -- A work fixed by hand: the new lines take numbers in a row from
    -- the entries' sequence.
    -- 手で直した作品：新しい行は、項目の番号の列から続けて番号を取る
    revise :: WorkId -> Revision -> IO (Checked ())
    revise wid r = do
      result <- run $ do
        from <- reserveFrom (Card.newItems r)
        change wid (Card.revise from r)
                   (writeRevision wid (notesOnRemoved r))
      pure (fmap (\_ -> ()) result)
    -- ANCHOR_END: pgRevise

-- 🔁 Read, decide, write: one change to one card, in a transaction.
-- 読む → 決める → 書く：カード1枚の変更を、トランザクションの中で
change :: WorkId -> (Card -> Checked Card) -> (Card -> Transaction ())
       -> Transaction (Checked Card)
change wid core write = do
  found <- loadCard wid
  case decide wid core found of
    Left why  -> pure (Left why)
    Right new -> do
      write new
      pure (Right new)

-- 🔎 Search every card on the shelf. The core picks them.
-- 本棚のカードを全部、絞り込む。選ぶのは核
searchAll :: Text -> Transaction (Checked [(WorkId, Card)])
searchAll typed = do
  schema <- loadSchema
  cards  <- loadAll
  pure (search schema typed cards)

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

-- ANCHOR: pgDeleteLog
-- 🧽 The same as SQLite's: every line about one entry of one work.
-- SQLite と同じ：作品1つの、項目1つについての行を全部消す
deleteLog :: WorkId -> EntryId -> Transaction ()
deleteLog wid i = statement (wid, i) (preparable sql encoder D.noResult)
  where
    sql = "DELETE FROM reading_log WHERE work_id = $1 AND entry_id = $2"
    encoder = mconcat
      [ fst >$< param workId
      , snd >$< param entryNo
      ]
-- ANCHOR_END: pgDeleteLog

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

-- 🏷️ Write my own attributes and states, as jsonb.
-- 自分で決めた属性と状態を、jsonb で書く
updateAttrs :: WorkId -> Card -> Transaction ()
updateAttrs wid card =
  statement (wid, attrsJson (attrs card)) $ preparable
    "UPDATE works SET attrs = $2 WHERE id = $1"
    ((fst >$< param workId) <> (snd >$< param E.jsonb))
    D.noResult

updateTags :: WorkId -> Set Text -> Transaction ()
updateTags wid ts =
  statement (wid, tagsJson ts) $ preparable
    "UPDATE works SET tags = $2 WHERE id = $1"
    ((fst >$< param workId) <> (snd >$< param E.jsonb))
    D.noResult

updateStates :: WorkId -> Card -> Transaction ()
updateStates wid card =
  statement (wid, statesJson (states card)) $ preparable
    "UPDATE works SET states = $2 WHERE id = $1"
    ((fst >$< param workId) <> (snd >$< param E.jsonb))
    D.noResult

-- 📒 Keep a definition, and read them all back in order.
-- 定義をしまう。そして、全部を順に読み戻す
insertDefinition :: Either AttrDef Flow -> Transaction ()
insertDefinition def =
  statement (definitionRow def) (preparable sql encoder D.noResult)
  where
    sql = "INSERT INTO definitions (name, what, body) VALUES ($1, $2, $3)"
    encoder = mconcat
      [ (\(n, _, _) -> n) >$< param E.text
      , (\(_, w, _) -> w) >$< param E.text
      , (\(_, _, b) -> b) >$< param E.jsonb
      ]

loadSchema :: Transaction Schema
loadSchema = do
  rows <- statement () selectDefinitions
  case schemaFromRows rows of
    Left broken  -> throw broken
    Right schema -> pure schema

selectDefinitions :: Statement () [(Text, Text, Value)]
selectDefinitions = preparable
  "SELECT name, what, body FROM definitions ORDER BY id"
  mempty
  (D.rowList ((,,) <$> column D.text <*> column D.text <*> column D.jsonb))

-- ANCHOR: pgRows
-- 🔢 The next n numbers of the entries' sequence. A number taken here
--    is never given again, even if the core says no (a gap is fine).
-- 項目の番号の列から、次の n 個。ここで取った番号は、核が「だめ」と
-- 言っても二度と振られない（欠番になってよい）
reserveEntries :: Int -> Transaction [EntryId]
reserveEntries n = statement n $ preparable sql (param int) decoder
  where
    sql = """
      SELECT nextval(pg_get_serial_sequence('entries', 'id'))
      FROM generate_series(1, $1)
      """
    decoder = D.rowList (EntryId <$> column intColumn)

-- 🌱 Put the new episodes in with the numbers we took. The id column is
--    GENERATED ALWAYS, so we say OVERRIDING SYSTEM VALUE.
-- 新しい話を、取った番号のまま書く。id の列は GENERATED ALWAYS なので、
-- OVERRIDING SYSTEM VALUE と断る
insertGrown :: WorkId -> [EntryId] -> Card -> Transaction ()
insertGrown wid fresh card =
  forM_ [ row | row@(_, _, e) <- placed toc, entryId e `elem` fresh ] $
    \(parent, position, e) ->
      statement (wid, parent, position, e) (preparable sql encoder D.noResult)
  where
    (_, _, toc) = toPlain (contents (work card))
    sql = """
      INSERT INTO entries (id, work_id, parent_id, position, heading, size)
      OVERRIDING SYSTEM VALUE
      VALUES ($1, $2, $3, $4, $5, $6)
      """
    encoder = mconcat
      [ (\(_, _, _, e) -> entryId e) >$< param entryNo
      , (\(w, _, _, _) -> w)         >$< param workId
      , (\(_, p, _, _) -> p)         >$< nullableParam entryNo
      , (\(_, _, n, _) -> n)         >$< param int
      , (\(_, _, _, e) -> heading e) >$< param E.text
      , (\(_, _, _, e) -> size e)    >$< param int
      ]
-- ANCHOR_END: pgRows

-- ANCHOR: pgWriteRevision
-- 🔢 Take n numbers in a row from the entries' sequence, and say the
--    first. (Nothing new: no number is taken, and the first is not used.)
-- 項目の番号の列から、続けて n 個を取り、最初の番号を言う。
-- （新しい行がなければ、番号は取らない。最初の番号も使われない）
reserveFrom :: Int -> Transaction EntryId
reserveFrom 0 = pure (EntryId 0)
reserveFrom n = statement n $ preparable sql (param int) decoder
  where
    sql = """
      SELECT setval(seq, nextval(seq) + $1 - 1) - $1 + 1
        FROM (SELECT pg_get_serial_sequence('entries', 'id') AS seq)
      """
    decoder = D.singleRow (EntryId <$> column intColumn)

-- ✏️ Write a work fixed by hand, in the same order as SQLite's: move (or
--    delete, as chosen) the notes of lines that are gone, change or put
--    in every line from the top, then delete the lines that are gone
--    (ON DELETE CASCADE takes their ratings, log, and texts).
-- 手で直した作品を、SQLite と同じ順番で書く：消える行の感想を、選んだ
-- とおりに移すか消し、どの行も上から書き換えるか足し、最後に消える行を
-- 消す（その★・ログ・本文は、ON DELETE CASCADE が一緒に消す）
writeRevision :: WorkId -> NotesOnRemoved -> Card -> Transaction ()
writeRevision wid choice card = do
  statement (wid, work card) renameWork
  had <- statement wid entriesOf
  let now  = [ entryId e | (_, _, e) <- placed toc ]
      gone = [ i | i <- had, i `notElem` now ]
  forM_ gone $ \i -> statement i $ case choice of
    MoveToWork -> moveNotes
    DropNotes  -> dropNotes
  forM_ (placed toc) $ \(parent, position, e) ->
    if entryId e `elem` had
      then statement (parent, position, e) changeEntry
      else statement (wid, parent, position, e) putEntry
  forM_ gone $ \i -> statement i dropEntry
  where
    (_, _, toc) = toPlain (contents (work card))

renameWork :: Statement (WorkId, Work) ()
renameWork = preparable
  "UPDATE works SET title = $2, creator = $3 WHERE id = $1"
  (mconcat [ fst             >$< param workId
           , (title . snd)   >$< param E.text
           , (creator . snd) >$< param E.text ])
  D.noResult

entriesOf :: Statement WorkId [EntryId]
entriesOf = preparable
  "SELECT id FROM entries WHERE work_id = $1"
  (param workId)
  (D.rowList (EntryId <$> column intColumn))

moveNotes :: Statement EntryId ()
moveNotes = preparable
  "UPDATE notes SET entry_id = NULL WHERE entry_id = $1"
  (param entryNo)
  D.noResult

dropNotes :: Statement EntryId ()
dropNotes = preparable
  "DELETE FROM notes WHERE entry_id = $1"
  (param entryNo)
  D.noResult

changeEntry :: Statement (Maybe EntryId, Int, Entry Int) ()
changeEntry = preparable sql encoder D.noResult
  where
    sql = """
      UPDATE entries SET parent_id = $2, position = $3, heading = $4,
                         size = $5
       WHERE id = $1
      """
    encoder = mconcat
      [ (\(_, _, e) -> entryId e) >$< param entryNo
      , (\(p, _, _) -> p)         >$< nullableParam entryNo
      , (\(_, n, _) -> n)         >$< param int
      , (\(_, _, e) -> heading e) >$< param E.text
      , (\(_, _, e) -> size e)    >$< param int
      ]

putEntry :: Statement (WorkId, Maybe EntryId, Int, Entry Int) ()
putEntry = preparable sql encoder D.noResult
  where
    sql = """
      INSERT INTO entries (id, work_id, parent_id, position, heading, size)
      OVERRIDING SYSTEM VALUE
      VALUES ($1, $2, $3, $4, $5, $6)
      """
    encoder = mconcat
      [ (\(_, _, _, e) -> entryId e) >$< param entryNo
      , (\(w, _, _, _) -> w)         >$< param workId
      , (\(_, p, _, _) -> p)         >$< nullableParam entryNo
      , (\(_, _, n, _) -> n)         >$< param int
      , (\(_, _, _, e) -> heading e) >$< param E.text
      , (\(_, _, _, e) -> size e)    >$< param int
      ]

dropEntry :: Statement EntryId ()
dropEntry = preparable
  "DELETE FROM entries WHERE id = $1"
  (param entryNo)
  D.noResult
-- ANCHOR_END: pgWriteRevision

-- ANCHOR: upsertText
-- 📝 Keep the text of an entry. A new text replaces the old one.
-- 項目の本文をしまう。新しい本文は、古いものと入れ替える
upsertText :: WorkId -> EntryId -> Text -> Transaction ()
upsertText wid i text =
  statement (wid, i, text) (preparable sql encoder D.noResult)
  where
    sql = """
      INSERT INTO texts (entry_id, work_id, body) VALUES ($2, $1, $3)
      ON CONFLICT (entry_id) DO UPDATE SET body = excluded.body
      """
    encoder = mconcat
      [ (\(w, _, _) -> w) >$< param workId
      , (\(_, e, _) -> e) >$< param entryNo
      , (\(_, _, t) -> t) >$< param E.text
      ]
-- ANCHOR_END: upsertText

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
  schema <- loadSchema
  found  <- about selectWork
  for found $ \row -> do
    toc     <- about selectToc
    notes'  <- about selectNotes
    log'    <- about selectLog
    ratings <- about selectRatings
    case assemble schema row toc notes' log' ratings of
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
      SELECT title, creator, kind, minutes, reading, rating, attrs, states,
             tags
        FROM works WHERE id = $1
      """
    row = WorkRow
      <$> column D.text
      <*> column D.text
      <*> column D.text
      <*> nullableColumn intColumn
      <*> column D.text
      <*> nullableColumn intColumn
      <*> column D.jsonb
      <*> column D.jsonb
      <*> column D.jsonb

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

-- ANCHOR: searchDocuments
-- 🔎 Search the documents with PGroonga: every word, short or long, in
--    one query. The order is PGroonga's score.
-- PGroonga で documents を探す。短い語も長い語も、1つの問い合わせで。
-- 順番は PGroonga の点数
searchDocuments :: Search -> Transaction [Hit]
searchDocuments s = do
  rows <- statement (groongaQuery s) selectDocuments
  for rows $ \(w, workTitle, name, entry, spoils, text) ->
    case placeOf name entry spoils of
      Left broken -> throw broken
      Right found -> do
        path <- maybe (pure []) trailOf (entryOf found)
        pure (Hit (WorkId w) workTitle found path (snippetOf s text))
  where
    trailOf i = statement i selectTrail

selectDocuments :: Statement Text [(Int, Text, Text, Maybe Int, Bool, Text)]
selectDocuments = preparable sql (param E.text) (D.rowList row)
  where
    sql = """
      SELECT work_id, (SELECT title FROM works WHERE id = work_id),
             place, entry_id, spoiler, body
        FROM documents
       WHERE body &@~ $1
       ORDER BY score DESC, work_id
      """
    row = (,,,,,)
      <$> column intColumn
      <*> column D.text
      <*> column D.text
      <*> nullableColumn intColumn
      <*> column D.bool
      <*> column D.text
-- ANCHOR_END: searchDocuments

-- ANCHOR: selectTrail
-- 🧭 The headings from the top down to an entry: from the entry up to
--    its root, as in SQLite's.
-- 上から、その項目までの見出し。項目から根へたどる。SQLite と同じ
selectTrail :: Statement EntryId [Text]
selectTrail = preparable sql (param entryNo) (D.rowList (column D.text))
  where
    sql = """
      WITH RECURSIVE up (id, parent_id, heading, depth) AS
        ( SELECT id, parent_id, heading, 0
            FROM entries WHERE id = $1
          UNION ALL
          SELECT e.id, e.parent_id, e.heading, up.depth + 1
            FROM entries AS e
            JOIN up ON e.id = up.parent_id
        )
      SELECT heading FROM up ORDER BY depth DESC
      """
-- ANCHOR_END: selectTrail

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
  , -- 2: my own attributes and states (jsonb), and their definitions
    -- 2: 自分で決めた属性と状態（jsonb）と、その定義
    """
    ALTER TABLE works
      ADD COLUMN attrs  jsonb NOT NULL DEFAULT '{}',
      ADD COLUMN states jsonb NOT NULL DEFAULT '{}';
    CREATE TABLE definitions
      ( id   bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY
      , name text   NOT NULL UNIQUE
      , what text   NOT NULL CHECK (what IN ('attr', 'flow'))
      , body jsonb  NOT NULL
      );
    """
    -- ANCHOR: migration3
  , -- 3: the texts of the entries, PGroonga's indexes, and everything to
    --    search, as a view
    -- 3: 項目の本文、PGroonga の索引、探すものの全部（ビューで）
    """
    CREATE EXTENSION IF NOT EXISTS pgroonga;
    CREATE TABLE texts
      ( entry_id bigint PRIMARY KEY REFERENCES entries ON DELETE CASCADE
      , work_id  bigint NOT NULL    REFERENCES works   ON DELETE CASCADE
      , body     text   NOT NULL
      );
    CREATE INDEX works_title_search   ON works   USING pgroonga (title);
    CREATE INDEX works_creator_search ON works   USING pgroonga (creator);
    CREATE INDEX entries_search       ON entries USING pgroonga (heading);
    CREATE INDEX notes_search         ON notes   USING pgroonga (body);
    CREATE INDEX texts_search         ON texts   USING pgroonga (body);
    CREATE VIEW documents AS
      SELECT id AS work_id, 'title' AS place, NULL::bigint AS entry_id,
             false AS spoiler, title AS body,
             pgroonga_score(tableoid, ctid) AS score
        FROM works
      UNION ALL
      SELECT id, 'creator', NULL, false, creator,
             pgroonga_score(tableoid, ctid)
        FROM works
      UNION ALL
      SELECT work_id, 'heading', id, false, heading,
             pgroonga_score(tableoid, ctid)
        FROM entries
      UNION ALL
      SELECT work_id, 'note', entry_id, spoiler, body,
             pgroonga_score(tableoid, ctid)
        FROM notes
      UNION ALL
      SELECT work_id, 'text', entry_id, false, body,
             pgroonga_score(tableoid, ctid)
        FROM texts;
    """
    -- ANCHOR_END: migration3
    -- ANCHOR: migration4
  , -- 4: the tags of each work (a jsonb array of words)
    -- 4: 作品ごとのタグ（言葉の jsonb の配列）
    """
    ALTER TABLE works ADD COLUMN tags jsonb NOT NULL DEFAULT '[]';
    """
    -- ANCHOR_END: migration4
  ]

-- 🪜 Run the steps not run yet. PostgreSQL has no user_version, so we
--    keep the number in a small table of our own. A script of several
--    statements sent at once runs as one transaction.
-- まだ動かしていない手順を動かす。PostgreSQL には user_version がないので、
-- 番号は、自分で作った小さな表にしまう。いくつもの文を1回で送った
-- スクリプトは、1つのトランザクションで動く
migrate :: Connection -> IO ()
migrate conn = do
  run (Session.script
    "CREATE TABLE IF NOT EXISTS schema_version (version bigint NOT NULL)")
  done <- run (Session.statement () currentVersion)
  forM_ (zip [1 ..] migrations) $ \(n, step) ->
    when (n > done) $ run (Session.script (step <> record n))
  where
    run :: Session.Session a -> IO a
    run session =
      Connection.use conn session >>= either (throwIO . Failed) pure
    record n    = "INSERT INTO schema_version (version) VALUES ("
                    <> T.pack (show (n :: Int)) <> ");"

currentVersion :: Statement () Int
currentVersion = preparable
  "SELECT coalesce(max(version), 0) FROM schema_version"
  mempty
  (D.singleRow (column intColumn))
-- ANCHOR_END: migrations
-- ANCHOR_END: all
