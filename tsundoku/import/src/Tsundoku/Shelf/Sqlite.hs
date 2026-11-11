-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE MultilineStrings #-}
-- | The shelf in SQLite: one file, no server.
-- SQLite の本棚：ファイル1つ。サーバーは要らない
module Tsundoku.Shelf.Sqlite
  ( withSqliteShelf
  , sqliteShelf
  , migrations
  ) where

-- a lock, exceptions, loops, JSON, and a job that can ask for the
-- connection
-- 鍵、例外、くり返し、JSON、接続を取り出せる仕事
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (throwIO)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Value)
import Data.String (fromString)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Tree (Tree (..))

-- SQLite (its query and execute are wrapped below, so they are qualified)
-- SQLite（query や execute は下で包み直すので、qualified で使う）
import Database.SQLite.Simple
  (Connection, FromRow, Only (..), Query, ToRow, withConnection)
import Database.SQLite.Simple qualified as SQLite

import Tsundoku.Card qualified as Card
import Tsundoku.Card (Card (..), newCard)
import Tsundoku.Custom qualified as Custom
import Tsundoku.Query (search)
import Tsundoku.Rating (Score, scoreValue)
import Tsundoku.Shelf
import Tsundoku.Shelf.Rows
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: withSqliteShelf
-- 🪶 Open the file, and lend the shelf to the action.
-- ファイルを開いて、本棚を仕事に貸す
withSqliteShelf :: FilePath -> (Shelf -> IO a) -> IO a
withSqliteShelf path action =
  withConnection path $ \conn -> sqliteShelf conn >>= action

-- 🪟 The shelf on an open connection. One change at a time, and each
--    change in one transaction: all of it, or none of it.
-- 開いた接続の上の本棚。変更は1つずつ、それぞれ1つのトランザクションで
-- （全部か、何もなしか）
sqliteShelf :: Connection -> IO Shelf
sqliteShelf conn = do
  runReaderT (execute_ "PRAGMA foreign_keys = ON") conn
  -- Another program writing the same file: wait for it, up to 5 seconds.
  -- 同じファイルに、ほかのプログラムが書いている間は、5秒まで待つ
  runReaderT (execute_ "PRAGMA busy_timeout = 5000") conn
  runReaderT migrate conn
  lock <- newMVar ()
  let -- Run a job: one at a time, in one transaction, on this connection.
      -- 仕事を動かす。1つずつ、1つのトランザクションで、この接続で
      run :: Db a -> IO a
      run job = withMVar lock (\_ -> runReaderT (transaction job) conn)
      -- Read, decide, write, as one job (change, below).
      -- 読む → 決める → 書く を、1つの仕事で（下の change）
      edit  :: WorkId -> (Card -> Checked Card)
            -> (Card -> Db ()) -> IO (Checked Card)
      edit wid core write = run (change wid core write)
      -- The same, for a write that does not need the new card.
      -- 同じことを、新しいカードを使わない write で
      edit_ :: WorkId -> (Card -> Checked Card)
            -> Db () -> IO (Checked ())
      edit_ wid core write = do
        result <- edit wid core (\_ -> write)
        pure (fmap (\_ -> ()) result)
      -- The same, for a core that reads the definitions too. They are
      -- read in the same transaction as the card.
      -- 同じことを、定義も読む核で。定義は、カードと同じ
      -- トランザクションで読む
      custom :: WorkId -> (Schema -> Card -> Checked Card)
             -> (Card -> Db ()) -> IO (Checked ())
      custom wid core write = do
        result <- run $ do
          schema <- loadSchema
          change wid (core schema) write
        pure (fmap (\_ -> ()) result)
      -- Add a definition, if the core says yes.
      -- 核が「よし」と言えば、定義を足す
      define :: (Schema -> Checked Schema) -> Db () -> IO (Checked ())
      define core write = run $ do
        schema <- loadSchema
        case core schema of
          Left why -> pure (Left why)
          Right _  -> do
            write
            pure (Right ())
      -- ANCHOR: sqliteGrow
      -- New episodes: SQLite gives the numbers, the core grows the table
      -- with them, and the new entries go in with those numbers.
      -- 新しい話：番号は SQLite が出し、核がその番号で目次を育て、
      -- 新しい項目をその番号のまま書く
      grow :: WorkId -> Int -> IO (Checked ())
      grow wid n = do
        result <- run $ do
          fresh <- reserveEntries n
          change wid (Card.addEpisodes fresh) (insertGrown wid fresh)
        pure (fmap (\_ -> ()) result)
      -- ANCHOR_END: sqliteGrow
  pure Shelf
    { -- 📚 The works on the shelf
      -- 本棚の作品
      addWork     = \w       -> run (insertWork w)
    , removeWork  = \wid     -> run (deleteWork wid)
    , getCard     = \wid     -> run (loadCard   wid)
    , listCards   =             run  loadAll

      -- 📖 Reading, notes, and ratings
      -- 読む・感想・評価
    , applyEvent  = \wid e   -> fmap reading <$>
        edit  wid (Card.applyEvent e)   (updateReading wid)
    , addNote     = \wid n   ->
        edit_ wid (Card.addNote n)      (insertNote    wid n)
    , logReading  = \wid l   ->
        edit_ wid (Card.logReading l)   (insertLog     wid l)
    , rate        = \wid s   ->
        edit_ wid (Right . Card.rate s) (updateRating  wid s)
    , rateEntry   = \wid i s ->
        edit_ wid (Card.rateEntry i s)  (upsertRating  wid i s)

      -- 🏷️ My own attributes and states
      -- 自分で決める属性と状態
    , getSchema   =             run  loadSchema
    , defineAttr  = \d       ->
        define (Custom.defineAttr d)    (insertDefinition (Left  d))
    , defineFlow  = \f       ->
        define (Custom.defineFlow f)    (insertDefinition (Right f))
    , editAttrs   = \wid t   ->
        custom wid (Card.editAttrs t)   (updateAttrs  wid)
    , moveFlow    = \wid f s ->
        custom wid (Card.moveFlow f s)  (updateStates wid)
    , findCards   = \q       -> run (searchAll q)

      -- ANCHOR: sqliteAdd
      -- 📥 Taking things in from outside
      -- 外から取り込む
    , addEpisodes = grow
      -- ANCHOR_END: sqliteAdd
    }

-- 🔁 Read, decide, write: one change to one card, as a job.
-- 読む → 決める → 書く：カード1枚の変更を、1つの仕事で
change :: WorkId -> (Card -> Checked Card) -> (Card -> Db ())
       -> Db (Checked Card)
change wid core write = do
  found <- loadCard wid
  case decide wid core found of
    Left why  -> pure (Left why)
    Right new -> do
      write new
      pure (Right new)

-- 🔎 Search every card on the shelf. The core picks them.
-- 本棚のカードを全部、絞り込む。選ぶのは核
searchAll :: Text -> Db (Checked [(WorkId, Card)])
searchAll typed = do
  schema <- loadSchema
  cards  <- loadAll
  pure (search schema typed cards)

-- 📚 Every card on the shelf, in the order the works were added.
-- 本棚のカードを全部、作品を足した順に
loadAll :: Db [(WorkId, Card)]
loadAll = do
  ids   <- query_ "SELECT id FROM works ORDER BY id"
  pairs <- for ids $ \(Only w) -> do
    card <- loadCard (WorkId w)
    pure (WorkId w, card)
  pure [ (wid, card) | (wid, Just card) <- pairs ]
-- ANCHOR_END: withSqliteShelf

-- ANCHOR: db
-- 🧵 A job on the shelf's database. It can ask for the connection
--    whenever it needs it, so no function has to pass it on.
-- 本棚のデータベースでする仕事。接続は要るときに ask で取り出せるので、
-- 関数から関数へ渡さなくてよい
type Db = ReaderT Connection IO

-- 🔌 sqlite-simple's functions, taking the connection from Db.
-- sqlite-simple の関数を、接続を Db から取る形に包み直す
query :: (ToRow q, FromRow r) => Query -> q -> Db [r]
query sql args = do
  conn <- ask
  liftIO (SQLite.query conn sql args)

query_ :: FromRow r => Query -> Db [r]
query_ sql = do
  conn <- ask
  liftIO (SQLite.query_ conn sql)

execute :: ToRow q => Query -> q -> Db ()
execute sql args = do
  conn <- ask
  liftIO (SQLite.execute conn sql args)

execute_ :: Query -> Db ()
execute_ sql = do
  conn <- ask
  liftIO (SQLite.execute_ conn sql)

-- How many rows the last statement changed.
-- 最後の文が変えた行の数
changes :: Db Int
changes = do
  conn <- ask
  liftIO (SQLite.changes conn)

-- 🔒 Run a job in a transaction: all of it, or none of it. IMMEDIATE
--    takes the right to write first, so no other program can write
--    between our reading and our writing.
-- 仕事をトランザクションの中で動かす。全部か、何もなしか。IMMEDIATE で、
-- 書く権利を先に取る。読んでから書くまでに、ほかのプログラムが書けない
transaction :: Db a -> Db a
transaction job = do
  conn <- ask
  liftIO (SQLite.withImmediateTransaction conn (runReaderT job conn))
-- ANCHOR_END: db

-- ANCHOR: changes
-- ✏️ The jobs that write one change.
-- 変更を1つ書く仕事
updateReading :: WorkId -> Card -> Db ()
updateReading (WorkId w) card =
  execute "UPDATE works SET reading = ? WHERE id = ?"
    (stateName (reading card), w)

updateRating :: WorkId -> Score -> Db ()
updateRating (WorkId w) s =
  execute "UPDATE works SET rating = ? WHERE id = ?" (scoreValue s, w)

insertNote :: WorkId -> Note -> Db ()
insertNote (WorkId w) n = execute """
  INSERT INTO notes (work_id, entry_id, body, written_at, spoiler)
  VALUES (?, ?, ?, ?, ?)
  """
  (w, targetEntry (target n), body n, writtenAt n, spoiler n)

insertLog :: WorkId -> LogLine -> Db ()
insertLog (WorkId w) l = execute """
  INSERT INTO reading_log (work_id, entry_id, read_on)
  VALUES (?, ?, ?)
  """
  (w, i, readOn l)
  where
    EntryId i = readEntry l

upsertRating :: WorkId -> EntryId -> Score -> Db ()
upsertRating (WorkId w) (EntryId i) s = execute """
  INSERT INTO entry_ratings (entry_id, work_id, score) VALUES (?, ?, ?)
  ON CONFLICT (entry_id) DO UPDATE SET score = excluded.score
  """
  (i, w, scoreValue s)

-- 🏷️ Write my own attributes and states, as JSON text.
-- 自分で決めた属性と状態を、JSON の文字で書く
updateAttrs :: WorkId -> Card -> Db ()
updateAttrs (WorkId w) card =
  execute "UPDATE works SET attrs = ? WHERE id = ?"
    (jsonText (attrsJson (attrs card)), w)

updateStates :: WorkId -> Card -> Db ()
updateStates (WorkId w) card =
  execute "UPDATE works SET states = ? WHERE id = ?"
    (jsonText (statesJson (states card)), w)

-- 📒 Keep a definition, and read them all back in order.
-- 定義をしまう。そして、全部を順に読み戻す
insertDefinition :: Either AttrDef Flow -> Db ()
insertDefinition def =
  execute "INSERT INTO definitions (name, what, body) VALUES (?, ?, ?)"
    (name, what, jsonText body)
  where
    (name, what, body) = definitionRow def

loadSchema :: Db Schema
loadSchema = do
  rows <- query_ "SELECT name, what, body FROM definitions ORDER BY id"
  either (liftIO . throwIO) pure $ do
    parsed <- traverse readBody rows
    schemaFromRows parsed
  where
    readBody :: (Text, Text, Text) -> Either BrokenRow (Text, Text, Value)
    readBody (name, what, body) = do
      json <- fromJsonText body
      pure (name, what, json)

-- ANCHOR: sqliteRows
-- 🔢 The next n numbers SQLite would give an entry. AUTOINCREMENT
--    keeps the largest number it has given in sqlite_sequence.
-- SQLite が項目に次に振る n 個の番号。AUTOINCREMENT は、振った番号の
-- いちばん大きいものを sqlite_sequence に覚えている
reserveEntries :: Int -> Db [EntryId]
reserveEntries n = do
  rows <- query_ "SELECT seq FROM sqlite_sequence WHERE name = 'entries'"
  let used = case rows of
        [Only s] -> s
        _        -> 0
  pure (map EntryId (take n [used + 1 ..]))

-- 🌱 Put the new episodes in, with the numbers we got, under their
--    parents (placed says where each one hangs).
-- 新しい話を、もらった番号のまま、親の下に書く（どこにぶら下がるかは
-- placed が教えてくれる）
insertGrown :: WorkId -> [EntryId] -> Card -> Db ()
insertGrown (WorkId w) fresh card =
  forM_ [ row | row@(_, _, e) <- placed toc, entryId e `elem` fresh ] $
    \(parent, position, e) -> execute """
      INSERT INTO entries (id, work_id, parent_id, position, heading, size)
      VALUES (?, ?, ?, ?, ?, ?)
      """
      (number (entryId e), w, fmap number parent, position, heading e, size e)
  where
    (_, _, toc)        = toPlain (contents (work card))
    number (EntryId i) = i
-- ANCHOR_END: sqliteRows

-- 🗑️ Remove a work. Its entries, notes, log, and ratings go with it
--    (ON DELETE CASCADE). Was it there?
-- 作品を消す。目次・感想・ログ・評価も一緒に消える（ON DELETE CASCADE）。
-- その作品はあった？
deleteWork :: WorkId -> Db Bool
deleteWork (WorkId w) = do
  execute "DELETE FROM works WHERE id = ?" (Only w)
  removed <- changes
  pure (removed > 0)
-- ANCHOR_END: changes

-- ANCHOR: insertWork
-- 📥 Put a new card in the tables: the work first, then its table of
--    contents. SQLite gives every number (RETURNING tells us which).
-- 新しいカードを表にしまう：まず作品、それから目次。番号は全部 SQLite が
-- 振る（どの番号になったかは、RETURNING が教えてくれる）
insertWork :: Work -> Db WorkId
insertWork w = do
  let (kind, minutes, toc) = toPlain (contents w)
      card                 = newCard w
  wid <- returningId """
    INSERT INTO works (title, creator, kind, minutes, reading)
    VALUES (?, ?, ?, ?, ?)
    RETURNING id
    """
    (title w, creator w, kind, minutes, stateName (reading card))
  insertForest wid Nothing toc
  pure (WorkId wid)

-- 🌳 Put each tree in, root first: the children need the root's number.
-- 木を1本ずつ、根から入れる。子には、根の番号が要るから
insertForest :: Int -> Maybe Int -> Toc Int -> Db ()
insertForest wid parent forest =
  forM_ (zip [1 :: Int ..] forest) $ \(position, Node entry children) -> do
    eid <- returningId """
      INSERT INTO entries (work_id, parent_id, position, heading, size)
      VALUES (?, ?, ?, ?, ?)
      RETURNING id
      """
      (wid, parent, position, heading entry, size entry)
    insertForest wid (Just eid) children

-- 🔢 Run an INSERT ... RETURNING id, and take the number.
-- INSERT ... RETURNING id を動かして、番号を受け取る
returningId :: ToRow q => Query -> q -> Db Int
returningId sql row = do
  ids <- query sql row
  case ids of
    [Only i] -> pure i
    _        -> liftIO (throwIO (BrokenRow "RETURNING id"))
-- ANCHOR_END: insertWork

-- ANCHOR: loadCard
-- 📖 Read a card back from its tables, if the work is there.
-- 作品があれば、表からカードを読み戻す
loadCard :: WorkId -> Db (Maybe Card)
loadCard (WorkId w) = do
  schema <- loadSchema
  rows   <- query """
    SELECT title, creator, kind, minutes, reading, rating, attrs, states
      FROM works WHERE id = ?
    """ (Only w)
  case rows of
    []                               -> pure Nothing
    (t, c, k, m, r, s, a, st) : _ -> do
      toc     <- query tocQuery (Only w)
      notes'  <- query """
        SELECT entry_id, body, written_at, spoiler
          FROM notes WHERE work_id = ? ORDER BY id
        """ (Only w)
      log'    <- query """
        SELECT read_on, entry_id
          FROM reading_log WHERE work_id = ? ORDER BY id
        """ (Only w)
      ratings <- query
        "SELECT entry_id, score FROM entry_ratings WHERE work_id = ?"
        (Only w)
      either (liftIO . throwIO) (pure . Just) $ do
        a'  <- fromJsonText a
        st' <- fromJsonText st
        assemble schema (WorkRow t c k m r s a' st') toc notes' log' ratings
-- ANCHOR_END: loadCard

-- ANCHOR: tocQuery
-- 🌲 The whole table of contents of a work, from top to bottom, with
--    depths. WITH RECURSIVE walks down from the roots. The path of
--    positions, written as text ("000001.000002"), keeps the order.
-- 作品の目次の全部を、上から順に、深さつきで。WITH RECURSIVE が根から
-- 下へ歩く。位置の道順を文字（"000001.000002"）で書いて、順番を守る
tocQuery :: Query
tocQuery = """
  WITH RECURSIVE toc (id, depth, heading, size, path) AS
    ( SELECT id, 0, heading, size, printf('%06d', position)
        FROM entries
       WHERE work_id = ? AND parent_id IS NULL
      UNION ALL
      SELECT e.id, toc.depth + 1, e.heading, e.size,
             toc.path || '.' || printf('%06d', e.position)
        FROM entries AS e
        JOIN toc ON e.parent_id = toc.id
    )
  SELECT id, depth, heading, size FROM toc ORDER BY path
  """
-- ANCHOR_END: tocQuery

-- ANCHOR: migrations
-- 🧱 The steps that build the tables, in order. A new step goes at the
--    end; a step that has run is never changed.
-- 表を作る手順を、順番に。新しい手順は最後に足す。
-- 一度動かした手順は、書き換えない
migrations :: [[Query]]
migrations =
  [ -- 1: works and their tables of contents, notes, log, and ratings
    -- 1: 作品と、その目次・感想・読書ログ・評価
    [ """
      CREATE TABLE works
        ( id      INTEGER PRIMARY KEY AUTOINCREMENT
        , title   TEXT    NOT NULL
        , creator TEXT    NOT NULL
        , kind    TEXT    NOT NULL
            CHECK (kind IN ('book', 'serial', 'series', 'film', 'game'))
        , minutes INTEGER
        , reading TEXT    NOT NULL
            CHECK (reading IN ('Piled', 'Reading', 'Finished', 'Paused'))
        , rating  INTEGER CHECK (rating BETWEEN 0 AND 100)
        )
      """
    , """
      CREATE TABLE entries
        ( id        INTEGER PRIMARY KEY AUTOINCREMENT
        , work_id   INTEGER NOT NULL REFERENCES works   ON DELETE CASCADE
        , parent_id INTEGER          REFERENCES entries ON DELETE CASCADE
        , position  INTEGER NOT NULL
        , heading   TEXT    NOT NULL
        , size      INTEGER NOT NULL CHECK (size >= 0)
        )
      """
    , """
      CREATE TABLE notes
        ( id         INTEGER PRIMARY KEY
        , work_id    INTEGER NOT NULL REFERENCES works   ON DELETE CASCADE
        , entry_id   INTEGER          REFERENCES entries ON DELETE CASCADE
        , body       TEXT    NOT NULL
        , written_at TEXT    NOT NULL
        , spoiler    INTEGER NOT NULL
        )
      """
    , """
      CREATE TABLE reading_log
        ( id       INTEGER PRIMARY KEY
        , work_id  INTEGER NOT NULL REFERENCES works   ON DELETE CASCADE
        , entry_id INTEGER NOT NULL REFERENCES entries ON DELETE CASCADE
        , read_on  TEXT    NOT NULL
        )
      """
    , """
      CREATE TABLE entry_ratings
        ( entry_id INTEGER PRIMARY KEY REFERENCES entries ON DELETE CASCADE
        , work_id  INTEGER NOT NULL    REFERENCES works   ON DELETE CASCADE
        , score    INTEGER NOT NULL    CHECK (score BETWEEN 0 AND 100)
        )
      """
    ]
  , -- 2: my own attributes and states (JSON), and their definitions
    -- 2: 自分で決めた属性と状態（JSON）と、その定義
    [ """
      ALTER TABLE works ADD COLUMN
        attrs  TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(attrs))
      """
    , """
      ALTER TABLE works ADD COLUMN
        states TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(states))
      """
    , """
      CREATE TABLE definitions
        ( id   INTEGER PRIMARY KEY
        , name TEXT    NOT NULL UNIQUE
        , what TEXT    NOT NULL CHECK (what IN ('attr', 'flow'))
        , body TEXT    NOT NULL CHECK (json_valid(body))
        )
      """
    ]
  ]

-- 🪜 Run the steps the file has not run yet. SQLite keeps a number of
--    our own in the file: PRAGMA user_version (0 for a new file).
--    Everything, from reading the number on, is one IMMEDIATE
--    transaction: when two programs open a new file at once, the second
--    waits, reads the number the first wrote, and runs no step twice.
-- ファイルがまだ動かしていない手順を動かす。SQLite は、自由に使える
-- 番号を1つ、ファイルにしまえる：PRAGMA user_version（新しいファイルは 0）。
-- 番号を読むところから全部を、1つの IMMEDIATE のトランザクションで。
-- 2つのプログラムが同時に新しいファイルを開いても、後のほうは待って、
-- 先のほうが書いた番号を読むので、同じ手順を2回動かさない
migrate :: Db ()
migrate = transaction $ do
  [Only done] <- query_ "PRAGMA user_version"
  forM_ (zip [1 :: Int ..] migrations) $ \(n, step) ->
    when (n > done) $ do
      mapM_ execute_ step
      execute_ (fromString ("PRAGMA user_version = " <> show n))
-- ANCHOR_END: migrations
-- ANCHOR_END: all
