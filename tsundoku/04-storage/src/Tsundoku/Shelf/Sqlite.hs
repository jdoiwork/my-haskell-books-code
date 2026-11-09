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

-- a lock, exceptions, loops, and SQLite
-- 鍵、例外、くり返し、SQLite
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (throwIO)
import Control.Monad (forM_, when)
import Data.String (fromString)
import Data.Traversable (for)
import Data.Tree (Tree (..))
import Database.SQLite.Simple

import Tsundoku.Card qualified as Card
import Tsundoku.Card (Card (..), newCard)
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
  execute_ conn "PRAGMA foreign_keys = ON"
  migrate conn
  lock <- newMVar ()
  let inTx :: IO a -> IO a
      inTx action = withMVar lock (\_ -> withTransaction conn action)
      change wid core write = inTx $ do
        found <- loadCard conn wid
        for (decide wid core found) (\new -> new <$ write new)
  pure Shelf
    { addWork    = \w -> inTx (insertWork conn w)
    , removeWork = \wid -> inTx (deleteWork conn wid)
    , getCard    = \wid -> inTx (loadCard conn wid)
    , listCards  = inTx $ do
        ids <- query_ conn "SELECT id FROM works ORDER BY id"
        concat <$> for ids (\(Only w) -> withId (WorkId w))
    , applyEvent = \wid e -> fmap reading <$>
        change wid (Card.applyEvent e) (updateReading conn wid . reading)
    , addNote    = \wid n -> done <$>
        change wid (Card.addNote n) (\_ -> insertNote conn wid n)
    , logReading = \wid l -> done <$>
        change wid (Card.logReading l) (\_ -> insertLog conn wid l)
    , rate       = \wid s -> done <$>
        change wid (Right . Card.rate s) (\_ -> updateRating conn wid s)
    , rateEntry  = \wid i s -> done <$>
        change wid (Card.rateEntry i s) (\_ -> upsertRating conn wid i s)
    }
  where
    done = fmap (const ())
    withId wid = maybe [] (\card -> [(wid, card)]) <$> loadCard conn wid
-- ANCHOR_END: withSqliteShelf

-- ANCHOR: changes
-- ✏️ The statements that write one change.
-- 変更を1つ書く文
updateReading :: Connection -> WorkId -> ReadingState -> IO ()
updateReading conn (WorkId w) state =
  execute conn "UPDATE works SET reading = ? WHERE id = ?"
    (stateName state, w)

updateRating :: Connection -> WorkId -> Score -> IO ()
updateRating conn (WorkId w) s =
  execute conn "UPDATE works SET rating = ? WHERE id = ?" (scoreValue s, w)

insertNote :: Connection -> WorkId -> Note -> IO ()
insertNote conn (WorkId w) n = execute conn """
  INSERT INTO notes (work_id, entry_id, body, written_at, spoiler)
  VALUES (?, ?, ?, ?, ?)
  """
  (w, targetEntry (target n), body n, writtenAt n, spoiler n)

insertLog :: Connection -> WorkId -> LogLine -> IO ()
insertLog conn (WorkId w) l = execute conn """
  INSERT INTO reading_log (work_id, entry_id, read_on)
  VALUES (?, ?, ?)
  """
  (w, i, readOn l)
  where
    EntryId i = readEntry l

upsertRating :: Connection -> WorkId -> EntryId -> Score -> IO ()
upsertRating conn (WorkId w) (EntryId i) s = execute conn """
  INSERT INTO entry_ratings (entry_id, work_id, score) VALUES (?, ?, ?)
  ON CONFLICT (entry_id) DO UPDATE SET score = excluded.score
  """
  (i, w, scoreValue s)

-- 🗑️ Remove a work. Its entries, notes, log, and ratings go with it
--    (ON DELETE CASCADE). Was it there?
-- 作品を消す。目次・感想・ログ・評価も一緒に消える（ON DELETE CASCADE）。
-- その作品はあった？
deleteWork :: Connection -> WorkId -> IO Bool
deleteWork conn (WorkId w) = do
  execute conn "DELETE FROM works WHERE id = ?" (Only w)
  (> 0) <$> changes conn
-- ANCHOR_END: changes

-- ANCHOR: insertWork
-- 📥 Put a new card in the tables: the work first, then its table of
--    contents. SQLite gives every number (RETURNING tells us which).
-- 新しいカードを表にしまう：まず作品、それから目次。番号は全部 SQLite が
-- 振る（どの番号になったかは、RETURNING が教えてくれる）
insertWork :: Connection -> Work -> IO WorkId
insertWork conn w = do
  let (kind, minutes, toc) = toPlain (contents w)
      card                 = newCard w
  wid <- returningId conn """
    INSERT INTO works (title, creator, kind, minutes, reading)
    VALUES (?, ?, ?, ?, ?)
    RETURNING id
    """
    (title w, creator w, kind, minutes, stateName (reading card))
  insertForest conn wid Nothing toc
  pure (WorkId wid)

-- 🌳 Put each tree in, root first: the children need the root's number.
-- 木を1本ずつ、根から入れる。子には、根の番号が要るから
insertForest :: Connection -> Int -> Maybe Int -> Toc Int -> IO ()
insertForest conn wid parent forest =
  forM_ (zip [1 :: Int ..] forest) $ \(position, Node entry children) -> do
    eid <- returningId conn """
      INSERT INTO entries (work_id, parent_id, position, heading, size)
      VALUES (?, ?, ?, ?, ?)
      RETURNING id
      """
      (wid, parent, position, heading entry, size entry)
    insertForest conn wid (Just eid) children

-- 🔢 Run an INSERT ... RETURNING id, and take the number.
-- INSERT ... RETURNING id を動かして、番号を受け取る
returningId :: ToRow q => Connection -> Query -> q -> IO Int
returningId conn sql row = do
  ids <- query conn sql row
  case ids of
    [Only i] -> pure i
    _        -> throwIO (BrokenRow "RETURNING id")
-- ANCHOR_END: insertWork

-- ANCHOR: loadCard
-- 📖 Read a card back from its tables, if the work is there.
-- 作品があれば、表からカードを読み戻す
loadCard :: Connection -> WorkId -> IO (Maybe Card)
loadCard conn (WorkId w) = do
  rows <- query conn """
    SELECT title, creator, kind, minutes, reading, rating
      FROM works WHERE id = ?
    """ (Only w)
  case rows of
    []                       -> pure Nothing
    (t, c, k, m, r, s) : _ -> do
      toc     <- query conn tocQuery (Only w)
      notes'  <- query conn """
        SELECT entry_id, body, written_at, spoiler
          FROM notes WHERE work_id = ? ORDER BY id
        """ (Only w)
      log'    <- query conn """
        SELECT read_on, entry_id
          FROM reading_log WHERE work_id = ? ORDER BY id
        """ (Only w)
      ratings <- query conn
        "SELECT entry_id, score FROM entry_ratings WHERE work_id = ?"
        (Only w)
      either throwIO (pure . Just)
        (assemble (WorkRow t c k m r s) toc notes' log' ratings)
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
  ]

-- 🪜 Run the steps the file has not run yet. SQLite keeps a number of
--    our own in the file: PRAGMA user_version (0 for a new file).
-- ファイルがまだ動かしていない手順を動かす。SQLite は、自由に使える
-- 番号を1つ、ファイルにしまえる：PRAGMA user_version（新しいファイルは 0）
migrate :: Connection -> IO ()
migrate conn = do
  [Only done] <- query_ conn "PRAGMA user_version"
  forM_ (zip [1 :: Int ..] migrations) $ \(n, step) ->
    when (n > done) $ withTransaction conn $ do
      mapM_ (execute_ conn) step
      execute_ conn (fromString ("PRAGMA user_version = " <> show n))
-- ANCHOR_END: migrations
-- ANCHOR_END: all
