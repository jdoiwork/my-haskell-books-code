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
  let -- One change at a time, in one transaction.
      -- 変更は1つずつ、1つのトランザクションで
      inTx :: IO a -> IO a
      inTx action = withMVar lock (\_ -> withTransaction conn action)
      -- Give a job the connection, in a transaction.
      -- 仕事に接続を渡して、トランザクションの中で動かす
      run :: (Connection -> IO a) -> IO a
      run job = inTx (job conn)
      -- Read, decide, write. The core decides; write saves the new card.
      -- 読む → 決める → 書く。決めるのは核。write が新しいカードをしまう
      edit  :: WorkId -> (Card -> Either Mistake Card)
            -> (Card -> Connection -> IO ()) -> IO (Either Mistake Card)
      edit wid core write = inTx $ do
        found <- loadCard wid conn
        case decide wid core found of
          Left why  -> pure (Left why)
          Right new -> do
            write new conn
            pure (Right new)
      -- The same, for a write that does not need the new card.
      -- 同じことを、新しいカードを使わない write で
      edit_ :: WorkId -> (Card -> Either Mistake Card)
            -> (Connection -> IO ()) -> IO (Either Mistake ())
      edit_ wid core write = do
        result <- edit wid core (\_ -> write)
        pure (fmap (\_ -> ()) result)
  pure Shelf
    { addWork    = \w       -> run (insertWork w)
    , removeWork = \wid     -> run (deleteWork wid)
    , getCard    = \wid     -> run (loadCard   wid)
    , listCards  =             run  loadAll
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

-- 📚 Every card on the shelf, in the order the works were added.
-- 本棚のカードを全部、作品を足した順に
loadAll :: Connection -> IO [(WorkId, Card)]
loadAll conn = do
  ids   <- query_ conn "SELECT id FROM works ORDER BY id"
  pairs <- for ids $ \(Only w) -> do
    card <- loadCard (WorkId w) conn
    pure (WorkId w, card)
  pure [ (wid, card) | (wid, Just card) <- pairs ]
-- ANCHOR_END: withSqliteShelf

-- ANCHOR: changes
-- ✏️ The statements that write one change. Like every job the window
--    runs, each takes the connection last, so the window can say only
--    what to write; `run` and `edit` hand it the connection.
-- 変更を1つ書く文。窓口が動かす仕事は、どれも接続を最後の引数に
-- もらう。窓口では「何を書くか」だけを書き、接続は run と edit が渡す
updateReading :: WorkId -> Card -> Connection -> IO ()
updateReading (WorkId w) card conn =
  execute conn "UPDATE works SET reading = ? WHERE id = ?"
    (stateName (reading card), w)

updateRating :: WorkId -> Score -> Connection -> IO ()
updateRating (WorkId w) s conn =
  execute conn "UPDATE works SET rating = ? WHERE id = ?" (scoreValue s, w)

insertNote :: WorkId -> Note -> Connection -> IO ()
insertNote (WorkId w) n conn = execute conn """
  INSERT INTO notes (work_id, entry_id, body, written_at, spoiler)
  VALUES (?, ?, ?, ?, ?)
  """
  (w, targetEntry (target n), body n, writtenAt n, spoiler n)

insertLog :: WorkId -> LogLine -> Connection -> IO ()
insertLog (WorkId w) l conn = execute conn """
  INSERT INTO reading_log (work_id, entry_id, read_on)
  VALUES (?, ?, ?)
  """
  (w, i, readOn l)
  where
    EntryId i = readEntry l

upsertRating :: WorkId -> EntryId -> Score -> Connection -> IO ()
upsertRating (WorkId w) (EntryId i) s conn = execute conn """
  INSERT INTO entry_ratings (entry_id, work_id, score) VALUES (?, ?, ?)
  ON CONFLICT (entry_id) DO UPDATE SET score = excluded.score
  """
  (i, w, scoreValue s)

-- 🗑️ Remove a work. Its entries, notes, log, and ratings go with it
--    (ON DELETE CASCADE). Was it there?
-- 作品を消す。目次・感想・ログ・評価も一緒に消える（ON DELETE CASCADE）。
-- その作品はあった？
deleteWork :: WorkId -> Connection -> IO Bool
deleteWork (WorkId w) conn = do
  execute conn "DELETE FROM works WHERE id = ?" (Only w)
  (> 0) <$> changes conn
-- ANCHOR_END: changes

-- ANCHOR: insertWork
-- 📥 Put a new card in the tables: the work first, then its table of
--    contents. SQLite gives every number (RETURNING tells us which).
-- 新しいカードを表にしまう：まず作品、それから目次。番号は全部 SQLite が
-- 振る（どの番号になったかは、RETURNING が教えてくれる）
insertWork :: Work -> Connection -> IO WorkId
insertWork w conn = do
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
loadCard :: WorkId -> Connection -> IO (Maybe Card)
loadCard (WorkId w) conn = do
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
