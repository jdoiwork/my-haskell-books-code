-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE MultilineStrings #-}
-- | The shelf in SQLite: one file, no server.
-- SQLite の本棚：ファイル1つ。サーバーは要らない
module Tsundoku.Shelf.Sqlite
  ( withSqliteShelf
  , sqliteShelf
  , backupSqlite
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
import Data.Aeson (Value, toJSON)
import Data.String (fromString)
import Data.Set (Set)
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
import Tsundoku.Search
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

-- ANCHOR: backup
-- 💾 Copy the whole shelf into a new file, safely even while it is in
--    use (SQLite's VACUUM INTO). The new file must not exist yet.
-- 本棚の全部を、新しいファイルに写す。使っている最中でも安全に
-- （SQLite の VACUUM INTO）。写す先のファイルは、まだあってはいけない
backupSqlite :: FilePath -> FilePath -> IO ()
backupSqlite path to =
  withConnection path $ \conn -> SQLite.execute conn "VACUUM INTO ?" (Only to)
-- ANCHOR_END: backup

-- 🪟 The shelf on an open connection. One change at a time, and each
--    change in one transaction: all of it, or none of it.
-- 開いた接続の上の本棚。変更は1つずつ、それぞれ1つのトランザクションで
-- （全部か、何もなしか）
sqliteShelf :: Connection -> IO Shelf
sqliteShelf conn = do
  runReaderT (execute_ "PRAGMA foreign_keys = ON") conn
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
      -- ANCHOR: sqliteRevise
      -- A work fixed by hand: new lines take numbers from the next one
      -- SQLite would give, as the new episodes do.
      -- 手で直した作品：新しい行は、SQLite が次に振る番号から取る
      -- （新しい話と同じ）
      revise :: WorkId -> Revision -> IO (Checked ())
      revise wid r = do
        result <- run $ do
          from <- nextEntryId
          change wid (Card.revise from r)
                     (writeRevision wid (notesOnRemoved r))
        pure (fmap (\_ -> ()) result)
      -- ANCHOR_END: sqliteRevise
  pure Shelf
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

-- 🔒 Run a job in a transaction: all of it, or none of it.
-- 仕事をトランザクションの中で動かす。全部か、何もなしか
transaction :: Db a -> Db a
transaction job = do
  conn <- ask
  liftIO (SQLite.withTransaction conn (runReaderT job conn))
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

-- ANCHOR: updateTags
-- 🔖 Write the tags, as JSON text.
-- タグを、JSON の文字で書く
updateTags :: WorkId -> Set Text -> Db ()
updateTags (WorkId w) ts =
  execute "UPDATE works SET tags = ? WHERE id = ?" (jsonText (tagsJson ts), w)
-- ANCHOR_END: updateTags

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

-- ANCHOR: writeRevision
-- 🔢 The number SQLite would give the next entry.
-- SQLite が次の項目に振る番号
nextEntryId :: Db EntryId
nextEntryId = do
  rows <- query_ "SELECT seq FROM sqlite_sequence WHERE name = 'entries'"
  pure $ case rows of
    [Only s] -> EntryId (s + 1)
    _        -> EntryId 1

-- ✏️ Write a work fixed by hand. The order matters:
--    1. notes on lines that are gone move to the whole work (or else
--       ON DELETE CASCADE would take them away), or are deleted, as
--       chosen;
--    2. every line is changed or put in, from the top (a parent before
--       its children);
--    3. the lines that are gone are deleted; their ratings, log, and
--       texts go with them (ON DELETE CASCADE).
-- 手で直した作品を書く。順番が大事：
--    1. 消える行の感想を、選んだとおりに、作品全体へ移す（でないと
--       ON DELETE CASCADE に消されてしまう）か、消す
--    2. どの行も、上から順に書き換えるか、足す（親が子より先）
--    3. 消える行を消す。その★・ログ・本文も一緒に消える（ON DELETE CASCADE）
writeRevision :: WorkId -> NotesOnRemoved -> Card -> Db ()
writeRevision (WorkId w) choice card = do
  execute "UPDATE works SET title = ?, creator = ? WHERE id = ?"
    (title (work card), creator (work card), w)
  before <- query "SELECT id FROM entries WHERE work_id = ?" (Only w)
  let had  = [ i | Only i <- before ]
      now  = [ number (entryId e) | (_, _, e) <- placed toc ]
      gone = [ i | i <- had, i `notElem` now ]
  forM_ gone $ \i -> case choice of
    MoveToWork ->
      execute "UPDATE notes SET entry_id = NULL WHERE entry_id = ?" (Only i)
    DropNotes  ->
      execute "DELETE FROM notes WHERE entry_id = ?" (Only i)
  forM_ (placed toc) $ \(parent, position, e) ->
    if number (entryId e) `elem` had
      then execute """
        UPDATE entries SET parent_id = ?, position = ?, heading = ?, size = ?
         WHERE id = ?
        """
        (fmap number parent, position, heading e, size e, number (entryId e))
      else execute """
        INSERT INTO entries (id, work_id, parent_id, position, heading, size)
        VALUES (?, ?, ?, ?, ?, ?)
        """
        (number (entryId e), w, fmap number parent, position, heading e
        , size e)
  forM_ gone $ \i -> execute "DELETE FROM entries WHERE id = ?" (Only i)
  where
    (_, _, toc)        = toPlain (contents (work card))
    number (EntryId i) = i
-- ANCHOR_END: writeRevision

-- ANCHOR: upsertText
-- 📝 Keep the text of an entry. A new text replaces the old one.
-- 項目の本文をしまう。新しい本文は、古いものと入れ替える
upsertText :: WorkId -> EntryId -> Text -> Db ()
upsertText (WorkId w) (EntryId i) text = execute """
  INSERT INTO texts (entry_id, work_id, body) VALUES (?, ?, ?)
  ON CONFLICT (entry_id) DO UPDATE SET body = excluded.body
  """
  (i, w, text)
-- ANCHOR_END: upsertText

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
    SELECT title, creator, kind, minutes, reading, rating, attrs, states,
           tags
      FROM works WHERE id = ?
    """ (Only w)
  case rows of
    []                               -> pure Nothing
    (t, c, k, m, r, s, a, st, tg) : _ -> do
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
        tg' <- fromJsonText tg
        let row = WorkRow t c k m r s a' st' tg'
        assemble schema row toc notes' log' ratings
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

-- ANCHOR: searchDocuments
-- 🔎 Search the documents. The long words go to MATCH, and the order is
--    FTS5's rank (bm25). The short words are looked for with instr, in
--    every row MATCH let through. MATCH cannot be put inside an OR, so
--    with no long words, it is another query.
-- documents を探す。長い語は MATCH に渡し、順番は FTS5 の rank（bm25）。
-- 短い語は、MATCH を通った行の中を instr で探す。MATCH は OR の中に
-- 書けないので、長い語がなければ、別の問い合わせにする
searchDocuments :: Search -> Db [Hit]
searchDocuments s = do
  rows <- case ftsQuery s of
    Just long -> query byRank  (long, short)
    Nothing   -> query inOrder (Only short)
  for rows $ \(w, workTitle, name, entry, spoils, text) -> do
    found <- either (liftIO . throwIO) pure (placeOf name entry spoils)
    path  <- maybe (pure []) trailOf (entryOf found)
    pure (Hit (WorkId w) workTitle found path (snippetOf s text))
  where
    -- the short words, as a JSON array for json_each
    -- 短い語を、json_each に渡す JSON の配列に
    short = jsonText (toJSON (shortWords s))

byRank :: Query
byRank = """
  SELECT work_id, (SELECT title FROM works WHERE id = work_id),
         place, entry_id, spoiler, body
    FROM documents
   WHERE documents MATCH ?
     AND NOT EXISTS (SELECT 1 FROM json_each(?)
                      WHERE instr(lower(body), lower(value)) = 0)
   ORDER BY rank
  """

inOrder :: Query
inOrder = """
  SELECT work_id, (SELECT title FROM works WHERE id = work_id),
         place, entry_id, spoiler, body
    FROM documents
   WHERE NOT EXISTS (SELECT 1 FROM json_each(?)
                      WHERE instr(lower(body), lower(value)) = 0)
   ORDER BY rowid
  """
-- ANCHOR_END: searchDocuments

-- ANCHOR: trailOf
-- 🧭 The headings from the top down to an entry. The same WITH
--    RECURSIVE as tocQuery, walking the other way: from the entry up to
--    its root, one parent at a time.
-- 上から、その項目までの見出し。tocQuery と同じ WITH RECURSIVE を、
-- 反対向きに歩く：項目から根へ、親を1つずつたどる
trailOf :: EntryId -> Db [Text]
trailOf (EntryId i) = do
  rows <- query """
    WITH RECURSIVE up (id, parent_id, heading, depth) AS
      ( SELECT id, parent_id, heading, 0
          FROM entries WHERE id = ?
        UNION ALL
        SELECT e.id, e.parent_id, e.heading, up.depth + 1
          FROM entries AS e
          JOIN up ON e.id = up.parent_id
      )
    SELECT heading FROM up ORDER BY depth DESC
    """ (Only i)
  pure (map fromOnly rows)
-- ANCHOR_END: trailOf

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
    -- ANCHOR: migration3
  , -- 3: the texts of the entries, and everything to search, in FTS5
    -- 3: 項目の本文と、探すものの全部を FTS5 に
    [ """
      CREATE TABLE texts
        ( entry_id INTEGER PRIMARY KEY REFERENCES entries ON DELETE CASCADE
        , work_id  INTEGER NOT NULL    REFERENCES works   ON DELETE CASCADE
        , body     TEXT    NOT NULL
        )
      """
    , """
      CREATE VIRTUAL TABLE documents USING fts5
        ( body
        , work_id  UNINDEXED
        , place    UNINDEXED
        , entry_id UNINDEXED
        , spoiler  UNINDEXED
        , tokenize = 'trigram'
        )
      """
      -- what is there already (a file from the earlier chapters)
      -- もうあるもの（前の章までのファイル）
    , """
      INSERT INTO documents (body, work_id, place, entry_id, spoiler)
        SELECT title,   id,      'title',   NULL,     0       FROM works
        UNION ALL
        SELECT creator, id,      'creator', NULL,     0       FROM works
        UNION ALL
        SELECT heading, work_id, 'heading', id,       0       FROM entries
        UNION ALL
        SELECT body,    work_id, 'note',    entry_id, spoiler FROM notes
      """
      -- and from now on, the triggers keep documents in step
      -- これからは、トリガーが documents を追いかける
    , """
      CREATE TRIGGER work_added AFTER INSERT ON works BEGIN
        INSERT INTO documents (body, work_id, place, entry_id, spoiler)
        VALUES (new.title,   new.id, 'title',   NULL, 0),
               (new.creator, new.id, 'creator', NULL, 0);
      END
      """
    , """
      CREATE TRIGGER work_removed AFTER DELETE ON works BEGIN
        DELETE FROM documents WHERE work_id = old.id;
      END
      """
    , """
      CREATE TRIGGER entry_added AFTER INSERT ON entries BEGIN
        INSERT INTO documents (body, work_id, place, entry_id, spoiler)
        VALUES (new.heading, new.work_id, 'heading', new.id, 0);
      END
      """
    , """
      CREATE TRIGGER note_added AFTER INSERT ON notes BEGIN
        INSERT INTO documents (body, work_id, place, entry_id, spoiler)
        VALUES (new.body, new.work_id, 'note', new.entry_id, new.spoiler);
      END
      """
    , """
      CREATE TRIGGER text_added AFTER INSERT ON texts BEGIN
        INSERT INTO documents (body, work_id, place, entry_id, spoiler)
        VALUES (new.body, new.work_id, 'text', new.entry_id, 0);
      END
      """
    , """
      CREATE TRIGGER text_changed AFTER UPDATE ON texts BEGIN
        DELETE FROM documents
         WHERE place = 'text' AND entry_id = old.entry_id;
        INSERT INTO documents (body, work_id, place, entry_id, spoiler)
        VALUES (new.body, new.work_id, 'text', new.entry_id, 0);
      END
      """
    ]
    -- ANCHOR_END: migration3
    -- ANCHOR: migration4
  , -- 4: the tags of each work (a JSON array of words)
    -- 4: 作品ごとのタグ（言葉の JSON の配列）
    [ """
      ALTER TABLE works ADD COLUMN
        tags TEXT NOT NULL DEFAULT '[]' CHECK (json_valid(tags))
      """
    ]
    -- ANCHOR_END: migration4
    -- ANCHOR: migration5
  , -- 5: works and entries can now be fixed by hand. documents follows
    --    a new title, a new heading, a line that is gone, and a note
    --    that moved to the whole work or was dropped.
    -- 5: 作品と項目を手で直せるようになった。documents は、新しい題、
    --    新しい見出し、消えた行、作品全体へ移った感想と消した感想を
    --    追いかける
    [ """
      CREATE TRIGGER work_renamed AFTER UPDATE OF title, creator ON works
      BEGIN
        DELETE FROM documents
         WHERE work_id = old.id AND place IN ('title', 'creator');
        INSERT INTO documents (body, work_id, place, entry_id, spoiler)
        VALUES (new.title,   new.id, 'title',   NULL, 0),
               (new.creator, new.id, 'creator', NULL, 0);
      END
      """
    , """
      CREATE TRIGGER entry_renamed AFTER UPDATE OF heading ON entries
      BEGIN
        UPDATE documents SET body = new.heading
         WHERE place = 'heading' AND entry_id = old.id;
      END
      """
    , """
      CREATE TRIGGER entry_removed AFTER DELETE ON entries BEGIN
        DELETE FROM documents
         WHERE entry_id = old.id AND place IN ('heading', 'text');
      END
      """
    , """
      CREATE TRIGGER note_moved AFTER UPDATE OF entry_id ON notes BEGIN
        UPDATE documents SET entry_id = new.entry_id
         WHERE place = 'note' AND entry_id = old.entry_id
           AND body = old.body;
      END
      """
    , """
      CREATE TRIGGER note_removed AFTER DELETE ON notes BEGIN
        DELETE FROM documents
         WHERE place = 'note' AND work_id = old.work_id
           AND entry_id IS old.entry_id AND body = old.body;
      END
      """
    ]
    -- ANCHOR_END: migration5
  ]

-- 🪜 Run the steps the file has not run yet. SQLite keeps a number of
--    our own in the file: PRAGMA user_version (0 for a new file).
-- ファイルがまだ動かしていない手順を動かす。SQLite は、自由に使える
-- 番号を1つ、ファイルにしまえる：PRAGMA user_version（新しいファイルは 0）
migrate :: Db ()
migrate = do
  [Only done] <- query_ "PRAGMA user_version"
  forM_ (zip [1 :: Int ..] migrations) $ \(n, step) ->
    when (n > done) $ transaction $ do
      mapM_ execute_ step
      execute_ (fromString ("PRAGMA user_version = " <> show n))
-- ANCHOR_END: migrations
-- ANCHOR_END: all
