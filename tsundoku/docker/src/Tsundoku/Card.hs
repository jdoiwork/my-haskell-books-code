-- ANCHOR: all
-- | A reading card: one work and everything I keep about it.
--   The pure core: functions that take a card and give a new one.
-- 読書カード：作品1つと、それについて書きためたもの全部。
-- 純粋な核：カードを受け取って、新しいカードを返す関数たち
-- ANCHOR: header
module Tsundoku.Card
  ( Card (..)
  , newCard
  , addNote
  , applyEvent
  , rate
  , visibleTo
  , logReading
  , unread
  , rateEntry
  , ratingsOf
  , editAttrs
  , stateOf
  , moveFlow
  , episodesIn
  , addEpisodes
  , hasEntry
  , checkWork
  , setTags
  , readTags
  , tagLimit
  , tagLength
  , titleLimit
  , sizeLimit
  , revise
  , asOpened
  , asRevision
  , newItems
  , followTable
  ) where

-- `when`, dictionaries, Maybe, and text
-- when、辞書、Maybe、文字列
import Control.Monad (when)
import Data.Char (isControl)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (mapAccumL)
import Data.Tree (Forest, flatten)

import Tsundoku.Custom (findFlow, nextStates, readFields)
import Tsundoku.Progress (growWith)
import Tsundoku.Rating (Score)
import Tsundoku.Reading (advance)
import Tsundoku.TocCheck (shapeProblems)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: card
-- 🗂️ One card of the library: the work, where I am, my rating of
--    the whole work, my notes, my reading log (both oldest first),
--    my ratings of single entries, like episodes, and the values of
--    my own attributes and states (by their names).
-- 図書室の読書カード1枚：作品、いまの状態、作品全体の評価、感想、
-- 読書ログ（感想とログは古い順）、目次の項目（話など）ごとの評価、
-- 自分で決めた属性の値と状態（名前で引く）
data Card = Card
  { work         :: Work
  , reading      :: ReadingState
  , rating       :: Maybe Score
  , notes        :: [Note]
  , readLog      :: [LogLine]
  , entryRatings :: Map EntryId Score
  , attrs        :: Map Text AttrValue
  , states       :: Map Text Text
  , tags         :: Set Text
  }
  deriving (Show, Eq)

-- 🆕 A new card starts on the pile, with no rating, no notes,
--    nothing in the log, and none of my own attributes.
-- 新しいカードは、積読から。評価も感想も、ログも、自分で決めた属性も
-- まだない
newCard :: Work -> Card
newCard w = Card
  { work         = w
  , reading      = Piled
  , rating       = Nothing
  , notes        = []
  , readLog      = []
  , entryRatings = Map.empty
  , attrs        = Map.empty
  , states       = Map.empty
  , tags         = Set.empty
  }
-- ANCHOR_END: card

-- ANCHOR: addNote
-- ✍️ Add a note, if it says something and is about something real.
-- 感想を足す。中身があって、ちゃんとある所に付くときだけ
addNote :: Note -> Card -> Checked Card
addNote note card = do
  when (T.null (T.strip (body note))) (Left EmptyNote)
  checkTarget (contents (work card)) (target note)
  pure card { notes = notes card ++ [note] }

-- 🎯 The whole work is always there; an entry must be in the table.
-- 作品全体は、いつでもある。目次の項目は、目次になければならない
checkTarget :: Contents -> Target -> Checked ()
checkTarget _    OnWork = Right ()
checkTarget what (OnEntry i)
  | i `elem` entryIds what = Right ()
  | otherwise              = Left (NoSuchEntry i)
-- ANCHOR_END: addNote

-- ANCHOR: hasEntry
-- 📍 The card as it is, if the entry is in its table. To keep a text
--    for an entry, the entry must be there.
-- 目次にその項目があれば、カードをそのまま返す。項目の本文をしまうには、
-- その項目がなければならない
hasEntry :: EntryId -> Card -> Checked Card
hasEntry i card = do
  checkTarget (contents (work card)) (OnEntry i)
  pure card
-- ANCHOR_END: hasEntry

-- ANCHOR: applyEvent
-- 🔔 Something happened: move the card on the map of reading.
-- できごとが起きた：カードを、読書の地図の上で進める
applyEvent :: Event -> Card -> Checked Card
applyEvent event card = do
  next <- advance event (reading card)
  pure card { reading = next }

-- ⭐ Rate the work (a new rating replaces the old one). Always a
--    score: how it was typed in, in stars or out of ten, is forgotten.
-- 評価を付ける（付け直したら、新しいほうにする）。いつも点で持つ。
-- ★で打ったか、10点満点で打ったかは、覚えない
rate :: Score -> Card -> Card
rate r card = card { rating = Just r }
-- ANCHOR_END: applyEvent

-- ANCHOR: visibleTo
-- 🙈 The notes someone may see: spoilers only after the end.
-- その人に見せてよい感想：ネタバレは、読み終えてから
visibleTo :: ReadingState -> [Note] -> [Note]
visibleTo state allNotes
  | state == Finished = allNotes
  | otherwise         = [ note | note <- allNotes, not (spoiler note) ]
-- ANCHOR_END: visibleTo

-- ANCHOR: logReading
-- 📓 Write a line in the reading log, if the entry is in the table.
-- 読書ログに1行書く。目次にある項目のときだけ
logReading :: LogLine -> Card -> Checked Card
logReading line card = do
  checkTarget (contents (work card)) (OnEntry (readEntry line))
  pure card { readLog = readLog card ++ [line] }
-- ANCHOR_END: logReading

-- ANCHOR: unread
-- 🧽 Take back "I read it": drop every line of the log about this
--    entry (read twice, both lines go), if the entry is in the table.
--    The other entries keep their lines, so their marks stay.
-- 「読んだ」を取り消す：この項目のログの行を全部消す（2回読んでいれば、
-- 2行とも）。目次にある項目のときだけ。ほかの項目の行は残るので、
-- ほかの印はそのまま
unread :: EntryId -> Card -> Checked Card
unread i card = do
  checkTarget (contents (work card)) (OnEntry i)
  pure card { readLog = [ l | l <- readLog card, readEntry l /= i ] }
-- ANCHOR_END: unread

-- ANCHOR: rateEntry
-- ⭐ Rate one entry, like an episode, if it is in the table
--    (a new rating replaces the old one, as with `rate`).
-- 目次の項目（話など）1つに評価を付ける。目次にある項目のときだけ
-- （付け直したら、rate と同じく、新しいほうにする）
rateEntry :: EntryId -> Score -> Card -> Checked Card
rateEntry i r card = do
  checkTarget (contents (work card)) (OnEntry i)
  pure card { entryRatings = Map.insert i r (entryRatings card) }

-- 🧺 Every rating I gave this work: the whole work (if rated) and
--    each entry, walking the table from top to bottom.
-- この作品に付けた評価を全部：作品全体（付けていれば）と、
-- 項目ごとの評価（目次を上から順に歩いて拾う）
ratingsOf :: Card -> [Score]
ratingsOf card = maybeToList (rating card) ++ entryScores
  where
    entryScores =
      [ r | i <- entryIds (contents (work card))
          , Just r <- [Map.lookup i (entryRatings card)] ]
-- ANCHOR_END: rateEntry

-- ANCHOR: editAttrs
-- 🏷️ Fill in my own attributes from a form, if every field reads by
--    its definition. A blank field takes the value away; a field that
--    is not in the form stays as it was.
-- フォームから、自分で決めた属性を書きこむ。どの欄も定義どおりに
-- 読めたときだけ。空の欄は値を消す。フォームにない欄は、そのまま
editAttrs :: Map Text Text -> Schema -> Card -> Checked Card
editAttrs typed schema card = do
  fields <- readFields schema typed
  pure card { attrs = Map.foldrWithKey put (attrs card) fields }
  where
    put name Nothing      = Map.delete name
    put name (Just value) = Map.insert name value
-- ANCHOR_END: editAttrs

-- ANCHOR: moveFlow
-- 📍 Where a card is in a flow, if it is in it at all.
-- カードが、流れのどこにいるか。まだ入っていなければ Nothing
stateOf :: Flow -> Card -> Maybe Text
stateOf flow card = Map.lookup (flowName flow) (states card)

-- 🔀 Move a card to a state of a flow: first to the start, then along
--    the arrows.
-- カードを、流れの状態へ動かす。はじめは、はじめの状態へ。
-- そのあとは、矢印に沿って
moveFlow :: Text -> Text -> Schema -> Card -> Checked Card
moveFlow name to schema card = do
  flow <- findFlow schema name
  let from = stateOf flow card
  when (to `notElem` nextStates flow from) (Left (NoSuchArrow name from to))
  pure card { states = Map.insert name to (states card) }
-- ANCHOR_END: moveFlow

-- ANCHOR: addEpisodes
-- 🔢 How many episodes a serial has. Arcs count 0, episodes 1.
-- 連載に、何話あるか。章は 0、話は 1 と数える
episodesIn :: Toc Episodes -> Int
episodesIn toc = sum [ n | Entry _ _ (Episodes n) <- concatMap flatten toc ]

-- 🌱 New episodes came out: one for each number the shelf gave, named
--    第4話, 第5話, ... (the API tells how many, not their titles).
-- 新しい話が出た：本棚がくれた番号ごとに1話、第4話、第5話……と名前を
-- 付けて足す（API が教えてくれるのは話の数だけで、題はわからない）
addEpisodes :: [EntryId] -> Card -> Checked Card
addEpisodes ids card = case contents (work card) of
  Serial toc -> Right card { work = (work card) { contents = grown toc } }
  Book   _   -> Left NotASerial
  Series _   -> Left NotASerial
  Film   _   -> Left NotASerial
  Game       -> Left NotASerial
  where
    grown toc = Serial (foldl' add toc (zip [episodesIn toc + 1 ..] ids))
    add toc (n, eid) =
      growWith eid ("第" <> T.pack (show n) <> "話", Episodes 1) toc
-- ANCHOR_END: addEpisodes

-- ANCHOR: checkWork
-- 📏 The longest title a work may have.
-- 作品の題の、いちばん長い字数
titleLimit :: Int
titleLimit = 200

-- 📏 The largest amount one heading may have (pages, minutes, or
--    episodes). Small enough that adding them all never overflows.
-- 見出し1つの量（ページ・分・話）の、いちばん大きな数。全部を
-- 足しても、あふれない大きさ
sizeLimit :: Int
sizeLimit = 1000000

-- 🚪 A work that came from outside: it needs a title (not too long),
--    its table must have a good shape (TocCheck: every problem at
--    once), and no amount may be below zero.
-- 外から来た作品を確かめる：題があって（長すぎず）、目次の形がよく
-- （TocCheck。まずい所は全部いっぺんに）、どの量もマイナスでないこと
checkWork :: Work -> Checked Work
checkWork = checkWorkAllowing []

-- 🚪 The same gate, letting through the problems of shape the table
--    already had. A work can come in by other ways (imported, or grown
--    by new episodes) without this gate; fixing its title must not be
--    refused for what was there before.
-- 同じ門。ただし、目次にもとからあった形のまずい所は通す。作品は、
-- この門を通らない入り口（取り込み、新しい話）からも来る。題を直す
-- だけで、前からあったものを理由に断らない
checkWorkAllowing :: [TocProblem] -> Work -> Checked Work
checkWorkAllowing known w = do
  when (T.null (T.strip (title w)))       (Left EmptyTitle)
  when (T.length (title w) > titleLimit) (Left (LongTitle titleLimit))
  case [ p | p <- shapeProblems (headingsOf (contents w))
           , p `notElem` known ] of
    []       -> Right ()
    problems -> Left (BadToc problems)
  case [ name | (name, n) <- amountsIn w, n < 0 ] of
    name : _ -> Left (NegativeSize name)
    []       -> Right ()
  case [ name | (name, n) <- amountsIn w, n > sizeLimit ] of
    name : _ -> Left (HugeSize name sizeLimit)
    []       -> Right w

-- 🌲 Only the headings of a table, as a tree.
-- 目次の見出しだけを、木のまま
headingsOf :: Contents -> Forest Text
headingsOf what = case what of
  Book   toc -> names toc
  Serial toc -> names toc
  Series toc -> names toc
  Film   _   -> []
  Game       -> []
  where
    names :: Toc u -> Forest Text
    names = map (fmap heading)

-- 🧮 Every amount of a work, with the name it goes by: the headings of
--    the table, or the title of a film.
-- 作品の量の全部を、その名前と一緒に：目次の見出しか、映画の題
amountsIn :: Work -> [(Text, Int)]
amountsIn w = case contents w of
  Book   toc         -> headings toc
  Serial toc         -> headings toc
  Series toc         -> headings toc
  Film   (Minutes m) -> [(title w, m)]
  Game               -> []
  where
    headings :: Amount u => Toc u -> [(Text, Int)]
    headings toc =
      [ (heading e, count (size e)) | e <- concatMap flatten toc ]
-- ANCHOR_END: checkWork
-- ANCHOR: revise
-- ✏️ Fix a work by hand: a new title, creator, and table. First, the
--    work must still be as the screen saw it (basedOn); if it changed
--    meanwhile, say so and change nothing. A line with a number keeps
--    it, wherever it moved; a new line gets the next number from `from`
--    on (the shelf keeps that many free: newItems). Then the new work
--    goes through the same gate as a new one (checkWork; what the table
--    already had is let through), and what hung on a line that is gone
--    is carried over (followTable).
-- 作品を手で直す：新しい題・作者・目次。まず、作品が画面の見たとおりの
-- ままか（basedOn）を確かめる。そのあいだに変わっていたら、そう言って、
-- 何も変えない。番号のある行は、どこへ動いても番号を保つ。新しい行は
-- from から順に番号をもらう（本棚はその分を空けておく：newItems）。
-- そのあと、新しい作品と同じ門（checkWork。もとの目次にあったまずい
-- 所は通す）を通し、消えた行にぶら下がっていたものを引き継ぐ
-- （followTable）
revise :: EntryId -> Revision -> Card -> Checked Card
revise from r card = do
  -- ANCHOR: staleCheck
  when (asOpened (work card) /= basedOn r) (Left ChangedElsewhere)
  -- ANCHOR_END: staleCheck
  table <- retable from r (contents (work card))
  let before = shapeProblems (headingsOf (contents (work card)))
  fixed <- checkWorkAllowing before (Work (newTitle r) (newCreator r) table)
  pure (followTable (notesOnRemoved r) (entryIds table)
                    card { work = fixed })

-- ANCHOR: asOpened
-- 📸 A work as the edit screen sees it when it opens: the title, the
--    creator, and every line with its number, heading, and size.
-- 直す画面が開いたときに見る作品：題、作者、番号・見出し・量つきの全部の行
asOpened :: Work -> Opened
asOpened w = Opened
  { openedTitle   = title w
  , openedCreator = creator w
  , openedToc     = case contents w of
      Book   toc -> items toc
      Serial toc -> items toc
      Series toc -> items toc
      Film   _   -> []
      Game       -> []
  }
  where
    items :: Amount u => Toc u -> Forest Item
    items = map (fmap (\e -> Item (Just (entryId e)) (heading e)
                                  (count (size e))))
-- ANCHOR_END: asOpened

-- 🪞 A work as a revision that changes nothing, based on the work as it
--    is now. An edit screen starts from this, with the safe choice for
--    notes.
-- 何も変えない Revision としての作品。いまの作品をもとにする。直す画面は、
-- ここから始める。感想は、安全な方を選んでおく
asRevision :: Work -> Revision
asRevision w = Revision
  { newTitle       = openedTitle seen
  , newCreator     = openedCreator seen
  , newToc         = openedToc seen
  , notesOnRemoved = MoveToWork
  , basedOn        = seen
  }
  where
    seen = asOpened w

-- 🔢 How many lines of the revision are new (need a number).
-- 直した目次の、新しい行（番号の要る行）の数
newItems :: Revision -> Int
newItems r =
  length [ () | Item Nothing _ _ <- concatMap flatten (newToc r) ]

-- 🌳 The new table, in the unit of the work. Every number must be one
--    of this work's, and used once. A film or a game has no table.
-- 新しい目次を、作品の単位で。番号はどれも、この作品のもので、1回だけ。
-- 映画とゲームには、目次がない
retable :: EntryId -> Revision -> Contents -> Checked Contents
retable (EntryId from) r what = case what of
  Book   _ -> Book   <$> filled Pages
  Serial _ -> Serial <$> filled Episodes
  Series _ -> Series <$> filled Minutes
  Film   m -> if null (newToc r) then Right (Film m) else Left NoTable
  Game     -> if null (newToc r) then Right Game     else Left NoTable
  where
    filled :: (Int -> u) -> Checked (Toc u)
    filled unit = do
      mapM_ known given
      case twice given of
        i : _ -> Left (EntryTwice i)
        []    -> Right ()
      pure (snd (mapAccumL (mapAccumL (give unit)) from (newToc r)))
    given = [ i | Item (Just i) _ _ <- concatMap flatten (newToc r) ]
    known i
      | i `elem` entryIds what = Right ()
      | otherwise              = Left (NoSuchEntry i)
    twice xs = [ x | (x, n) <- Map.toList (Map.fromListWith (+)
                                 [ (x, 1 :: Int) | x <- xs ]), n > 1 ]
    -- From the top down, a new line takes the next free number.
    -- 上から順に、新しい行が次の空いた番号を取る
    give unit next item = case item of
      Item Nothing  h n -> (next + 1, Entry (EntryId next) h (unit n))
      Item (Just i) h n -> (next,     Entry i h (unit n))

-- 🧳 What hung on lines that are gone, given the numbers still there.
--    A note is my own words, so I choose: move it to the whole work, or
--    drop it with the line. A rating or a log line of a line that is
--    gone means nothing any more, so it always goes with the line.
-- 消えた行にぶら下がっていたものを、残った番号から決める。感想は自分の
-- 言葉なので、作品全体へ移すか、行と一緒に消すかを選ぶ。消えた行の★と
-- 読書ログは、もう意味がないので、いつも行と一緒に消える
followTable :: NotesOnRemoved -> [EntryId] -> Card -> Card
followTable choice kept card = card
  { notes        = concatMap follow (notes card)
  , readLog      = [ l | l <- readLog card, readEntry l `elem` kept ]
  , entryRatings = Map.filterWithKey (\i _ -> i `elem` kept)
                                     (entryRatings card)
  }
  where
    follow note = case target note of
      OnEntry i | i `notElem` kept -> case choice of
        MoveToWork -> [note { target = OnWork }]
        DropNotes  -> []
      _                            -> [note]
-- ANCHOR_END: revise
-- ANCHOR: tags
-- 🏷️ How many tags a card may have, and how long one may be.
-- カードに付けられるタグの数と、1つの長さ
tagLimit, tagLength :: Int
tagLimit  = 20
tagLength = 30

-- 🏷️ Put these tags on a card (replacing the old ones). A tag must not
--    be blank, hold a comma (, 、 or ，) or a control character (a line
--    break, NUL), or be too long; and not too many of them.
-- カードにタグを付ける（前のタグと取り替える）。空白だけ、カンマ（, と
-- 、と ，）入り、制御文字（改行・NUL）入り、長すぎるタグは使えない。
-- 数も多すぎてはいけない
setTags :: Set Text -> Card -> Checked Card
setTags new card
  | Set.size new > tagLimit = Left (TooManyTags (Set.size new))
  | otherwise = case filter unusable (Set.toList new) of
      bad : _ -> Left (BadTag bad)
      []      -> Right card { tags = new }
  where
    unusable t = T.null (T.strip t) || T.strip t /= t
              || T.any (`elem` tagCommas) t || T.any isControl t
              || T.length t > tagLength

-- ✂️ The commas that split tags: the ASCII one, 、, and the full-width ，.
-- タグを分けるカンマ：半角の , と 、と、全角の ，
tagCommas :: [Char]
tagCommas = [',', '、', '，']

-- ✂️ Tags typed in one box: "推理, 魔法、図書館" → {図書館, 推理, 魔法}.
--    Commas (, 、 or ，) split them; blanks around them go away.
-- 1つの箱に打ったタグ：「推理, 魔法、図書館」→ {図書館, 推理, 魔法}。
-- カンマ（, か 、か ，）で分け、前後の空白は落とす
readTags :: Text -> Set Text
readTags typed = Set.fromList
  [ t | piece <- T.split (`elem` tagCommas) typed
      , let t = T.strip piece, not (T.null t) ]
-- ANCHOR_END: tags
-- ANCHOR_END: all
