-- ANCHOR: all
-- | The shelf in memory: a dictionary in an IORef. Fast, and gone when
--   the program ends. For tests, and for trying things out.
-- メモリの本棚：IORef に入れた辞書。速くて、プログラムが終わると消える。
-- テストと、試しに使うため
-- ANCHOR: header
module Tsundoku.Shelf.Memory (newMemoryShelf) where

-- a box to rewrite, dictionaries, and walking with a counter
-- 書き換えられる箱、辞書、数えながら歩く
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Traversable (mapAccumL)

import Tsundoku.Card qualified as Card
import Tsundoku.Card (Card (..), newCard)
import Tsundoku.Custom qualified as Custom
import Tsundoku.Query (search)
import Tsundoku.Search (Hit, Search, findIn)
import Tsundoku.Shelf
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: memory
-- ANCHOR: memoryData
-- 🧠 Everything on the shelf, the next free numbers, what I defined,
--    and the texts of the entries (beside the cards, not in them).
--    Numbers only go up, so a removed number is never used again.
-- 本棚の全部と、次に空いている番号、自分で決めたもの、項目の本文
-- （カードの中ではなく、横に）。番号は増えるだけなので、消した番号を
-- 使い回すことはない
data Memory = Memory
  { nextWork  :: !Int
  , nextEntry :: !Int
  , cards     :: !(Map WorkId Card)
  , schema    :: !Schema
  , texts     :: !(Map WorkId (Map EntryId Text))
  }
-- ANCHOR_END: memoryData

-- 🆕 A new, empty shelf in memory.
-- メモリに、空っぽの本棚を新しく
newMemoryShelf :: IO Shelf
newMemoryShelf = do
  ref <- newIORef (Memory 1 1 Map.empty Custom.emptySchema Map.empty)
  let -- Change the box in one step, with no other thread in between.
      -- 箱の中身を、ほかのスレッドに割りこまれずに、1歩で変える
      step :: (Memory -> (Memory, a)) -> IO a
      step = atomicModifyIORef' ref
      -- Read the box, and look at one part of it.
      -- 箱を読んで、その一部を見る
      look :: (Memory -> a) -> IO a
      look part = part <$> readIORef ref
      -- Ask the core to change one card. edit_ forgets the new card.
      -- 核にカードを1枚変えてもらう。edit_ は新しいカードを返さない
      edit  wid core = step (changeCard wid core)
      edit_ wid core = do
        result <- edit wid core
        pure (fmap (\_ -> ()) result)
      -- The same, for a core that reads the definitions too. They are
      -- read in the same step as the card.
      -- 同じことを、定義も読む核で。定義は、カードと同じ1歩で読む
      custom wid core = do
        result <- step (\m -> changeCard wid (core (schema m)) m)
        pure (fmap (\_ -> ()) result)
      -- Change the definitions, if the core says yes.
      -- 核が「よし」と言えば、定義を変える
      define core = step (changeSchema core)
  pure Shelf
    { -- 📚 The works on the shelf
      -- 本棚の作品
      addWork     = \w       -> step (shelve w)
    , removeWork  = \wid     -> step (unshelve wid)
    , getCard     = \wid     -> look (Map.lookup wid . cards)
    , listCards   =             look (Map.toList     . cards)
      -- ANCHOR: memoryRevise
    , reviseWork  = \wid r   -> step (reviseCard wid r)
      -- ANCHOR_END: memoryRevise

      -- 📖 Reading, notes, and ratings
      -- 読む・感想・評価
    , applyEvent  = \wid e   -> fmap reading <$> edit wid (Card.applyEvent e)
    , addNote     = \wid n   -> edit_ wid (Card.addNote n)
    , logReading  = \wid l   -> edit_ wid (Card.logReading l)
      -- ANCHOR: memoryUnread
    , unread      = \wid i   -> edit_ wid (Card.unread i)
      -- ANCHOR_END: memoryUnread
    , rate        = \wid s   -> edit_ wid (Right . Card.rate s)
    , rateEntry   = \wid i s -> edit_ wid (Card.rateEntry i s)

      -- 🏷️ My own attributes and states
      -- 自分で決める属性と状態
    , getSchema   =             look schema
    , defineAttr  = \d       -> define (Custom.defineAttr d)
    , defineFlow  = \f       -> define (Custom.defineFlow f)
    , editAttrs   = \wid t   -> custom wid (Card.editAttrs t)
    , moveFlow    = \wid f s -> custom wid (Card.moveFlow f s)
    , findCards   = \q       -> look (searchIn q)

      -- ANCHOR: memoryAdd
      -- 📥 Taking things in from outside
      -- 外から取り込む
    , addEpisodes = \wid n   -> step (growCard wid n)
      -- ANCHOR_END: memoryAdd

      -- ANCHOR: memorySearch
      -- 🔎 Searching what is inside
      -- 中身を探す
    , setText     = \wid i t -> step (keepText wid i t)
    , searchText  = searchWith (\s -> look (searchTexts s))
      -- ANCHOR_END: memorySearch
    }

-- 🔁 Read, decide, write: one change to one card. No card, or the
--    core says no: the shelf stays as it was.
-- 読む → 決める → 書く。カードが1枚変わる。カードがないか、
-- 核が「だめ」と言えば、本棚はそのまま
changeCard :: WorkId -> (Card -> Checked Card) -> Memory
           -> (Memory, Checked Card)
changeCard wid core m =
  case decide wid core (Map.lookup wid (cards m)) of
    Left why  -> (m, Left why)
    Right new -> (m { cards = Map.insert wid new (cards m) }, Right new)

-- ANCHOR: growCard
-- 🌱 New episodes: the shelf gives the next free numbers, and the core
--    grows the table with them. If the core says no, the numbers stay
--    free.
-- 新しい話：本棚が次に空いている番号を渡し、核がその番号で目次を育てる。
-- 核が「だめ」と言えば、番号は空いたまま
growCard :: WorkId -> Int -> Memory -> (Memory, Checked ())
growCard wid n m =
  case changeCard wid (Card.addEpisodes fresh) m of
    (_,     Left why) -> (m, Left why)
    (after, Right _ ) -> (after { nextEntry = next }, Right ())
  where
    fresh = map EntryId (take n [nextEntry m ..])
    next  = nextEntry m + length fresh
-- ANCHOR_END: growCard

-- ANCHOR: reviseCard
-- ✏️ Fix a work by hand. New lines take the next free numbers; the texts
--    of lines that are gone go with them (the core carries the rest).
-- 作品を手で直す。新しい行は、次に空いている番号を取る。消えた行の本文は、
-- 行と一緒に消える（ほかのものは、核が引き継ぐ）
reviseCard :: WorkId -> Revision -> Memory -> (Memory, Checked ())
reviseCard wid r m =
  case changeCard wid (Card.revise (EntryId (nextEntry m)) r) m of
    (_,     Left why) -> (m, Left why)
    (after, Right new) ->
      ( after { nextEntry = nextEntry m + Card.newItems r
              , texts     = Map.adjust (keepOnly new) wid (texts after) }
      , Right () )
  where
    keepOnly new = Map.filterWithKey
      (\i _ -> i `elem` entryIds (contents (work new)))
-- ANCHOR_END: reviseCard

-- 📒 Read, decide, write, for the definitions.
-- 定義も、読む → 決める → 書く
changeSchema :: (Schema -> Checked Schema) -> Memory -> (Memory, Checked ())
changeSchema core m = case core (schema m) of
  Left why  -> (m, Left why)
  Right new -> (m { schema = new }, Right ())

-- 🔎 Search every card on the shelf.
-- 本棚のカードを全部、絞り込む
searchIn :: Text -> Memory -> Checked [(WorkId, Card)]
searchIn typed m = search (schema m) typed (Map.toList (cards m))

-- ANCHOR: keepText
-- 🗑️ Take a card off the shelf, with its texts. Was it there?
-- カードを本棚から、本文と一緒に外す。その作品はあった？
unshelve :: WorkId -> Memory -> (Memory, Bool)
unshelve wid m =
  ( m { cards = Map.delete wid (cards m), texts = Map.delete wid (texts m) }
  , Map.member wid (cards m) )

-- 📝 Keep the text of an entry (a new text replaces the old one), if
--    the core says the entry is in the work.
-- 項目の本文をしまう（新しい本文は、古いものと入れ替える）。その項目が
-- 作品にあると、核が言うときだけ
keepText :: WorkId -> EntryId -> Text -> Memory -> (Memory, Checked ())
keepText wid i text m =
  case decide wid (Card.hasEntry i) (Map.lookup wid (cards m)) of
    Left why -> (m, Left why)
    Right _  -> (m { texts = Map.insertWith Map.union wid new (texts m) }
                , Right ())
  where
    new = Map.singleton i text

-- 🔎 Search every card and text on the shelf, by hand.
-- 本棚のカードと本文の全部を、手で探す
searchTexts :: Search -> Memory -> [Hit]
searchTexts s m = findIn s
  [ (wid, card, Map.findWithDefault Map.empty wid (texts m))
  | (wid, card) <- Map.toList (cards m) ]
-- ANCHOR_END: keepText
-- ANCHOR_END: memory

-- ANCHOR: shelve
-- 📥 Put a new card on the shelf. The shelf numbers the work and every
--    entry of its table, going on from the numbers used so far.
-- 新しいカードを本棚に置く。作品と目次の項目に、本棚が番号を振る。
-- 番号は、いままでに使った番号の続きから
shelve :: Work -> Memory -> (Memory, WorkId)
shelve w m =
  let (after, contents') = renumber (nextEntry m) (contents w)
      wid                = WorkId (nextWork m)
      card               = newCard w { contents = contents' }
  in  ( m { nextWork  = nextWork m + 1
          , nextEntry = after
          , cards     = Map.insert wid card (cards m) }
      , wid )

-- 🔢 Number the entries from `next`, from top to bottom.
-- next から、上から順に番号を振り直す
renumber :: Int -> Contents -> (Int, Contents)
renumber next what = case what of
  Book   toc -> fmap Book   (numberFrom toc)
  Serial toc -> fmap Serial (numberFrom toc)
  Series toc -> fmap Series (numberFrom toc)
  Film   m   -> (next, Film m)
  Game       -> (next, Game)
  where
    numberFrom :: Toc u -> (Int, Toc u)
    numberFrom = mapAccumL (mapAccumL label) next
    label n e  = (n + 1, e { entryId = EntryId n })
-- ANCHOR_END: shelve
-- ANCHOR_END: all
