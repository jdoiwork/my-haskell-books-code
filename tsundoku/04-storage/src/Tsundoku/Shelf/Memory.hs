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
import Data.Traversable (mapAccumL)

import Tsundoku.Card qualified as Card
import Tsundoku.Card (Card (..), newCard)
import Tsundoku.Shelf
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: memory
-- 🧠 Everything on the shelf, and the next free numbers.
--    Numbers only go up, so a removed number is never used again.
-- 本棚の全部と、次に空いている番号。番号は増えるだけなので、
-- 消した番号を使い回すことはない
data Memory = Memory
  { nextWork  :: !Int
  , nextEntry :: !Int
  , cards     :: !(Map WorkId Card)
  }

-- 🆕 A new, empty shelf in memory.
-- メモリに、空っぽの本棚を新しく
newMemoryShelf :: IO Shelf
newMemoryShelf = do
  ref <- newIORef (Memory 1 1 Map.empty)
  let change wid core = atomicModifyIORef' ref $ \m ->
        case decide wid core (Map.lookup wid (cards m)) of
          Left why  -> (m, Left why)
          Right new -> (m { cards = Map.insert wid new (cards m) }, Right new)
  pure Shelf
    { addWork    = \w -> atomicModifyIORef' ref (shelve w)
    , removeWork = \wid -> atomicModifyIORef' ref $ \m ->
        ( m { cards = Map.delete wid (cards m) }
        , Map.member wid (cards m) )
    , getCard    = \wid -> Map.lookup wid . cards <$> readIORef ref
    , listCards  = Map.toList . cards <$> readIORef ref
    , applyEvent = \wid e -> fmap reading <$> change wid (Card.applyEvent e)
    , addNote    = \wid n -> done <$> change wid (Card.addNote n)
    , logReading = \wid l -> done <$> change wid (Card.logReading l)
    , rate       = \wid s -> done <$> change wid (Right . Card.rate s)
    , rateEntry  = \wid i s -> done <$> change wid (Card.rateEntry i s)
    }
  where
    done = fmap (const ())
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
  in  ( Memory (nextWork m + 1) after (Map.insert wid card (cards m))
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
