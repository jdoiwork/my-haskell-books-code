-- ANCHOR: all
-- ANCHOR: header
-- | Searching what is inside the works: the words to look for (checked
--   at the gate), and what a hit looks like, the same on every shelf.
--   All pure: the databases find, the core says how it looks.
-- 作品の中身を探す：探す言葉（門番が確かめる）と、見つかったものの形。
-- どの本棚でも同じ形。ここは全部純粋：見つけるのはデータベース、
-- どう見せるかは核
module Tsundoku.Search
  ( -- * The gate / 門番
    Search
  , parseSearch
  , searchWords
  , longWords
  , shortWords
  , ftsQuery
  , groongaQuery
    -- * Hits / 見つかったもの
  , Place (..)
  , Hit (..)
  , Piece (..)
  , entryOf
  , marked
  , cutAround
  , snippetOf
  , trailTo
    -- * Searching by hand / 手で探す
  , documentsOf
  , findIn
  ) where

-- characters, lists, dictionaries, text, and trees
-- 文字、リスト、辞書、文字列、木
import Data.Char (toLower)
import Data.Functor (void)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..), flatten)

import Tsundoku.Card (Card (..))
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: search
-- 🚪 The words to look for. Only parseSearch makes one, so a Search is
--    never empty, and no word in it has a space.
-- 探す言葉。作れるのは parseSearch だけなので、Search はいつも1語以上で、
-- どの語にも空白がない
newtype Search = Search [Text]
  deriving (Show, Eq)

-- 🧹 Cut what was typed at the spaces. Nothing left: no search.
-- 打ったものを空白で切る。何も残らなければ、探せない
parseSearch :: Text -> Checked Search
parseSearch typed = case nub (T.words typed) of
  []     -> Left EmptySearch
  wanted -> Right (Search wanted)

searchWords :: Search -> [Text]
searchWords (Search wanted) = wanted

-- 📏 FTS5's trigram finds a word only if it has 3 characters or more.
--    The shorter ones are looked for in another way.
-- FTS5 の trigram が見つけられるのは、3文字以上の語だけ。
-- それより短い語は、別のやり方で探す
longWords, shortWords :: Search -> [Text]
longWords  search = filter (\w -> T.length w >= 3) (searchWords search)
shortWords search = filter (\w -> T.length w <  3) (searchWords search)
-- ANCHOR_END: search

-- ANCHOR: queries
-- 🔒 The long words as an FTS5 query: each one a string in double
--    quotes (a " inside is written twice), side by side, so all of them
--    must be there. Nothing typed is read as AND, OR, NEAR, or (.
-- 長い語を FTS5 の問い合わせに：1語ずつ、二重引用符の文字列にする
-- （中の " は2つ重ねる）。並べると「全部を含む」。打ったものが
-- AND・OR・NEAR・( として読まれることはない
ftsQuery :: Search -> Maybe Text
ftsQuery search = case longWords search of
  []     -> Nothing
  wanted -> Just (T.unwords (map quoted wanted))
  where
    quoted w = "\"" <> T.replace "\"" "\"\"" w <> "\""

-- 🔒 Every word as a query for PGroonga's &@~: each one in double
--    quotes, with \ before every " and \ in it.
-- 全部の語を、PGroonga の &@~ の問い合わせに：1語ずつ二重引用符で
-- 囲み、中の " と \ の前に \ を置く
groongaQuery :: Search -> Text
groongaQuery search = T.unwords (map quoted (searchWords search))
  where
    quoted w = "\"" <> T.concatMap escape w <> "\""
    escape c
      | c == '"' || c == '\\' = T.pack ['\\', c]
      | otherwise             = T.singleton c
-- ANCHOR_END: queries

-- ANCHOR: hit
-- 📍 Where the words were found.
-- 言葉が見つかった場所
data Place
  = InTitle                -- the title / 題
  | InCreator              -- the creator / 作者
  | InHeading EntryId      -- a heading in the table / 目次の見出し
  | InNote    Target Bool  -- a note; True: a spoiler / 感想。True はネタバレ
  | InText    EntryId      -- the text of an entry / 項目の本文
  deriving (Show, Eq, Ord)

-- ✂️ A piece of a snippet: plain text, or one of the words.
-- 抜き出しのかけら：ふつうの文字か、探した語か
data Piece = Plain Text | Match Text
  deriving (Show, Eq)

-- 🔎 One hit. The same on every shelf. There is no score in it: each
--    database counts its own way, so only the order of the list says
--    which hit is better.
-- 見つかったもの1つ。どの本棚でも同じ形。点数は持たない（データベース
-- ごとに数え方がちがう）。どれがよく当たったかは、リストの順番だけが言う
data Hit = Hit
  { hitWork  :: WorkId
  , hitTitle :: Text
  , place    :: Place
  , trail    :: [Text]     -- headings from the top / 上からの見出しの道順
  , snippet  :: [Piece]
  }
  deriving (Show, Eq)

-- 🧭 The entry a place is in, if it is in one.
-- その場所がある項目（項目の中なら）
entryOf :: Place -> Maybe EntryId
entryOf found = case found of
  InTitle              -> Nothing
  InCreator            -> Nothing
  InHeading i          -> Just i
  InNote (OnEntry i) _ -> Just i
  InNote OnWork      _ -> Nothing
  InText i             -> Just i
-- ANCHOR_END: hit

-- ANCHOR: snippet
-- 🖍️ Mark every place where one of the words is. Upper and lower case
--    are the same, as in the databases. A longer word wins over a
--    shorter one that starts at the same place.
-- 語のある所に、全部しるしを付ける。大文字と小文字は同じとみなす
-- （データベースと同じ）。同じ所から始まる語なら、長いほうが勝つ
marked :: Search -> Text -> [Piece]
marked search = joinPlain . go
  where
    lower = T.map toLower       -- one character for one: lengths stay
    wanted = sortOn (Down . T.length) (map lower (searchWords search))
    go text = case T.uncons text of
      Nothing        -> []
      Just (c, rest) ->
        case [ n | w <- wanted, let n = T.length w
                 , lower (T.take n text) == w ] of
          n : _ -> Match (T.take n text) : go (T.drop n text)
          []    -> Plain (T.singleton c) : go rest
    -- one Plain for each run of plain characters
    -- ふつうの文字が続く所は、1つの Plain に
    joinPlain (Plain a : Plain b : more) = joinPlain (Plain (a <> b) : more)
    joinPlain (piece : more)             = piece : joinPlain more
    joinPlain []                         = []

-- ✂️ Keep the part around the first match: `width` characters before
--    it and after it, with … where we cut. No match: the start.
-- 最初に当たった所のまわりを残す。前と後ろに width 字ずつ。切った所には
-- … を付ける。当たりがなければ、頭から
cutAround :: Int -> [Piece] -> [Piece]
cutAround width pieces = case break isMatch pieces of
  (before, [])          -> keepFirst (2 * width) before
  (before, hit : after) ->
    keepLast width (T.concat [ t | Plain t <- before ])
      ++ hit : keepFirst width after
  where
    isMatch (Match _) = True
    isMatch (Plain _) = False

    keepLast n t
      | T.null t       = []
      | T.length t > n = [Plain ("…" <> T.takeEnd n t)]
      | otherwise      = [Plain t]

    keepFirst _ [] = []
    keepFirst n (piece : more)
      | n <= 0    = [Plain "…"]
      | otherwise = case piece of
          Plain t | T.length t > n -> [Plain (T.take n t <> "…")]
          Plain t                  -> piece : keepFirst (n - T.length t) more
          Match t                  -> piece : keepFirst (n - T.length t) more

-- 🔦 The snippet of a hit: the text on one line, marked, and cut to
--    20 characters on each side of the first match.
-- ヒットの抜き出し：文字を1行にして、しるしを付け、最初に当たった所の
-- 前後 20 字に切る
snippetOf :: Search -> Text -> [Piece]
snippetOf search = cutAround 20 . marked search . T.unwords . T.words
-- ANCHOR_END: snippet

-- ANCHOR: trail
-- 🧭 The headings from the top of the table down to an entry
--    ("第2部 > 第4章"). Not in the table: no trail.
-- 目次の上から、その項目までの見出し（「第2部 > 第4章」）。
-- 目次になければ、道順もない
trailTo :: EntryId -> Contents -> [Text]
trailTo wanted what =
  fromMaybe [] (lookup wanted (concatMap (walk []) (shape what)))
  where
    walk above (Node entry children) =
      let here = above ++ [heading entry]
      in  (entryId entry, here) : concatMap (walk here) children

-- 🌲 The table without its amounts (pages, episodes, minutes), so every
--    kind of work has the same shape.
-- 量（ページ・話・分）を外した目次。どの種類の作品も、同じ形になる
shape :: Contents -> Toc ()
shape what = case what of
  Book   toc -> map (fmap void) toc
  Serial toc -> map (fmap void) toc
  Series toc -> map (fmap void) toc
  Film   _   -> []
  Game       -> []
-- ANCHOR_END: trail

-- ANCHOR: byHand
-- 📄 Everything in a card we can search, with where it is. The texts
--    of the entries are kept beside the card, not in it.
-- カードの中で探せるものの全部と、その場所。項目の本文は、カードの中では
-- なく、カードの横にしまってある
documentsOf :: Card -> Map EntryId Text -> [(Place, Text)]
documentsOf card texts =
  [ (InTitle,   title   (work card))
  , (InCreator, creator (work card)) ]
  ++ [ (InHeading (entryId e), heading e)
     | e <- concatMap flatten (shape (contents (work card))) ]
  ++ [ (InNote (target n) (spoiler n), body n) | n <- notes card ]
  ++ [ (InText i, text) | (i, text) <- Map.toList texts ]

-- 🔎 Search by hand: every document that has all the words, the ones
--    with more matches first (otherwise in shelf order). The shelf in
--    memory searches this way, and the tests use it as the answer.
-- 手で探す：全部の語を含むものを、当たった数の多い順に（同じなら本棚の
-- 順に）。メモリの本棚はこうして探し、テストはこれを答えにする
findIn :: Search -> [(WorkId, Card, Map EntryId Text)] -> [Hit]
findIn search shelf = map snd (sortOn (Down . fst) found)
  where
    found =
      [ (matches pieces, Hit wid (title (work card)) at path
                             (snippetOf search text))
      | (wid, card, texts) <- shelf
      , (at, text)         <- documentsOf card texts
      , hasAll text
      , let pieces = marked search text
            path   = maybe [] (`trailTo` contents (work card)) (entryOf at) ]
    lower = T.map toLower
    hasAll text =
      all (`T.isInfixOf` lower text) (map lower (searchWords search))
    matches pieces = length [ () | Match _ <- pieces ]
-- ANCHOR_END: byHand
-- ANCHOR_END: all
