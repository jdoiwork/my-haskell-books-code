-- ANCHOR: all
-- | Cards as plain rows of a table, and back. Both databases share it:
--   only the SQL around it is theirs.
-- カードを、表の素朴な行にする。そして、行からカードに戻す。
-- 2つのデータベースで共有する（それぞれのものは、まわりの SQL だけ）
-- ANCHOR: header
module Tsundoku.Shelf.Rows
  ( -- * Writing / しまう
    toPlain
  , stateName
  , targetEntry
    -- * Reading back / 読み戻す
  , WorkRow (..)
  , assemble
  , BrokenRow (..)
    -- * JSON for what I defined / 自分で決めたものの JSON
  , attrsJson
  , statesJson
  , definitionRow
  , schemaFromRows
  , jsonText
  , fromJsonText
  ) where

-- exceptions, folding with effects, dictionaries, text, and time
-- 例外、失敗しうる畳み込み、辞書、文字列、日時
import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (Day, UTCTime)

-- JSON (aeson)
-- JSON（aeson）
import Data.Aeson (Value (..), object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Parser, parseEither)
import Data.Scientific (toBoundedInteger)

import Tsundoku.Card
import Tsundoku.Custom (emptySchema, findAttr, readValue, showValue)
import Tsundoku.Outline (fromOutline)
import Tsundoku.Rating (score)
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: plain
-- 📤 Contents as plain values: the kind, the length of a film, and the
--    table with every amount as a plain number.
-- 作品の中身を素朴な値に：種類、映画の長さ、量を数にした目次
toPlain :: Contents -> (Text, Maybe Int, Toc Int)
toPlain what = case what of
  Book   toc         -> ("book",   Nothing, counted toc)
  Serial toc         -> ("serial", Nothing, counted toc)
  Series toc         -> ("series", Nothing, counted toc)
  Film   (Minutes m) -> ("film",   Just m,  [])
  Game               -> ("game",   Nothing, [])
  where
    counted :: Amount u => Toc u -> Toc Int
    counted = map (fmap (fmap count))

-- 📥 And back. The kind says which unit the numbers are in.
-- そして、もとに戻す。数の単位は、種類が決める
fromPlain :: Text -> Maybe Int -> Toc Int -> Maybe Contents
fromPlain kind minutes toc = case (kind, minutes) of
  ("book",   _)      -> Just (Book   (amounts toc))
  ("serial", _)      -> Just (Serial (amounts toc))
  ("series", _)      -> Just (Series (amounts toc))
  ("film",   Just m) -> Just (Film (Minutes m))
  ("game",   _)      -> Just Game
  _                  -> Nothing
  where
    amounts :: Amount u => Toc Int -> Toc u
    amounts = map (fmap (fmap amount))
-- ANCHOR_END: plain

-- ANCHOR: names
-- 🏷️ A reading state as a word in a table: "Piled", "Reading", ...
-- 読書の状態を、表の中の言葉に："Piled"、"Reading"……
stateName :: ReadingState -> Text
stateName = T.pack . show

-- 🔎 And back. Only the words of the four states are states.
-- そして、もとに戻す。4つの状態の言葉だけが、状態
stateFromName :: Text -> Maybe ReadingState
stateFromName name =
  lookup name [ (stateName s, s) | s <- [minBound .. maxBound] ]

-- 🎯 The entry a note is about, or no entry (the whole work).
-- 感想が付く項目。作品全体なら、項目なし
targetEntry :: Target -> Maybe Int
targetEntry OnWork                = Nothing
targetEntry (OnEntry (EntryId i)) = Just i
-- ANCHOR_END: names

-- ANCHOR: assemble
-- 🗃️ The row of the works table. My own attributes and states are
--    two JSON objects, keyed by their names.
-- 作品の表の1行。自分で決めた属性と状態は、名前をキーにした
-- JSON のオブジェクト2つ
data WorkRow = WorkRow
  { rowTitle   :: Text
  , rowCreator :: Text
  , rowKind    :: Text
  , rowMinutes :: Maybe Int
  , rowReading :: Text
  , rowRating  :: Maybe Int
  , rowAttrs   :: Value
  , rowStates  :: Value
  }

-- 🧩 Put a card together from the rows of its tables: the work, the
--    table of contents (number, depth, heading, size, from top to
--    bottom), the notes, the log, and the ratings of entries. The
--    definitions say how to read my own attributes.
-- 表の行から、カードを組み立てる：作品、目次（番号・深さ・見出し・量を
-- 上から順に）、感想、読書ログ、項目の評価。自分で決めた属性の
-- 読み方は、定義が教えてくれる
assemble
  :: Schema
  -> WorkRow
  -> [(Int, Int, Text, Int)]
  -> [(Maybe Int, Text, UTCTime, Bool)]
  -> [(Day, Int)]
  -> [(Int, Int)]
  -> Either BrokenRow Card
assemble schema row tocRows noteRows logRows ratingRows = do
  contents' <- found "kind"    (fromPlain (rowKind row) (rowMinutes row) toc)
  reading'  <- found "reading" (stateFromName (rowReading row))
  rating'   <- traverse points (rowRating row)
  given     <- traverse (\(i, n) -> (,) (EntryId i) <$> points n) ratingRows
  attrs'    <- attrsFromJson  schema (rowAttrs  row)
  states'   <- statesFromJson schema (rowStates row)
  pure Card
    { work         = Work (rowTitle row) (rowCreator row) contents'
    , reading      = reading'
    , rating       = rating'
    , notes        = map toNote noteRows
    , readLog      = [ LogLine day (EntryId i) | (day, i) <- logRows ]
    , entryRatings = Map.fromList given
    , attrs        = attrs'
    , states       = states'
    }
  where
    toc = fromOutline
      [ (depth, Entry (EntryId i) heading' size')
      | (i, depth, heading', size') <- tocRows ]
    toNote (entry, body', at, spoiler') =
      Note (maybe OnWork (OnEntry . EntryId) entry) body' at spoiler'
    found what = maybe (Left (BrokenRow what)) Right
    points n   = either (const (Left (BrokenRow "score"))) Right (score n)

-- 💥 A row the core cannot read. Only a broken table makes one.
-- 核が読めない行。表が壊れているときにだけ起きる
newtype BrokenRow = BrokenRow Text
  deriving Show

instance Exception BrokenRow
-- ANCHOR_END: assemble

-- ANCHOR: values
-- 📤 My own attributes, as a JSON object: {"推し度": 5, ...}. The JSON
--    keeps only the shape; what each value means is in the definitions.
-- 自分で決めた属性を、JSON のオブジェクトに：{"推し度": 5, ...}。
-- JSON は形だけを持つ。それぞれの値の意味は、定義が知っている
attrsJson :: Map Text AttrValue -> Value
attrsJson = Aeson.toJSON . fmap valueJson
  where
    valueJson value = case value of
      NumberV n -> Aeson.toJSON n
      YesNoV  b -> Bool b
      other     -> String (showValue other)  -- text, date, choice

-- 📥 And back, reading each value by its definition.
-- そして、もとに戻す。どの値も、その定義で読む
attrsFromJson :: Schema -> Value -> Either BrokenRow (Map Text AttrValue)
attrsFromJson schema json = do
  object' <- fromJson "attrs" json
  Map.traverseWithKey one (object' :: Map Text Value)
  where
    one name value =
      case findAttr schema name >>= \def -> jsonValue (attrKind def) value of
        Just v  -> Right v
        Nothing -> Left (BrokenRow ("attrs: " <> name))

-- 🔎 One JSON value, as a value of this kind.
-- JSON の値1つを、その種類の値として
jsonValue :: AttrKind -> Value -> Maybe AttrValue
jsonValue kind json = case (kind, json) of
  (TextKind,     String s) -> readValue kind s
  (DateKind,     String s) -> readValue kind s
  (ChoiceKind _, String s) -> readValue kind s
  (NumberKind,   Number n) -> NumberV <$> toBoundedInteger n
  (YesNoKind,    Bool b  ) -> Just (YesNoV b)
  _                        -> Nothing

-- 📤 My own states, as a JSON object: {"連載の状態": "休止"}.
-- 自分で決めた状態を、JSON のオブジェクトに：{"連載の状態": "休止"}
statesJson :: Map Text Text -> Value
statesJson = Aeson.toJSON

-- 📥 And back. Each name must be a flow, and each state one of its states.
-- そして、もとに戻す。名前は流れの名前で、状態はその流れの状態のはず
statesFromJson :: Schema -> Value -> Either BrokenRow (Map Text Text)
statesFromJson schema json = do
  states' <- fromJson "states" json
  if and [ inFlow name now | (name, now) <- Map.toList states' ]
    then Right states'
    else Left (BrokenRow "states")
  where
    inFlow name now =
      or [ flowName f == name && now `elem` flowStates f | f <- flows schema ]
-- ANCHOR_END: values

-- ANCHOR: definitions
-- 📤 A definition, as a row of the definitions table: its name, what it
--    is ("attr" or "flow"), and its body as JSON.
-- 定義を、定義の表の1行に：名前、何の定義か（"attr" か "flow"）、
-- 中身の JSON
definitionRow :: Either AttrDef Flow -> (Text, Text, Value)
definitionRow (Left def)   = (attrName def, "attr", kindJson (attrKind def))
definitionRow (Right flow) = (flowName flow, "flow", object
  [ "start"  .= start flow
  , "others" .= others flow
  , "arrows" .= arrows flow
  ])

-- 🧩 The kind of an attribute as JSON: {"kind": "number"}.
-- 属性の種類を JSON に：{"kind": "number"}
kindJson :: AttrKind -> Value
kindJson kind = case kind of
  TextKind      -> object [ "kind" .= ("text"   :: Text) ]
  NumberKind    -> object [ "kind" .= ("number" :: Text) ]
  DateKind      -> object [ "kind" .= ("date"   :: Text) ]
  YesNoKind     -> object [ "kind" .= ("yesno"  :: Text) ]
  ChoiceKind cs -> object [ "kind" .= ("choice" :: Text), "choices" .= cs ]

-- 📥 And back: the rows of the definitions table, in order, to a schema.
-- そして、もとに戻す：定義の表の行を順に読んで、Schema に
schemaFromRows :: [(Text, Text, Value)] -> Either BrokenRow Schema
schemaFromRows = foldM add emptySchema
  where
    add schema (name, what, body) = case what of
      "attr" -> do
        kind <- parsed what (kindFrom body)
        pure schema { attrDefs = attrDefs schema ++ [AttrDef name kind] }
      "flow" -> do
        flow <- parsed what (flowFrom name body)
        pure schema { flows = flows schema ++ [flow] }
      _      -> Left (BrokenRow ("definition: " <> what))
    parsed what p =
      either (\_ -> Left (BrokenRow what)) Right (parseEither id p)

kindFrom :: Value -> Parser AttrKind
kindFrom = withObject "kind" $ \o -> do
  name <- o .: "kind"
  case name :: Text of
    "text"   -> pure TextKind
    "number" -> pure NumberKind
    "date"   -> pure DateKind
    "yesno"  -> pure YesNoKind
    "choice" -> ChoiceKind <$> o .: "choices"
    _        -> fail "kind"

flowFrom :: Text -> Value -> Parser Flow
flowFrom name = withObject "flow" $ \o ->
  Flow name <$> o .: "start" <*> o .: "others" <*> o .: "arrows"
-- ANCHOR_END: definitions

-- ANCHOR: jsonText
-- 🔤 JSON as text (for SQLite, which keeps JSON in TEXT), and back.
-- JSON を文字に（JSON を TEXT にしまう SQLite のため）。そして、もとに戻す
jsonText :: Value -> Text
jsonText = TL.toStrict . encodeToLazyText

fromJsonText :: Text -> Either BrokenRow Value
fromJsonText = either (\_ -> Left (BrokenRow "json")) Right
             . Aeson.eitherDecodeStrictText

-- 📥 Read a JSON value as a Haskell value, or it is a broken row.
-- JSON の値を Haskell の値として読む。読めなければ、壊れた行
fromJson :: Aeson.FromJSON a => Text -> Value -> Either BrokenRow a
fromJson what json = case Aeson.fromJSON json of
  Aeson.Success a -> Right a
  Aeson.Error _   -> Left (BrokenRow what)
-- ANCHOR_END: jsonText

-- ANCHOR_END: all
