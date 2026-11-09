-- ANCHOR: all
-- | Attributes and states of my own: the definitions are data, and
--   the gate checks every value against them. Pure.
-- 自分で決める属性と状態：定義はデータで、値は門で定義に照らす。純粋
-- ANCHOR: header
module Tsundoku.Custom
  ( -- * Values / 値
    readValue
  , showValue
  , kindName
    -- * Definitions / 定義
  , emptySchema
  , defineAttr
  , defineFlow
  , findAttr
  , findFlow
  , reachable
    -- * The gate / 門
  , readFields
  , nextStates
  ) where

-- dictionaries, sets, text, dates, and reading numbers
-- 辞書、集合、文字列、日付、数を読む
import Data.List (nub, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

import Tsundoku.Types
import Tsundoku.Validation
-- ANCHOR_END: header

-- ANCHOR: readValue
-- 📥 Read what someone typed, as a value of this kind. The same reader
--    serves the form and the search.
-- 打ちこまれた文字を、その種類の値として読む。フォームにも、
-- 絞り込みの言葉にも、同じものを使う
readValue :: AttrKind -> Text -> Maybe AttrValue
readValue kind typed = case kind of
  TextKind       -> TextV   <$> nonBlank
  NumberKind     -> NumberV <$> wholeNumber
  DateKind       -> DateV   <$> iso8601ParseM (T.unpack typed)
  YesNoKind      -> YesNoV  <$> lookup typed yesNo
  ChoiceKind cs
    | typed `elem` cs -> Just (ChoiceV typed)
    | otherwise       -> Nothing
  where
    yesNo = [("はい", True), ("いいえ", False)]
    nonBlank
      | T.null (T.strip typed) = Nothing
      | otherwise              = Just typed
    -- "12" and "-3" are numbers; "12冊" and "3.5" are not.
    -- "12" と "-3" は数。"12冊" や "3.5" は数ではない
    wholeNumber = case T.signed T.decimal typed of
      Right (n, rest) | T.null rest -> Just n
      _                             -> Nothing

-- 📤 And back, to show in a form. readValue reads it again.
-- そして、フォームに見せる文字に戻す。readValue で読み戻せる
showValue :: AttrValue -> Text
showValue value = case value of
  TextV   t     -> t
  NumberV n     -> T.pack (show n)
  DateV   d     -> T.pack (iso8601Show d)
  YesNoV  True  -> "はい"
  YesNoV  False -> "いいえ"
  ChoiceV c     -> c

-- 🏷️ The name of a kind, for the screen.
-- 種類の名前（画面に出すため）
kindName :: AttrKind -> Text
kindName kind = case kind of
  TextKind     -> "文字"
  NumberKind   -> "数"
  DateKind     -> "日付"
  YesNoKind    -> "はい/いいえ"
  ChoiceKind _ -> "選択肢"
-- ANCHOR_END: readValue



-- ANCHOR: defineAttr
-- 📒 Nothing defined yet.
-- まだ何も決めていない
emptySchema :: Schema
emptySchema = Schema { attrDefs = [], flows = [] }

-- ➕ Define an attribute, if the name is free and the kind makes sense.
--    Every problem is reported, not just the first.
-- 属性を定義する。名前が空いていて、種類に意味があるときだけ。
-- まずい所は、最初の1つだけでなく、全部知らせる
defineAttr :: AttrDef -> Schema -> Checked Schema
defineAttr def schema = do
  checked BadDefinition
    (freeName schema (attrName def) *> goodKind (attrKind def))
  pure schema { attrDefs = attrDefs schema ++ [def] }

-- 🪪 A name is free if no attribute or flow has it. ★ is the rating's.
-- 名前が空いている：属性にも流れにも使っていない。★は評価のもの
freeName :: Schema -> Text -> Validation [DefProblem] ()
freeName schema name
  | name `elem` taken = problem (NameTaken name)
  | otherwise         = Valid ()
  where
    taken =  ["★"]
          ++ map attrName (attrDefs schema)
          ++ map flowName (flows schema)

-- 🎛️ Choices need at least one choice, each one only once.
-- 選択肢は、少なくとも1つ。同じものは1回だけ
goodKind :: AttrKind -> Validation [DefProblem] ()
goodKind (ChoiceKind [])      = problem NothingToPick
goodKind (ChoiceKind choices) = noTwice choices
goodKind _                    = Valid ()

-- 👯 Each name only once.
-- 同じ名前は1回だけ
noTwice :: [Text] -> Validation [DefProblem] ()
noTwice names = case nub (names \\ nub names) of
  []      -> Valid ()
  doubled -> Invalid (map Twice doubled)
-- ANCHOR_END: defineAttr

-- ANCHOR: readFields
-- 🔎 Look up a definition by its name.
-- 名前で定義を探す
findAttr :: Schema -> Text -> Maybe AttrDef
findAttr schema name =
  lookup name [ (attrName d, d) | d <- attrDefs schema ]

-- 🚪 The gate for a form: read every field by its definition. A blank
--    field means "no value". Every problem is reported at once.
-- フォームの門：どの欄も、その定義で読む。空の欄は「値なし」。
-- まずい所は、全部まとめて知らせる
readFields :: Schema -> Map Text Text -> Checked (Map Text (Maybe AttrValue))
readFields schema typed =
  checked BadAttrs (Map.traverseWithKey readField typed)
  where
    readField :: Text -> Text -> Validation [AttrProblem] (Maybe AttrValue)
    readField name raw = case findAttr schema name of
      Nothing -> problem (UnknownName name)
      Just def
        | T.null (T.strip raw) -> Valid Nothing
        | otherwise -> case readValue (attrKind def) (T.strip raw) of
            Just value -> Valid (Just value)
            Nothing    -> problem (Unreadable name raw)
-- ANCHOR_END: readFields

-- ANCHOR: defineFlow
-- ➕ Define a flow of states, if its map makes sense.
-- 状態の流れを定義する。地図に意味があるときだけ
defineFlow :: Flow -> Schema -> Checked Schema
defineFlow flow schema = do
  checked BadDefinition
    (  freeName schema (flowName flow)
    *> noTwice (flowStates flow)
    *> arrowsLand flow
    *> allReachable flow )
  pure schema { flows = flows schema ++ [flow] }

-- 🎯 Every arrow starts and ends at a state of the flow.
-- 矢印の元と先は、どれも流れの中の状態
arrowsLand :: Flow -> Validation [DefProblem] ()
arrowsLand flow = case nub ends \\ flowStates flow of
  []      -> Valid ()
  missing -> Invalid (map NoSuchState missing)
  where
    ends = concat [ [from, to] | (from, to) <- arrows flow ]

-- 🧭 Every state can be reached from the start.
-- どの状態にも、はじめの状態からたどり着ける
allReachable :: Flow -> Validation [DefProblem] ()
allReachable flow =
  case filter (`Set.notMember` reachable flow) (flowStates flow) of
    []    -> Valid ()
    alone -> Invalid (map Unreachable alone)

-- 🗺️ The states we can reach from the start, by following arrows.
-- はじめの状態から、矢印をたどって行ける状態
reachable :: Flow -> Set Text
reachable flow = go Set.empty [start flow]
  where
    go seen []         = seen
    go seen (s : todo)
      | s `Set.member` seen = go seen todo
      | otherwise           = go (Set.insert s seen) (arrowsFrom s ++ todo)
    arrowsFrom s = [ to | (from, to) <- arrows flow, from == s ]

-- ➡️ Where a card may go next. A card not in the flow yet may only
--    enter at the start; after that, it follows the arrows.
-- カードが次に行ける状態。まだ流れに入っていないカードは、はじめの
-- 状態にだけ入れる。そのあとは、矢印に沿って
nextStates :: Flow -> Maybe Text -> [Text]
nextStates flow Nothing    = [start flow]
nextStates flow (Just now) = [ to | (from, to) <- arrows flow, from == now ]

-- 🔎 Look up a flow by its name.
-- 名前で流れを探す
findFlow :: Schema -> Text -> Checked Flow
findFlow schema name =
  case lookup name [ (flowName f, f) | f <- flows schema ] of
    Just flow -> Right flow
    Nothing   -> Left (NoSuchFlow name)
-- ANCHOR_END: defineFlow
-- ANCHOR_END: all
