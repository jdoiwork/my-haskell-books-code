-- ANCHOR: all
-- ANCHOR: module
-- ANCHOR: imports
module Battle where

-- JSON: the classes, the generic helpers, and the options.
-- JSON の型クラス、Generic を使う関数、オプション
import Data.Aeson
  ( FromJSON (..)
  , Options (..)
  , SumEncoding (..)
  , ToJSON (..)
  , Value
  , camelTo2
  , defaultOptions
  , genericParseJSON
  , genericToJSON
  )
import GHC.Generics (Generic)

-- The elements, from the bestiary.
-- 図鑑の属性
import Monster (Element (..))
-- ANCHOR_END: imports
-- ANCHOR_END: module

-- ANCHOR: action
-- ⚔️ What a hero can do in one turn.
-- 勇者が1ターンにできること
data Action
  = Attack Int    -- attack with this power / この威力で攻撃
  | Spell Element -- cast a spell of this element / この属性の呪文
  | Heal Int      -- heal this much HP / これだけ HP を回復
  | Defend        -- defend (no value) / 防御（値なし）
  deriving stock (Show, Eq, Generic)
-- ANCHOR_END: action

-- ANCHOR: defaultJson
-- 🔍 The JSON that Generic makes with the default options.
-- 既定のオプションで Generic が作る JSON
defaultActionJson :: Action -> Value
defaultActionJson = genericToJSON defaultOptions
-- ANCHOR_END: defaultJson

-- ANCHOR: options
-- ⚙️ Our own shape: {"type": "attack", "value": 12}.
-- 自分で決めた形：{"type": "attack", "value": 12}
actionOptions :: Options
actionOptions = defaultOptions
  { sumEncoding            = TaggedObject "type" "value"
  , constructorTagModifier = camelTo2 '_'
  }

instance ToJSON Action where
  toJSON = genericToJSON actionOptions

instance FromJSON Action where
  parseJSON = genericParseJSON actionOptions
-- ANCHOR_END: options

-- ANCHOR: battleLog
-- 📜 A battle log: what the hero did, turn by turn.
-- バトルログ：勇者がターンごとにしたこと
battleLog :: [Action]
battleLog = [Attack 12, Spell Fire, Heal 20, Defend]
-- ANCHOR_END: battleLog
-- ANCHOR_END: all
