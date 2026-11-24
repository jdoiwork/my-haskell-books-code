-- ANCHOR: all
-- ANCHOR: header
-- | The small language of replacement texts, the same as PCRE2's:
--   $1 or ${1} is group 1 ($0 is the whole match), $name or ${name} is
--   the named group, and $$ is a $. It is read once, before it is used,
--   so that a wrong one is found early, like a wrong pattern.
-- 置き換えの文字列の小さな言語。PCRE2 と同じ：$1 か ${1} は1番の
-- グループ（$0 は一致の全体）、$name か ${name} は名前付きのグループ、
-- $$ は $ そのもの。使う前に1回だけ読むので、まちがいは、パターンと
-- 同じく、早くわかる
module MiniRegex.Template
  ( Template
  , template
  , expand
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Types
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🧾 A replacement text, read: plain text, and the groups to put in.
-- 読んだ置き換えの文字列：そのままの文字と、差しこむグループ
newtype Template = Template [Part]
  deriving (Show, Eq)

data Part
  = Plain Text  -- as it is / そのまま
  | Insert Int  -- the group with this number (0: all) / この番号のグループ
  deriving (Show, Eq)
-- ANCHOR_END: types

-- ANCHOR: read
-- 📖 Read a replacement text for this regex. The groups must be in the
--    regex. The places and messages of the errors are PCRE2's. Use the
--    template with this regex only: a name is kept as its number here.
-- この正規表現のための置き換えの文字列を読む。グループは、正規表現の
-- 中になければならない。エラーの場所と文は PCRE2 と同じ。テンプレートは、
-- この正規表現にだけ使う（名前は、ここでの番号にして覚えるから）
template :: Regex -> Text -> Either RegexError Template
template re = fmap (Template . merge) . go 0 . T.unpack
  where
    go :: Int -> String -> Either RegexError [Part]
    go _ [] = Right []
    go i ('$' : '$' : cs) = (Plain "$" :) <$> go (i + 2) cs
    go i ('$' : '{' : cs) = do
      -- a number, or a name: PCRE2 reads only digits after a digit, so
      -- ${1x} wants } just after the 1
      -- 番号か名前。PCRE2 は、数字で始まれば数字だけを読むので、${1x} は
      -- 1 のすぐうしろに } がほしい
      let (w, after) = case cs of
            d : _ | isDigit d -> span isDigit cs
            _                 -> span isNameChar cs
          end        = i + 2 + length w
      case after of
        _ | null w -> wrong (i + 2)                         -- ${}
        '}' : rest -> do
          -- (a number that is not there: just after its first digit, as
          -- for $12; a name: just after the })
          -- （ない番号なら、$12 と同じく最初の数字のすぐうしろ。名前なら } の
          -- うしろ）
          p <- insert (if all isDigit w then i + 3 else end + 1) w
          (p :) <$> go (end + 1) rest
        _          -> Left (Invalid end
          "expected closing curly bracket in replacement string")  -- -58
    go i ('$' : cs@(d : _)) | isDigit d =
      -- $12 is group 12; PCRE2 points just after the first digit
      -- $12 は12番のグループ。PCRE2 は最初の数字のすぐうしろを指す
      let (w, rest) = span isDigit cs
      in  (:) <$> insert (i + 2) w <*> go (i + 1 + length w) rest
    go i ('$' : cs) = case span isNameChar cs of
      ([], _)   -> wrong (i + 1)                            -- $ alone
      (w, rest) -> let end = i + 1 + length w
                   in  (:) <$> insert end w <*> go end rest
    go i (c : cs) = (Plain (T.singleton c) :) <$> go (i + 1) cs

    -- a group by number or by name; `at` is where PCRE2 points if it is
    -- not in the regex
    -- 番号か名前のグループ。正規表現になければ、PCRE2 は at を指す
    insert at w
      | all isDigit w, n <= toInteger total = Right (Insert (fromInteger n))
      | all isDigit w = unknown
      | Just g <- lookup (T.pack w) (groupNames (tree re)) = Right (Insert g)
      | otherwise = unknown
      where
        n       = read w :: Integer
        total   = groupCount (tree re)
        unknown = Left (Invalid at "unknown substring")     -- -49

    wrong at = Left (Invalid at "invalid replacement string")  -- -35
    isNameChar c = c == '_' || (isAlphaNum c && c < '\x80')

-- 🧹 Join the plain parts next to each other.
-- となりあう、そのままの文字をつなぐ
merge :: [Part] -> [Part]
merge (Plain a : Plain b : ps) = merge (Plain (a <> b) : ps)
merge (p : ps)                 = p : merge ps
merge []                       = []
-- ANCHOR_END: read

-- ANCHOR: expand
-- 🖨️ The replacement for one match. A group that found nothing puts
--    in nothing (PCRE2 calls it an error, unless it is told
--    "substitute_unset_empty"; MiniRegex always does that).
-- 見つけたもの1つの置き換え。何も見つけなかったグループは、何も
-- 差しこまない（PCRE2 はエラーにする。ただし substitute_unset_empty を
-- 付ければ、こうなる。MiniRegex は、いつもこちら）
expand :: Template -> Match -> Text
expand (Template parts) m = T.concat (map one parts)
  where
    one (Plain t)  = t
    one (Insert 0) = matchText m
    one (Insert n) = case drop (n - 1) (matchGroups m) of
      Just s : _ -> spanText s
      _          -> ""
-- ANCHOR_END: expand
-- ANCHOR_END: all
