-- ANCHOR: all
-- | Read a pattern into the tree of MiniRegex.Syntax, the way PCRE2
--   reads it. A wrong pattern stops with PCRE2's message, at the place
--   PCRE2 gives. A right pattern with something the tree has no place
--   for yet (\d, look-ahead, ...) stops as Unsupported.
-- パターンを読んで、MiniRegex.Syntax の木にする。読み方は PCRE2 と同じ。
-- まちがったパターンは、PCRE2 と同じ文と場所で止まる。正しいけれど、
-- 木にまだ居場所のないもの（\d、先読みなど）は Unsupported で止まる
module MiniRegex.Parser
  ( parsePattern
  , Parser
  , Reading (..)
  , Known (..)
  ) where

-- ANCHOR: imports
-- The parser, and what it keeps while reading
-- パーサーと、読みながら覚えておくもの
import Control.Monad (when)
import Control.Monad.State.Strict (State, evalState, gets, modify')
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec
  ( ParsecT, ParseErrorBundle (..), ParseError (..), ErrorFancy (..)
  , runParserT, customFailure, getOffset, atEnd, eof, try, lookAhead
  , anySingle, satisfy, takeWhileP, option, many, some, (<|>)
  , notFollowedBy, getInput )
import Text.Megaparsec.Char (char, string)

-- Characters, text and numbers
-- 文字、文字列、数
import Data.Char
  ( chr, isAlphaNum, isAscii, isDigit, isHexDigit, isLetter, isOctDigit
  , ord, toUpper, digitToInt, generalCategory, GeneralCategory (..) )
import Data.Bits (xor)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign (lengthWord8)

import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Syntax
import MiniRegex.Types
-- ANCHOR_END: imports

-- ANCHOR: parser
-- 🧰 A parser of Text that stops with a RegexError, and keeps a
--    Reading.
-- Text を読むパーサー。止まるときは RegexError で。Reading を覚えておく
type Parser = ParsecT RegexError Text (State Reading)

-- 📒 What the parser keeps while it reads: the names of the groups so
--    far (to find a name used twice), how many groups have opened so
--    far (for \g{-1}), and what the first reading found out about the
--    whole pattern (Nothing while it is the first reading).
-- パーサーが読みながら覚えておくもの：ここまでのグループの名前（同じ
-- 名前が2回出てきたら、わかるように）、ここまでに開いたグループの数
-- （\g{-1} のため）、1回目の読みでわかった、パターン全体のこと
-- （1回目の読みのあいだは Nothing）
data Reading = Reading
  { seenNames :: Set Text
  , opened    :: Int
  , known     :: Maybe Known
  }

-- 🔭 The whole pattern, from the first reading: how many groups, and
--    the numbers of the names. \k<year>(?<year>...) refers to a group
--    that comes later.
-- 1回目の読みでわかった、パターン全体のこと：グループの数と、名前の
-- 番号。\k<year>(?<year>...) は、うしろのグループを指している
data Known = Known
  { knownCount :: Int
  , knownNames :: [(Text, Int)]
  }

-- 🚧 Stop: the pattern is wrong. `at` is the place PCRE2 gives.
-- 止まる：パターンがまちがっている。at は PCRE2 が言う場所
invalid :: Int -> Text -> Parser a
invalid at message = customFailure (Invalid at message)

-- 🚧 Stop: something the tree cannot hold yet (whether the rest of the
--    pattern is right is not looked at). `at` is where that thing
--    starts.
-- 止まる：木にまだ入れられないものに出会った（パターンの残りが正しいか
-- どうかは、まだ見ていない）。at は、それが始まる場所
unsupported :: Int -> Text -> Parser a
unsupported at what = customFailure (Unsupported at what)
-- ANCHOR_END: parser

-- ANCHOR: run
-- 🏗️ Read a pattern, twice. The first reading finds the wrong
--    patterns, and counts the groups and their names. The second one,
--    knowing them, turns \k<name> into a number, and refuses a
--    reference to a group that is not there (PCRE2, too, looks over
--    the whole pattern first). Of the flags, only x (Extended) changes
--    how a pattern is read, and it is not done yet.
-- パターンを2回読む。1回目で、まちがったパターンを見つけ、グループと
-- 名前を数える。2回目は、それを知ったうえで、\k<name> を番号にし、
-- ないグループを指していたら断る（PCRE2 も、先にパターン全体を
-- 見渡す）。フラグのうち、読み方を変えるのは x（Extended）だけ。
-- それは、まだ
parsePattern :: [Option] -> Text -> Either RegexError Pattern
parsePattern opts src
  | Extended `elem` opts = Left (Unsupported 0 "flag x")
  | otherwise            = do
      first <- readOnce Nothing
      readOnce (Just (Known (groupCount first) (groupNames first)))
  where
    readOnce k = case evalState run (Reading Set.empty 0 k) of
      Right tree   -> Right tree
      Left  bundle -> Left (firstError bundle)
    run = runParserT (whole <* eof) "pattern" src

-- 🚩 The pattern, and then the end. A ) here has no ( before it.
-- パターン全体と、その終わり。ここに来た ) には、対になる ( がない
whole :: Parser Pattern
whole = do
  tree <- pattern
  at <- getOffset
  end <- atEnd
  if end
    then pure tree
    else invalid (at + 1) "unmatched closing parenthesis"  -- 122

-- 📮 Our own error, out of megaparsec's bundle. Every way to fail
--    in this parser is one of ours; just in case, anything else is
--    "not yet", never "wrong".
-- megaparsec の束から、自分のエラーを取り出す。このパーサーの失敗は
-- どれも自分のエラーのはず。念のため、ほかのものは「まちがい」では
-- なく「まだ」にする
firstError :: ParseErrorBundle Text RegexError -> RegexError
firstError bundle = case NE.head (bundleErrors bundle) of
  FancyError _ fancy | e : _ <- [e | ErrorCustom e <- Set.toList fancy] -> e
  other -> Unsupported (errorOffsetOf other) "?"
  where
    errorOffsetOf (TrivialError at _ _) = at
    errorOffsetOf (FancyError   at _)   = at
-- ANCHOR_END: run

-- ANCHOR: pattern
-- 🌳 Branches split by |.
-- | で分かれた枝
pattern :: Parser Pattern
pattern = do
  first <- branch
  rest  <- many (char '|' *> branch)
  pure (Pattern (first :| rest))

-- 🪵 Pieces, up to | or ) or the end. A quantifier may not come
--    first: there is nothing before it to repeat.
-- 部品を、| か ) か終わりまで。量指定子は先頭に来られない
-- （くり返すものが、前にないから）
branch :: Parser Branch
branch = do
  skipIgnorable
  noQuantifier
  pieces <- many piece
  pure (Branch (concat pieces))

-- 🧩 One piece: an anchor, or something to repeat with how many
--    times. \Q...\E gives some characters at once; the quantifier
--    after it is for the last one.
-- 部品1つ：アンカーか、くり返すものと回数。\Q...\E は文字をまとめて
-- くれるので、うしろの量指定子は最後の1文字にかかる
piece :: Parser [Piece]
piece = anchorPiece <|> quantifiedPieces
  where
    anchorPiece = do
      a <- (LineStart <$ char '^') <|> (LineEnd <$ char '$')
      skipIgnorable
      noQuantifier
      pure [Anchor a]
    quantifiedPieces = do
      atoms <- atomsP
      skipIgnorable
      q <- option once quantifier
      skipIgnorable
      noQuantifier
      let lastOne = NE.last atoms
          before  = NE.init atoms
      pure ([Quantified a once | a <- before] ++ [Quantified lastOne q])

-- 🙅 A quantifier here would repeat nothing (error 109). PCRE2 reads
--    the quantifier first ({5,2} is still error 104), then says 109
--    at the place just after it.
-- ここに量指定子が来たら、くり返すものがない（エラー 109）。PCRE2 は
-- 量指定子を読み終えてから（{5,2} なら、やはり 104）、そのすぐうしろの
-- 場所で 109 を言う
noQuantifier :: Parser ()
noQuantifier = do
  found <- option False (True <$ quantifierBase)
  at <- getOffset
  when found $
    invalid at "quantifier does not follow a repeatable item"  -- 109
-- ANCHOR_END: pattern

-- ANCHOR: ignorable
-- 💤 Things that leave nothing in the tree: a comment (?#...), a lone
--    \E, and an empty \Q\E. PCRE2 reads past them, so a* and a(?#c)*
--    are the same.
-- 木に何も残さないもの：コメント (?#...)、ひとりの \E、空の \Q\E。
-- PCRE2 は読み飛ばすので、a* と a(?#c)* は同じ
skipIgnorable :: Parser ()
skipIgnorable = () <$ many ignorable
  where
    ignorable = comment <|> try (string "\\E" >> pure ())
                        <|> try emptyQuote
    emptyQuote = string "\\Q" >> (() <$ string "\\E" <|> eof)
    comment = do
      _ <- try (string "(?#")
      _ <- takeWhileP Nothing (/= ')')
      closed <- option False (True <$ char ')')
      at <- getOffset
      when (not closed) $
        invalid at "missing ) after (?# comment"  -- 118
-- ANCHOR_END: ignorable

-- ANCHOR: atom
-- ⚛️ Something to repeat: one atom, or the characters of \Q...\E.
-- くり返せるもの：アトム1つ、または \Q...\E の文字
atomsP :: Parser (NonEmpty Atom)
atomsP = quoted <|> ((:| []) <$> atom)
  where
    -- \Q...\E: every character up to \E (or the end) is itself.
    -- \Q...\E：\E（か終わり）までの文字は、どれもその文字そのもの
    quoted = do
      _ <- try (string "\\Q" <* lookAhead anySingle)
      body <- many (satisfy (/= '\\') <|> try (char '\\' <* notFollowedBy e))
      _ <- option "" (string "\\E")
      -- (an empty \Q\E never comes here: skipIgnorable took it)
      -- （空の \Q\E は、ここには来ない。skipIgnorable が読み飛ばした）
      maybe (fail "empty \\Q") (pure . fmap Lit) (NE.nonEmpty body)
    e = char 'E'

-- ⚛️ One atom.
-- アトム1つ
atom :: Parser Atom
atom = do
  c <- lookAhead anySingle
  case c of
    '.'  -> AnyChar <$ anySingle
    '['  -> Class <$> charClass
    '('  -> group
    '\\' -> reference <|> (Lit <$> escaped OutsideClass)
    _ | c == '|' || c == ')' -> fail "the end of a branch"
      -- ^ and $ are pieces; * + ? and {n,m} were caught by
      -- noQuantifier. Anything else is the character itself
      -- (] and }, and a { that is not a quantifier, too).
      -- ^ と $ は部品の側で、* + ? {n,m} は noQuantifier が止めた。
      -- 残りは、その文字そのもの（] と }、量指定子でない { も）
      | otherwise -> Lit <$> anySingle
-- ANCHOR_END: atom

-- ANCHOR: escape
-- 🏠 Where a \ is: some escapes mean another thing in [ ... ].
-- \ がどこにあるか：[ ... ] の中では意味の変わるものがある
data Place = OutsideClass | InsideClass
  deriving Eq

-- 🔐 A \ that means one character. \d, \b, \1 and the like mean
--    more than one character; the tree has no place for them yet.
-- 1文字を表す \。\d・\b・\1 などは1文字ではないので、木にはまだ
-- 居場所がない
escaped :: Place -> Parser Char
escaped place = do
  at <- getOffset
  _ <- char '\\'
  end <- atEnd
  when end $ invalid (at + 1) "\\ at end of pattern"  -- 101
  c <- anySingle
  let what = T.pack ['\\', c]
  case c of
    _ | not (isAscii c)  -> pure c        -- \日 is 日
      | not (isAlphaNum c) -> pure c      -- \. \* \\ \- ...
    'a' -> pure '\a'
    'e' -> pure '\ESC'
    'f' -> pure '\f'
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    'b' | place == InsideClass -> pure '\b'
    'x' -> hexEscape at
    'o' -> octalBraces at
    '0' -> octalDigits
    'c' -> control at
    -- [\1] is the character 1 (octal), not a reference
    -- [\1] は1番の文字（8進）で、後方参照ではない
    _ | place == InsideClass && c >= '1' && c <= '7' -> do
          ds <- takeWhileUpTo 2 isOctDigit
          pure (chr (readBase 8 (c : ds)))
      | c `elem` ("ijmqyIJMOTY" :: String) ->
          invalid (at + 2) "unrecognized character follows \\"  -- 103
      | c `elem` ("lLuUF" :: String) ->
          invalid (at + 2)
            "PCRE2 does not support \\F, \\L, \\l, \\N{name}, \\U, or \\u"
            -- 137
      | otherwise -> unsupported at what  -- \d \w \b \p ...
-- ANCHOR_END: escape

-- ANCHOR: numbers
-- 🔢 \x41 (one or two digits) and \x{263a}.
-- \x41（数字は1つか2つ）と \x{263a}
hexEscape :: Int -> Parser Char
hexEscape at = braced <|> short
  where
    short = do
      ds <- takeWhileUpTo 2 isHexDigit
      when (null ds) $
        invalid (at + 2)
          "digits missing after \\x or in \\x{} or \\o{} or \\N{U+}"  -- 178
      pure (chr (readBase 16 ds))
    braced = do
      _ <- char '{'
      inBraces 16 isHexDigit
        "non-hex character in \\x{} (closing brace missing?)"  -- 167

-- 🔢 \o{101}.
-- \o{101}
octalBraces :: Int -> Parser Char
octalBraces at = do
  open <- option False (True <$ char '{')
  if open
    then inBraces 8 isOctDigit
           "non-octal character in \\o{} (closing brace missing?)"  -- 164
    else invalid (at + 2) "missing opening brace after \\o"  -- 155

-- 🔢 \0, \012: a 0 and up to two more octal digits.
-- \0、\012：0 と、あと2つまでの8進の数字
octalDigits :: Parser Char
octalDigits = do
  ds <- takeWhileUpTo 2 isOctDigit
  pure (chr (readBase 8 ds))

-- ANCHOR: inbraces
-- 🔢 The digits in { }, with spaces or tabs allowed next to the
--    braces, then }. The code point must be a Unicode character.
-- { } の中の数字（かっこのすぐ内側には空白とタブを置ける）と }。
-- 番号は、Unicode の文字でなければならない
inBraces :: Int -> (Char -> Bool) -> Text -> Parser Char
inBraces base isDigitOf wrongDigit = do
  blanks
  ds <- takeWhileP Nothing isDigitOf
  blanks
  at <- getOffset
  closed <- option False (True <$ char '}')
  end <- atEnd
  case () of
    _ | (closed || end) && T.null ds -> invalid at
          "digits missing after \\x or in \\x{} or \\o{} or \\N{U+}"  -- 178
      | T.null ds -> wrong
      | otherwise -> do
          -- the number first, as PCRE2 does: \x{110000g} is 134, not 167
          -- 先に番号を見る（PCRE2 と同じ）：\x{110000g} は 167 ではなく 134
          c <- codePoint at (readBase (toInteger base) (T.unpack ds))
          if closed then pure c else wrong
  where
    -- the place just after the wrong character (or the end)
    -- まちがった文字のすぐうしろ（か、終わり）
    wrong = do
      at <- getOffset
      end <- atEnd
      invalid (if end then at else at + 1) wrongDigit

-- ANCHOR_END: inbraces

-- 🔢 A number as a character. `at` is the place just after the digits:
--    the place of the }, or of the character where the } should be
--    (PCRE2 says that place for these two errors).
-- 番号を文字にする。at は数字のすぐうしろ：} の場所か、} があるべき
-- 場所の文字（この2つのエラーで、PCRE2 はその場所を言う）
codePoint :: Int -> Integer -> Parser Char
codePoint at n
  | n > 0x10FFFF =
      invalid at "character code point value in \\x{} or \\o{} is too large"
        -- 134
  | n >= 0xD800 && n <= 0xDFFF =
      invalid at "disallowed Unicode code point (>= 0xd800 && <= 0xdfff)"
        -- 173
  | otherwise = pure (chr (fromInteger n))

-- 🎛️ \cA is the control character 1 (A is 65, and 65 xor 64 = 1).
-- \cA は制御文字の 1（A は 65 で、65 xor 64 = 1）
control :: Int -> Parser Char
control at = do
  end <- atEnd
  when end $ invalid (at + 2) "\\c at end of pattern"  -- 102
  c <- anySingle
  when (c < ' ' || c > '~') $
    invalid (at + 3) "\\c must be followed by a printable ASCII character"
      -- 168
  pure (chr (ord (toUpper c) `xor` 0x40))

-- Read at most n characters that pass the test.
-- 条件に合う文字を、n 個まで読む
takeWhileUpTo :: Int -> (Char -> Bool) -> Parser String
takeWhileUpTo 0 _ = pure []
takeWhileUpTo n ok =
  option [] ((:) <$> satisfy ok <*> takeWhileUpTo (n - 1) ok)

-- Spaces and tabs.
-- 空白とタブ
blanks :: Parser ()
blanks = () <$ takeWhileP Nothing (`elem` (" \t" :: String))

readBase :: Num n => n -> String -> n
readBase base = foldl (\acc d -> acc * base + fromIntegral (digitToInt d)) 0
-- ANCHOR_END: numbers

-- ANCHOR: class
-- 🔤 [ ... ] and [^ ... ]. A ] just after the [ (or [^) is the
--    character ], so a class always has an item.
-- [ ... ] と [^ ... ]。[（か [^）のすぐうしろの ] は文字の ] なので、
-- 文字の集まりには、いつも項目がある
charClass :: Parser CharClass
charClass = do
  _ <- char '['
  noPosixHere
  neg <- option False (True <$ char '^')
  firstItem <- classItem (char ']' <|> classChar)
  rest <- many (classItem classChar)
  closed <- option False (True <$ char ']')
  at <- getOffset
  when (not closed) $
    invalid at "missing terminating ] for character class"  -- 106
  pure (CharClass neg (firstItem :| rest))

-- 🙅 [:alpha:] is for inside a class ([[:alpha:]]). Written as a
--    class by itself, PCRE2 refuses it (errors 112 and 113), so that
--    nobody writes [:digit:] and gets the characters d, g, i, t and :.
-- [:alpha:] は、文字の集まりの中に書くもの（[[:alpha:]]）。それだけで
-- 書くと、PCRE2 は断る（エラー 112 と 113）。[:digit:] と書いて、
-- d・g・i・t・: の集まりになってしまわないように
noPosixHere :: Parser ()
noPosixHere = do
  rest <- getInput
  at <- getOffset
  case T.unpack (T.take 1 rest) of
    [c] | c `elem` (":.=" :: String)
        , Just len <- posixLength c (T.unpack (T.drop 1 rest)) ->
          invalid (at + 1 + len) (posixError c)
    _ -> pure ()
  where
    -- how far to the c] that closes it, as PCRE2 looks for it
    -- それを閉じる c] まで、何文字か（PCRE2 と同じ探し方で）
    posixLength c = go 0
      where
        go n ('\\' : x : more) | x `elem` ("]\\" :: String) = go (n + 2) more
        go _ ('[' : x : _) | x == c = Nothing
        go n (x : ']' : _) | x == c = Just (n + 2)
        go _ (']' : _)              = Nothing
        go n (_ : more)             = go (n + 1) more
        go _ []                     = Nothing
    posixError ':' =
      "POSIX named classes are supported only within a class"  -- 112
    posixError _ = "POSIX collating elements are not supported"  -- 113

-- 🔤 a, or a-z. A - just before the ] is the character -.
-- a か a-z。] のすぐ前の - は、文字の -
classItem :: Parser Char -> Parser ClassItem
classItem first = do
  noEnd
  lo <- first
  dash <- option False (True <$ try (char '-' <* lookAhead notClose))
  if not dash
    then pure (Single lo)
    else do
      hi <- classChar
      at <- getOffset
      when (hi < lo) $
        invalid at "range out of order in character class"  -- 108
      pure (Range lo hi)
  where
    notClose = satisfy (/= ']')
    -- a [ with nothing after it is missing its ]
    -- うしろに何もない [ には、] がない
    noEnd = do
      end <- atEnd
      at <- getOffset
      when end $
        invalid at "missing terminating ] for character class"  -- 106

-- 🔤 One character in [ ... ]: itself, or a \ that means one.
-- [ ... ] の中の1文字：その文字か、1文字を表す \
classChar :: Parser Char
classChar = do
  at <- getOffset
  c <- lookAhead anySingle
  next <- lookAhead (anySingle *> option ' ' anySingle)
  case c of
    ']' -> fail "the end of the class"
    -- \Q \E, and [:alpha:] and its friends, are for later chapters.
    -- Read them first, then stop: a parser in `many` that stops
    -- without reading anything only ends the `many`.
    -- \Q \E と、[:alpha:] とその仲間は、あとの章で。先に読んでから
    -- 止まる（many の中のパーサーが、何も読まずに止まると、many が
    -- 終わるだけになる）
    '\\' | next `elem` ("QE" :: String) -> do
      _ <- anySingle
      unsupported at (T.pack ['\\', next] <> " in []")
    '\\' -> escaped InsideClass
    '[' | next `elem` (":.=" :: String) -> do
      _ <- anySingle
      unsupported at "[:"
    _ -> anySingle
-- ANCHOR_END: class

-- ANCHOR: group
-- ANCHOR: groupfn
-- 🫧 ( ... ), (?: ... ), (?<name> ... ); and the things in (? ... )
--    that are for later chapters.
-- ( ... )、(?: ... )、(?<name> ... )。(? ... ) のうち、あとの章で
-- 読むもの
group :: Parser Atom
group = pythonRef <|> do
  at <- getOffset
  _ <- char '('
  kind <- groupKind at
  -- one more group has opened (for \g{-1})
  -- グループが1つ開いた（\g{-1} のため）
  when (kind /= NonCapturing) $
    modify' (\r -> r { opened = opened r + 1 })
  inner <- pattern
  closed <- option False (True <$ char ')')
  end <- getOffset
  when (not closed) $
    invalid end "missing closing parenthesis"  -- 114
  pure (Group kind inner)
-- ANCHOR_END: groupfn

-- 🏷️ What is just after the (.
-- ( のすぐうしろに何があるか
groupKind :: Int -> Parser Capture
groupKind at = do
  rest <- lookAhead (takeWhileUpTo 3 (const True))
  case rest of
    '?' : ':' : _       -> NonCapturing <$ string "?:"
    '?' : '<' : c : _
      | c `elem` ("=!*" :: String) -> notYet (T.pack ['(', '?', '<', c])
    '?' : '<' : _       -> string "?<"  *> name '>'
    '?' : '\'' : _      -> string "?'"  *> name '\''
    '?' : 'P' : '<' : _ -> string "?P<" *> name '>'
    '?' : 'P' : c : _   -> notYet (T.pack ['(', '?', 'P', c])
    '?' : c : _
      | c `elem` ("=!>|(" :: String) -> notYet (T.pack ['(', '?', c])
      | c == 'R' || c == '&' || c == '+' || isDigit c -> notYet "(?R"
      | c == 'C' -> notYet "(?C"
    '?' : _             -> notYet "(?flags)"  -- (?i) (?-m) (?s: ...)
    '*' : _             -> notYet "(*"
    _                   -> pure Numbered
  where
    notYet = unsupported at

-- ANCHOR: names
-- 📛 A group's name, up to its end mark. A letter or _ first, then
--    letters, digits and _ (Unicode ones too, as PCRE2 with UTF).
--    At most 128 bytes in UTF-8; no name twice.
-- グループの名前を、終わりの印まで。頭は文字か _、そのあとは文字・
-- 数字・_（PCRE2 の UTF と同じく、Unicode の文字も）。UTF-8 で
-- 128 バイトまで。同じ名前は2回使えない
name :: Char -> Parser Capture
name close = do
  n <- nameText close
  seen <- gets (Set.member n . seenNames)
  after <- getOffset
  when seen $
    invalid after
      "two named subpatterns have the same name (PCRE2_DUPNAMES not set)"
      -- 143
  modify' (\r -> r { seenNames = Set.insert n (seenNames r) })
  pure (Named n)

-- 📛 Just the name, up to its end mark (and the mark).
-- 名前だけを、終わりの印まで（印も読む）
nameText :: Char -> Parser Text
nameText close = do
  at <- getOffset
  first <- lookAhead (option close anySingle)
  when (isNameDigit first) $
    invalid (at + 1) "subpattern name must start with a non-digit"  -- 144
  n <- takeWhileP Nothing isNameChar
  nameEnd <- getOffset
  when (T.null n) $ invalid at "subpattern name expected"  -- 162
  when (lengthWord8 n > 128) $
    invalid nameEnd "subpattern name is too long (maximum 128 code units)"
      -- 148
  when (close == '}') blanks
  closed <- option False (True <$ char close)
  when (not closed) $
    invalid nameEnd "syntax error in subpattern name (missing terminator?)"
      -- 142
  pure n
  where
    isNameChar c  = c == '_' || isLetter c || isDigit c || isNameDigit c
    isNameDigit c = generalCategory c == DecimalNumber
-- ANCHOR_END: names
-- ANCHOR_END: group

-- ANCHOR: reference
-- ANCHOR: refentry
-- 🔗 A back reference: \1, \g{1}, \g{-1}, \k<name> and the like.
--    Anything else that starts with \ is not one: fail, having read
--    nothing, so that `escaped` can try.
-- 後方参照：\1・\g{1}・\g{-1}・\k<name> など。\ で始まる、ほかのものは
-- 後方参照ではない。何も読まずに失敗して、escaped に任せる
reference :: Parser Atom
reference = do
  at <- getOffset
  ahead <- lookAhead (takeWhileUpTo 2 (const True))
  case ahead of
    ['\\', d] | d >= '1' && d <= '9' -> char '\\' *> digitsRef
    "\\g" -> string "\\g" *> gRef at
    "\\k" -> string "\\k" *> kRef
    _     -> fail "not a reference"

-- 🔢 \1 to \9 are always references. \10 and longer ones are, when
--    they start with 8 or 9, or when that many groups have opened
--    before here (as PCRE2 does: a group after it does not count); if
--    not, up to three octal digits (\12 is a newline).
-- \1 から \9 は、いつも後方参照。\10 より長いものは、8 か 9 で
-- 始まるか、ここまでにその数だけグループが開いていれば後方参照
-- （PCRE2 と同じ。うしろのグループは数えない）。なければ、3つまでの
-- 8進の数字（\12 は改行）
digitsRef :: Parser Atom
digitsRef = do
  ds <- lookAhead (takeWhileP Nothing isDigit)
  o <- gets opened
  let n = readBase 10 (T.unpack ds) :: Integer
      isReference = n < 10 || T.head ds `elem` ("89" :: String)
                      || n <= toInteger o
  if isReference
    then do
      _ <- takeWhileP Nothing isDigit
      end <- getOffset
      tooBig end n
      byNumber end n
    else do
      os <- takeWhileUpTo 3 isOctDigit
      pure (Lit (chr (readBase 8 os)))

-- ANCHOR_END: refentry

-- ANCHOR: refnamed
-- 🏷️ \k<name>, \k'name' and \k{name}.
-- \k<name>・\k'name'・\k{name}
kRef :: Parser Atom
kRef = do
  afterK <- getOffset
  next <- lookAhead (option '\0' anySingle)
  case lookup next [('<', '>'), ('\'', '\''), ('{', '}')] of
    -- \k{ name } may have spaces inside the braces
    -- \k{ name } は、かっこの内側に空白を置ける
    Just '}'   -> anySingle *> blanks *> namedRef '}'
    Just close -> anySingle *> namedRef close
    Nothing    -> invalid afterK
      "\\k is not followed by a braced, angle-bracketed, or quoted name"
      -- 169

-- 🐍 (?P=name), the way Python writes it.
-- (?P=name)。Python の書き方
pythonRef :: Parser Atom
pythonRef = do
  _ <- try (string "(?P=")
  namedRef ')'

-- 📛 A name, up to `close`; PCRE2 points at the start of the name when
--    there is no such group.
-- close までの名前。そのグループがなければ、PCRE2 は名前の頭を指す
namedRef :: Char -> Parser Atom
namedRef close = do
  start <- getOffset
  n <- nameText close
  byName start n

-- ANCHOR_END: refnamed

-- ANCHOR: refg
-- 🔢 \g1 \g-1 \g+1 \g{1} \g{-1} \g{+1} \g{name}. \g<1> and \g'1' are
--    not references but calls of a group (the chapter on recursion).
-- \g1 \g-1 \g+1 \g{1} \g{-1} \g{+1} \g{name}。\g<1> と \g'1' は
-- 後方参照ではなく、グループの呼び出し（再帰の章で）
gRef :: Int -> Parser Atom
gRef at = do
  afterG <- getOffset
  next <- lookAhead (option '\0' anySingle)
  case next of
    '{' -> anySingle *> braced afterG
    _ | next `elem` ("<'" :: String) -> unsupported at "\\g<"
      | isDigit next || next `elem` ("+-" :: String) -> do
          (sign, n) <- signedNumber afterG
          end <- getOffset
          tooBig end n
          zeroNotAllowed end sign n
          relative afterG end sign n
      | otherwise -> noNumber afterG
  where
    braced afterG = do
      blanks
      two <- lookAhead (takeWhileUpTo 2 (const True))
      if numberAhead two
        then do
          (sign, n) <- signedNumber afterG
          tooBig afterG n
          zeroNotAllowed afterG sign n
          blanks
          numEnd <- getOffset
          closed <- option False (True <$ char '}')
          when (not closed) $
            invalid numEnd
              "syntax error in subpattern number (missing terminator?)"
              -- 219
          end <- getOffset
          -- (PCRE2 points just after \g for \g{-2}, after } for the rest)
          -- （\g{-2} なら \g のすぐうしろ、ほかは } のうしろを指す）
          relative afterG (if sign == Just '-' then afterG else end) sign n
        else namedRef '}'
    numberAhead (d : _) | isDigit d = True
    numberAhead [s, d] = s `elem` ("+-" :: String) && isDigit d
    numberAhead _ = False

-- 🔢 A number, maybe with + or - before it.
-- 数。前に + か - が付くこともある
signedNumber :: Int -> Parser (Maybe Char, Integer)
signedNumber afterG = do
  sign <- option Nothing (Just <$> satisfy (`elem` ("+-" :: String)))
  ds <- takeWhileP Nothing isDigit
  when (T.null ds) $ noNumber afterG
  pure (sign, readBase 10 (T.unpack ds))

-- 🚫 A group number bigger than 65535 (PCRE2's largest). PCRE2 points
--    just after \g for \g{…}, and after the digits for \g123456.
-- 65535（PCRE2 のいちばん大きい番号）より大きいグループの番号。PCRE2 は、
-- \g{…} なら \g のすぐうしろ、\g123456 なら数字のうしろを指す
tooBig :: Int -> Integer -> Parser ()
tooBig at n =
  when (n > 65535) $ invalid at "subpattern number is too big"  -- 161

-- 🚫 +0 and -0 point nowhere. PCRE2 says it when it has read the
--    number: just after \g for \g{-0}, after the digits for \g-0.
-- +0 と -0 は、どこも指さない。PCRE2 は数を読んだところで言う：\g{-0}
-- なら \g のすぐうしろ、\g-0 なら数字のうしろ
zeroNotAllowed :: Int -> Maybe Char -> Integer -> Parser ()
zeroNotAllowed at sign n =
  when (sign /= Nothing && n == 0) $
    invalid at "a relative value of zero is not allowed"  -- 126

noNumber :: Int -> Parser a
noNumber afterG = invalid afterG
  "\\g is not followed by a braced, angle-bracketed, or quoted name/number\
  \ or by a plain number"  -- 157

-- ➕ -1 is the last group opened before here, +1 the next one.
-- -1 は、ここより前に開いた最後のグループ、+1 は次のグループ
relative :: Int -> Int -> Maybe Char -> Integer -> Parser Atom
relative afterG end sign n = do
  o <- toInteger <$> gets opened
  case sign of
    Nothing           -> byNumber end n
    Just _ | n == 0   ->
      invalid afterG "a relative value of zero is not allowed"  -- 126
    Just '-'          -> byNumber end (o + 1 - n)
    Just _            -> byNumber end (o + n)

-- ANCHOR_END: refg

-- ANCHOR: refcheck
-- ✅ The group with this number, if there is one. The first reading
--    does not know yet, and only counts (Backref 0 is thrown away).
--    `end` is the place PCRE2 gives.
-- この番号のグループがあれば、それ。1回目の読みでは、まだわからないので
-- 数えるだけ（Backref 0 は捨てる）。end は PCRE2 が言う場所
byNumber :: Int -> Integer -> Parser Atom
byNumber end n = do
  k <- gets known
  case k of
    Nothing -> pure (Backref 0)
    Just kn
      | n >= 1 && n <= toInteger (knownCount kn) ->
          pure (Backref (fromInteger n))
      | otherwise -> nonExistent end

-- ✅ The group with this name, if there is one.
-- この名前のグループがあれば、それ
byName :: Int -> Text -> Parser Atom
byName start n = do
  k <- gets known
  case k of
    Nothing -> pure (Backref 0)
    Just kn -> maybe (nonExistent start) (pure . Backref)
                 (lookup n (knownNames kn))

nonExistent :: Int -> Parser a
nonExistent at = invalid at "reference to non-existent subpattern"  -- 115
-- ANCHOR_END: refcheck
-- ANCHOR_END: reference

-- ANCHOR: quantifier
-- ANCHOR: quantifierfn
-- 🔁 A quantifier, and after it ? (Lazy) or + (Possessive). A comment
--    or an empty \Q\E may come between them: a*(?#c)? is a*?.
-- 量指定子と、そのうしろの ?（控えめ）か +（所有的）。あいだに
-- コメントや、空の \Q\E があってもよい：a*(?#c)? は a*?
quantifier :: Parser Quantifier
quantifier = do
  c <- quantifierBase
  -- a?\E+ : PCRE2 10.49 reads it as a?+, 10.47 as a wrong pattern
  -- a?\E+ を、PCRE2 10.49 は a?+ と読み、10.47 はまちがいとする
  at <- getOffset
  ahead <- lookAhead (takeWhileUpTo 3 (const True))
  case ahead of
    ['\\', 'E', x] | x `elem` ("?+" :: String) ->
      unsupported at "\\E after a quantifier"
    _ -> pure ()
  skipIgnorable
  g <- option Greedy ((Lazy <$ char '?') <|> (Possessive <$ char '+'))
  pure (Quantifier c g)
-- ANCHOR_END: quantifierfn

-- 🔢 * + ? or {n,m}. A { that does not have the shape of {n,m} is
--    not a quantifier (it is the character {).
-- * + ? か {n,m}。{n,m} の形でない { は、量指定子ではない（文字の {）
quantifierBase :: Parser Count
quantifierBase =
      (Count 0 Nothing  <$ char '*')
  <|> (Count 1 Nothing  <$ char '+')
  <|> (Count 0 (Just 1) <$ char '?')
  <|> braces

-- 📏 {n} {n,} {n,m} {,m}, with spaces or tabs allowed around the
--    numbers and the comma. Numbers up to 65535.
-- {n} {n,} {n,m} {,m}。数とコンマのまわりには空白とタブを置ける。
-- 数は 65535 まで
braces :: Parser Count
braces = do
  (lo, comma, hi) <- try shape
  lo' <- traverse checked lo
  hi' <- traverse checked hi
  case (lo', hi') of
    (Just (n, _), Nothing)
      | comma     -> pure (Count n Nothing)   -- {n,}
      | otherwise -> pure (Count n (Just 0))  -- {n}
    (Nothing, Just (m, _)) -> pure (Count 0 (Just m))  -- {,m}
    (Just (n, _), Just (m, at)) -> case between n m of  -- {n,m}
      Just q  -> pure (count q)
      Nothing -> invalid at "numbers out of order in {} quantifier"  -- 104
    (Nothing, Nothing) -> fail "not a quantifier"  -- shape stops this
  where
    shape = do
      _ <- char '{'
      blanks
      lo <- option Nothing (Just <$> number)
      blanks
      comma <- option False (True <$ char ',')
      hi <- if comma
              then blanks *> option Nothing (Just <$> number) <* blanks
              else pure Nothing
      _ <- char '}'
      -- {} {,} { } are not quantifiers
      -- {} {,} { } は量指定子ではない
      case (lo, hi) of
        (Nothing, Nothing) -> fail "not a quantifier"
        _                  -> pure (lo, comma, hi)
    -- the digits, and the place just after them
    -- 数字と、そのすぐうしろの場所
    number = do
      ds <- some (satisfy isDigit)
      at <- getOffset
      pure (readBase 10 ds :: Integer, at)
    -- read as an Integer, so that a long number does not wrap round
    -- Integer で読むので、長い数でも、ひと回りしない
    checked (n, at)
      | n > 65535 = invalid at "number too big in {} quantifier"  -- 105
      | otherwise = pure (fromInteger n, at)
-- ANCHOR_END: quantifier
-- ANCHOR_END: all
