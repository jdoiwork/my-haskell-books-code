-- ANCHOR: all
-- | Read a pattern into the tree of MiniRegex.Syntax, the way PCRE2
--   reads it. A wrong pattern stops with PCRE2's message, at the place
--   PCRE2 gives. A right pattern with something the tree has no place
--   for yet (recursion, conditions, ...) stops as Unsupported.
-- パターンを読んで、MiniRegex.Syntax の木にする。読み方は PCRE2 と同じ。
-- まちがったパターンは、PCRE2 と同じ文と場所で止まる。正しいけれど、
-- 木にまだ居場所のないもの（再帰、条件など）は Unsupported で止まる
module MiniRegex.Parser
  ( parsePattern
  , Parser
  , Reading (..)
  , Known (..)
  , Flag (..)
  ) where

-- ANCHOR: imports
-- The parser, and what it keeps while reading
-- パーサーと、読みながら覚えておくもの
import Control.Monad (unless, when)
import Control.Monad.State.Strict (State, evalState, gets, modify')
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec
  ( ParsecT, ParseErrorBundle (..), ParseError (..), ErrorFancy (..)
  , runParserT, customFailure, getOffset, atEnd, eof, try, lookAhead
  , anySingle, satisfy, takeWhileP, option, many, some, (<|>)
  , notFollowedBy, getInput, empty )
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
import MiniRegex.Unicode (caseVariants, isPatternSpace, posixNames,
  propertyNamed)
import MiniRegex.Width (Width (..), branchWidth, hasBackref)
-- ANCHOR_END: imports

-- ANCHOR: parser
-- 🧰 A parser of Text that stops with a RegexError, and keeps a
--    Reading.
-- Text を読むパーサー。止まるときは RegexError で。Reading を覚えておく
type Parser = ParsecT RegexError Text (State Reading)

-- ANCHOR: reading
-- 📒 What the parser keeps while it reads: the names of the groups so
--    far (to find a name used twice), how many groups have opened so
--    far (for \g{-1}), and what the first reading found out about the
--    whole pattern (Nothing while it is the first reading). The flags
--    in effect here, and whether we are in (?| ... ). From this
--    chapter, whether we are in a look, whether we saw \K in a look,
--    and the troubles with lookbehinds (PCRE2 tells them only after
--    reading it all).
-- パーサーが読みながら覚えておくもの：ここまでのグループの名前（同じ
-- 名前が2回出てきたら、わかるように）、ここまでに開いたグループの数
-- （\g{-1} のため）、1回目の読みでわかった、パターン全体のこと
-- （1回目の読みのあいだは Nothing）。この場所で効いているフラグと、
-- (?| ... ) の中にいるか。この章から、「見る」ものの中にいるか、
-- \K を「見る」ものの中で見たか、後読みの困りごと（PCRE2 は、
-- 読み終えてから言う）
data Reading = Reading
  { seenNames  :: Set Text
  , opened     :: Int
  , known      :: Maybe Known
  , flags      :: Set Flag
  , inReset    :: Bool
  , inLook     :: Bool
  , keepInLook :: Bool
  , troubles   :: [RegexError]
  }
-- ANCHOR_END: reading

-- 🔭 The whole pattern, from the first reading: how many groups, and
--    the numbers of the names. \k<year>(?<year>...) refers to a group
--    that comes later.
-- 1回目の読みでわかった、パターン全体のこと：グループの数と、名前の
-- 番号。\k<year>(?<year>...) は、うしろのグループを指している
data Known = Known
  { knownCount :: Int
  , knownNames :: [(Text, Int)]
  }

-- ANCHOR: flags
-- 🚩 The flags that can change inside a pattern: (?i) (?m) (?n) (?s)
--    (?x) (?xx) (?U), and the ASCII ones (?aD) (?aS) (?aW) (?aP) (?aT).
-- パターンの途中で変えられるフラグ：(?i) (?m) (?n) (?s) (?x) (?xx) (?U)
-- と、ASCII にするもの (?aD) (?aS) (?aW) (?aP) (?aT)
data Flag
  = I | M | N | S | X | XX | U   -- caseless, multiline ... ungreedy
  | AD | AS | AW | AP | AT       -- \d \s \w POSIX, and [:digit:]
  deriving (Show, Eq, Ord, Enum, Bounded)

-- 🚩 The flags from compileWith, at the start of the pattern.
-- compileWith のフラグ。パターンの頭で効いている
startFlags :: [Option] -> Set Flag
startFlags = Set.fromList . concatMap one
  where
    one Caseless  = [I]
    one Multiline = [M]
    one DotAll    = [S]
    one Extended  = [X]
    one Ascii     = asciiFlags

asciiFlags :: [Flag]
asciiFlags = [AD, AS, AW, AP, AT]

-- ❓ Is this flag in effect here?
-- このフラグは、ここで効いているか
on :: Flag -> Parser Bool
on f = gets (Set.member f . flags)

-- 🌏 \d, \s, \w ... reach all of Unicode, unless the flag says ASCII.
-- \d・\s・\w などは、フラグが ASCII と言わなければ、Unicode の全部に届く
reachOf :: Flag -> Parser Reach
reachOf f = do
  ascii <- on f
  pure (if ascii then AsciiOnly else AllUnicode)
-- ANCHOR_END: flags

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
--    the whole pattern first). The flags are written into the tree as
--    it is read.
-- パターンを2回読む。1回目で、まちがったパターンを見つけ、グループと
-- 名前を数える。2回目は、それを知ったうえで、\k<name> を番号にし、
-- ないグループを指していたら断る（PCRE2 も、先にパターン全体を
-- 見渡す）。フラグは、どれも読みながら木に書き込む
parsePattern :: [Option] -> Text -> Either RegexError Pattern
parsePattern opts src = do
  first <- readOnce Nothing
  readOnce (Just (Known (groupCount first) (groupNames first)))
  where
    start k = Reading Set.empty 0 k (startFlags opts) False False False []
    readOnce k = case evalState run (start k) of
      Right tree   -> Right tree
      Left  bundle -> Left (firstError bundle)
    run = runParserT (whole <* eof) "pattern" src

-- ANCHOR: whole
-- 🚩 The pattern, and then the end. A ) here has no ( before it.
--    Then what PCRE2 tells only after reading it all: a lookbehind it
--    cannot measure (the first one), and then \K in a look.
-- パターン全体と、その終わり。ここに来た ) には、対になる ( がない。
-- そのあと、PCRE2 が読み終えてから言うもの：長さを測れない後読み
-- （最初のもの）、それから「見る」ものの中の \K
whole :: Parser Pattern
whole = do
  _ <- many leadingVerb
  tree <- pattern
  at <- getOffset
  end <- atEnd
  unless end $
    invalid (at + 1) "unmatched closing parenthesis"  -- 122
  ts <- gets troubles
  case firstTrouble ts of
    Just e  -> customFailure e
    Nothing -> pure ()
  bsk <- gets keepInLook
  when bsk $
    invalid at ("\\K is not allowed in lookarounds"   -- 199
                <> " (but see PCRE2_EXTRA_ALLOW_LOOKAROUND_BSK)")
  pure tree
  where
    -- a wrong one first, the one most to the left
    -- まちがいが先。いちばん左のもの
    firstTrouble ts = case ([e | e@Invalid{} <- ts], ts) of
      (e : es, _) -> Just (minimum (e : es))
      ([], e : _) -> Just e
      _           -> Nothing
-- ANCHOR_END: whole

-- 🏁 (*UTF) and (*UCP), only at the very start. MiniRegex always reads
--    Text as Unicode; (*UCP) takes the ASCII flags away.
-- (*UTF) と (*UCP)。パターンのいちばん頭にだけ書ける。MiniRegex は、
-- いつも Text を Unicode として読む。(*UCP) は ASCII のフラグを外す
leadingVerb :: Parser ()
leadingVerb = utf <|> ucp
  where
    utf = () <$ try (string "(*UTF)")
    ucp = do
      _ <- try (string "(*UCP)")
      modify' (\r -> r { flags = flags r `Set.difference`
                                    Set.fromList asciiFlags })

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
piece = anchorPiece <|> keepPiece <|> settingPiece <|> quantifiedPieces
  where
    anchorPiece = do
      a <- anchor
      skipIgnorable
      noQuantifier
      pure [Anchor a]
    -- ANCHOR: keeppiece
    -- \K: not in a look (PCRE2 tells it at the end), and it cannot
    -- be repeated
    -- \K：「見る」ものの中には書けない（PCRE2 は最後に言う）。
    -- くり返せない
    keepPiece = do
      _ <- try (string "\\K")
      looking <- gets inLook
      when looking $ modify' (\r -> r { keepInLook = True })
      skipIgnorable
      noQuantifier
      pure [KeepOut]
    -- ANCHOR_END: keeppiece
    -- (?i) and the like leave nothing in the tree; they change the
    -- flags for the rest of the group
    -- (?i) などは、木に何も残さない。グループの残りのフラグを変える
    settingPiece = do
      _ <- try (lookAhead settingShape)
      at <- getOffset
      _ <- string "(?"
      change <- optionLetters at
      _ <- char ')'
      modify' (\r -> r { flags = change (flags r) })
      skipIgnorable
      noQuantifier
      pure []
    -- ((?P is Python's way of writing names, not a flag)
    -- （(?P は Python の名前の書き方で、フラグではない）
    settingShape =
      string "(?" *> notFollowedBy (char 'P')
        *> takeWhileP Nothing isOptionChar *> char ')'
    quantifiedPieces = do
      atoms <- atomsP >>= traverse caseAware
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

-- ANCHOR: anchors
-- ⚓ ^ and $ ask the flag m which they are. \A, \z and \Z never do.
--    \b and \B ask the flag aW, as \w does.
-- ^ と $ は、フラグ m に、どちらになるかを聞く。\A・\z・\Z は聞かない。
-- \b と \B は、\w と同じく、フラグ aW に聞く
anchor :: Parser Anchor
anchor = do
  m <- on M
  w <- reachOf AW
  choice' [ (if m then LineStart else TextStart)   <$ char '^'
          , (if m then LineEnd   else LastLineEnd) <$ char '$'
          , TextStart   <$ try (string "\\A")
          , TextEnd     <$ try (string "\\z")
          , LastLineEnd <$ try (string "\\Z")
          , WordBoundary w    <$ try (string "\\b")
          , NotWordBoundary w <$ try (string "\\B") ]
  where
    choice' = foldr (<|>) empty
-- ANCHOR_END: anchors

-- ANCHOR: caseaware
-- 🔡 Under (?i), a letter, a class or a back reference does not tell
--    A from a. A character with no other case (1, 日) stays as it is.
--    In a class, [:upper:] and [:lower:] (ASCII) become [:alpha:], and
--    \p{Lu}, \p{Ll} and \p{Lt} become \p{Lc}, as PCRE2 does.
-- (?i) のもとでは、文字・文字の集まり・後方参照が、A と a を区別しない。
-- ほかの形のない文字（1・日）は、そのまま。集まりの中では、PCRE2 と
-- 同じく、[:upper:] と [:lower:]（ASCII）は [:alpha:] に、\p{Lu}・
-- \p{Ll}・\p{Lt} は \p{Lc} になる
caseAware :: Atom -> Parser Atom
caseAware a = do
  i <- on I
  pure (if i then noCase a else a)
  where
    noCase x = case x of
      Lit c | length (caseVariants c) > 1 -> NoCase x
      Class cls -> NoCase (Class cls { items = fmap item (items cls) })
      Backref _ -> NoCase x
      _         -> x
    item (Builtin b (Posix AsciiOnly n))
      | n `elem` ["upper", "lower"] = Builtin b (Posix AsciiOnly "alpha")
    item (Builtin b (Property (Category n)))
      | n `elem` ["Lu", "Ll", "Lt"] = Builtin b (Property (Category "Lc"))
    item other = other
-- ANCHOR_END: caseaware

-- ANCHOR: ignorable
-- 💤 Things that leave nothing in the tree: a comment (?#...), a lone
--    \E, and an empty \Q\E. PCRE2 reads past them, so a* and a(?#c)*
--    are the same. Under (?x), spaces and # comments too.
-- 木に何も残さないもの：コメント (?#...)、ひとりの \E、空の \Q\E。
-- PCRE2 は読み飛ばすので、a* と a(?#c)* は同じ。(?x) のもとでは、空白と
-- # のコメントも
skipIgnorable :: Parser ()
skipIgnorable = () <$ many ignorable
  where
    ignorable = comment <|> try (string "\\E" >> pure ())
                        <|> try emptyQuote
                        <|> extendedSpace
    emptyQuote = string "\\Q" >> (() <$ string "\\E" <|> eof)
    comment = do
      _ <- try (string "(?#")
      _ <- takeWhileP Nothing (/= ')')
      closed <- option False (True <$ char ')')
      at <- getOffset
      when (not closed) $
        invalid at "missing ) after (?# comment"  -- 118

-- 💤 With (?x): spaces, and # up to the end of the line.
-- (?x) のもとでは：空白と、# から行の終わりまで
extendedSpace :: Parser ()
extendedSpace = do
  x <- on X
  if not x
    then empty
    else (() <$ satisfy isPatternSpace)
           <|> (char '#' *> takeWhileP Nothing (/= '\n') *> pure ())
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
    '.'  -> anySingle *> dot
    '['  -> Class <$> charClass
    '('  -> group
    '\\' -> anyNewline <|> reference <|> typeEscape
              <|> (Lit <$> escaped OutsideClass)
    _ | c == '|' || c == ')' -> fail "the end of a branch"
      -- ^ and $ are pieces; * + ? and {n,m} were caught by
      -- noQuantifier. Anything else is the character itself
      -- (] and }, and a { that is not a quantifier, too).
      -- ^ と $ は部品の側で、* + ? {n,m} は noQuantifier が止めた。
      -- 残りは、その文字そのもの（] と }、量指定子でない { も）
      | otherwise -> Lit <$> anySingle
  where
    -- . asks the flag s whether it takes a newline, too
    -- . は、改行も食べるかを、フラグ s に聞く
    dot = do
      s <- on S
      pure (if s then Anything else AnyChar)
-- ANCHOR_END: atom

-- ANCHOR: newline
-- ↩️ \R: any one newline, \r\n taken as one. Inside, it is an atomic
--    group, so that \r\n is never taken back as \r alone.
-- \R：どれかの改行1つ。\r\n は1つと数える。中身はアトミックグループ
-- なので、\r\n を \r だけに戻すことはない
anyNewline :: Parser Atom
anyNewline = Atomic newlines <$ try (string "\\R")
  where
    newlines = Pattern (fmap chars ("\r\n" :| ["\n", "\v", "\f", "\r",
                                               "\x85", "\x2028", "\x2029"]))
    chars cs = Branch [Quantified (Lit c) once | c <- cs]
-- ANCHOR_END: newline

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
    -- \N{U+263a}: a character by its Unicode number
    -- \N{U+263a}：Unicode の番号で書いた文字
    'N' -> unicodeName at place
    '0' -> octalDigits
    'c' -> control at
    -- [\1] is the character 1 (octal), not a reference
    -- [\1] は1番の文字（8進）で、後方参照ではない
    _ | place == InsideClass && c >= '1' && c <= '7' -> do
          ds <- takeWhileUpTo 2 isOctDigit
          pure (chr (readBase 8 (c : ds)))
      | c `elem` ("ijmqyIJMOTY" :: String) ->
          invalid (at + 2) "unrecognized character follows \\"  -- 103
      | place == InsideClass && c `elem` ("BRX" :: String) ->
          invalid (at + 2) "escape sequence is invalid in character class"
            -- 107
      | c `elem` ("lLuUF" :: String) ->
          invalid (at + 2)
            "PCRE2 does not support \\F, \\L, \\l, \\N{name}, \\U, or \\u"
            -- 137
      | otherwise -> unsupported at what  -- \b \G \K \R \X ...

-- 🔢 \N{U+263a}. \N{name} is not in PCRE2; \N alone (any character
--    but a newline) cannot be in a class.
-- \N{U+263a}。\N{name} は PCRE2 にない。\N だけ（改行のほかの1文字）は、
-- [ ] の中に書けない
unicodeName :: Int -> Place -> Parser Char
unicodeName at place = do
  brace <- option False (True <$ char '{')
  when brace blanks
  hasU <- option False (True <$ try (string "U+"))
  case () of
    _ | brace && hasU -> inBraces 16 isHexDigit
          "non-hex character in \\x{} (closing brace missing?)"  -- 167
      | brace -> invalid (at + 3)
          "PCRE2 does not support \\F, \\L, \\l, \\N{name}, \\U, or \\u"
          -- 137
      | place == InsideClass ->
          invalid (at + 2) "\\N is not supported in a class"  -- 171
      | otherwise -> fail "\\N is any character but a newline"
-- ANCHOR_END: escape

-- ANCHOR: types
-- 🧺 \d \D \w \W \s \S \h \H \v \V \p \P \N outside [ ]: an atom.
--    All but \N are a class of one item.
-- [ ] の外の \d \D \w \W \s \S \h \H \v \V \p \P \N：アトム。
-- \N のほかは、項目が1つの文字の集まり
typeEscape :: Parser Atom
typeEscape = do
  two <- lookAhead (takeWhileUpTo 3 (const True))
  case two of
    -- \N{2,3} is \N, repeated; \N{U+263a} is a character
    -- \N{2,3} は \N のくり返し。\N{U+263a} は文字
    -- (a wrong count, like \N{5,2}, is wrong just after \N)
    -- （\N{5,2} のような、まちがった数は、\N のすぐうしろでまちがい）
    ['\\', 'N', '{'] -> do
      shape <- option Nothing
        (Just <$> try (lookAhead (string "\\N" *> braceShape)))
      case shape of
        Nothing -> fail "a character"
        Just (lo, _, hi) -> do
          at <- string "\\N" *> getOffset
          let numbers = [n | Just (n, _) <- [lo, hi]]
          when (any (> 65535) numbers) $
            invalid at "number too big in {} quantifier"  -- 105
          case numbers of
            [n, m] | n > m ->
              invalid at "numbers out of order in {} quantifier"  -- 104
            _ -> pure AnyChar
    '\\' : 'N' : _   -> AnyChar <$ string "\\N"
    '\\' : c : _ | c `elem` typeLetters -> do
      item <- typeItem
      pure (Class (CharClass False (item :| [])))
    _ -> fail "not a type of character"

typeLetters :: String
typeLetters = "dDwWsShHvVpP"

-- 🧺 One of \d \D \w \W \s \S \h \H \v \V \p{...} \P{...}, as an
--    item of a class. The flags (?aD) (?aS) (?aW) say whether \d \s \w
--    are ASCII only.
-- \d \D \w \W \s \S \h \H \v \V \p{...} \P{...} を、文字の集まりの
-- 項目として。\d \s \w が ASCII だけかは、フラグ (?aD) (?aS) (?aW) が言う
typeItem :: Parser ClassItem
typeItem = do
  at <- getOffset
  _ <- char '\\'
  c <- satisfy (`elem` typeLetters)
  let opposite = isUpperAscii c
  case c of
    _ | c `elem` ("dD" :: String) -> Builtin opposite . Digit <$> reachOf AD
      | c `elem` ("sS" :: String) ->
          Builtin opposite . WhiteSpace <$> reachOf AS
      | c `elem` ("wW" :: String) -> Builtin opposite . Word  <$> reachOf AW
      | c `elem` ("hH" :: String) -> pure (Builtin opposite HSpace)
      | c `elem` ("vV" :: String) -> pure (Builtin opposite VSpace)
      | otherwise                 -> property at opposite
  where
    isUpperAscii c = c >= 'A' && c <= 'Z'
-- ANCHOR_END: types

-- ANCHOR: property
-- 🏷️ \p{Han}, \p{^Lu}, \pL. A name MiniRegex does not know is "not
--    yet" if PCRE2 knows it (\p{Alphabetic}, \p{bc:AL}), and wrong if
--    PCRE2 does not either (\p{Letter}, error 147).
-- \p{Han}・\p{^Lu}・\pL。MiniRegex の知らない名前は、PCRE2 が知って
-- いれば「まだ」（\p{Alphabetic}・\p{bc:AL}）、PCRE2 も知らなければ
-- まちがい（\p{Letter}。エラー 147）
property :: Int -> Bool -> Parser ClassItem
property at opposite = do
  next <- option Nothing (Just <$> anySingle)
  case next of
    Nothing  -> malformed
    -- PCRE2 reads only the ASCII from & to z, and the spaces, as a
    -- name. At any other character, } too, it stops, just after it.
    -- PCRE2 が名前として読むのは、& から z までの ASCII と空白だけ。
    -- ほかの文字に出会ったら、} でもそうでなくても、そのすぐうしろで止まる
    Just '{' -> do
      body <- takeWhileP Nothing nameChar
      after <- option Nothing (Just <$> anySingle)
      unless (after == Just '}') malformed
      let trimmed = T.dropWhile (`elem` (" \t\n\v\f\r" :: String)) body
          (hat, written) = case T.stripPrefix "^" trimmed of
            Just rest -> (True, rest)
            Nothing   -> (False, body)
      named (opposite /= hat) written
    -- \pL: one ASCII letter, without braces
    -- \pL：かっこなしの、ASCII の文字1つ
    Just c
      | isAscii c && isLetter c -> named opposite (T.singleton c)
      | otherwise               -> malformed
  where
    nameChar c = (c >= '&' && c <= 'z') || c `elem` (" \t\n\v\f\r" :: String)
    malformed = do
      end <- getOffset
      invalid end "malformed \\P or \\p sequence"  -- 146
    named neg written = do
      end <- getOffset
      case propertyNamed written of
        Just p -> pure (Builtin neg (Property p))
        Nothing
          | pcre2Knows written -> unsupported at "\\p{binary property}"
          | otherwise ->
              invalid end "unknown property after \\P or \\p"  -- 147

-- 📋 The names PCRE2 10.49 knows that MiniRegex does not: the
--    yes-or-no properties (pcre2test -LP), and Bidi_Class.
-- PCRE2 10.49 が知っていて、MiniRegex が知らない名前：はい・いいえの
-- 性質（pcre2test -LP）と、Bidi_Class
pcre2Knows :: Text -> Bool
pcre2Knows written =
  loose `elem` binaryProperties
    || any (`T.isPrefixOf` loose) ["bc:", "bidiclass:"]
  where
    loose = T.toLower (T.filter (`notElem` (" \t\n\v\f\r-_" :: String))
                                (T.replace "=" ":" written))

binaryProperties :: [Text]
binaryProperties = T.words
  "ahex alpha alphabetic ascii asciihexdigit bidic bidicontrol bidim \
  \bidimirrored cased caseignorable changeswhencasefolded \
  \changeswhencasemapped changeswhenlowercased changeswhentitlecased \
  \changeswhenuppercased ci cwcf cwcm cwl cwt cwu dash \
  \defaultignorablecodepoint dep deprecated di dia diacritic ebase ecomp \
  \emod emoji emojicomponent emojimodifier emojimodifierbase \
  \emojipresentation epres ext extendedpictographic extender extpict \
  \graphemebase graphemeextend graphemelink grbase grext grlink hex \
  \hexdigit idc idcompatmathcontinue idcompatmathstart idcontinue ideo \
  \ideographic ids idsb idsbinaryoperator idst idstart idstrinaryoperator \
  \idsu idsunaryoperator incb joinc joincontrol loe \
  \logicalorderexception lower lowercase math mcm modifiercombiningmark \
  \nchar noncharactercodepoint patsyn patternsyntax patternwhitespace \
  \patws pcm prependedconcatenationmark qmark quotationmark radical \
  \regionalindicator ri sd sentenceterminal softdotted space sterm term \
  \terminalpunctuation uideo unifiedideograph upper uppercase \
  \variationselector vs whitespace wspace xidc xidcontinue xids xidstart"
-- ANCHOR_END: property

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
  -- under (?xx), [ ^a] is [^a]: the spaces first, then the ^
  -- (?xx) のもとでは、[ ^a] は [^a]：空白を先に読み飛ばしてから ^ を見る
  classSpace
  neg <- option False (True <$ char '^')
  classSpace
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

-- 🔤 a, a-z, \d or [:alpha:]. A - just before the ] is the character
--    -. A range cannot start or end with \d or [:alpha:] (error 150).
--    Under (?xx), spaces and tabs between the items are skipped.
-- a・a-z・\d・[:alpha:]。] のすぐ前の - は、文字の -。\d や [:alpha:] は、
-- 範囲の端になれない（エラー 150）。(?xx) のもとでは、項目のあいだの
-- 空白とタブを読み飛ばす
classItem :: Parser Char -> Parser ClassItem
classItem first = do
  noEnd
  named <- option Nothing (Just <$> (posixItem <|> typeInClass))
  item <- case named of
    Just it -> do
      -- [\d-z]: a - that is not just before ] is a wrong range
      -- [\d-z]：] のすぐ前でない - は、まちがった範囲
      wrongDash <- option False
        (True <$ try (lookAhead (char '-' *> classSpace *> notClose)))
      at <- getOffset
      when wrongDash $
        invalid (at + 1) "invalid range in character class"  -- 150
      pure it
    Nothing -> do
      lo <- first
      classSpace
      dash <- option False
        (True <$ try (char '-' <* lookAhead (classSpace *> notClose)))
      if not dash
        then pure (Single lo)
        else do
          classSpace
          endsWithName <- option False (True <$ lookAhead
                            (posixItem <|> typeInClass))
          when endsWithName $ do
            _ <- posixItem <|> typeInClass
            at <- getOffset
            invalid at "invalid range in character class"  -- 150
          hi <- classChar
          at <- getOffset
          when (hi < lo) $
            invalid at "range out of order in character class"  -- 108
          pure (Range lo hi)
  classSpace
  pure item
  where
    notClose = satisfy (/= ']')
    -- a [ with nothing after it is missing its ]
    -- うしろに何もない [ には、] がない
    noEnd = do
      end <- atEnd
      at <- getOffset
      when end $
        invalid at "missing terminating ] for character class"  -- 106
    typeInClass = try (lookAhead (char '\\' *> satisfy (`elem` typeLetters)))
                    *> typeItem

-- 💤 Under (?xx), spaces and tabs in [ ] are skipped.
-- (?xx) のもとでは、[ ] の中の空白とタブを読み飛ばす
classSpace :: Parser ()
classSpace = do
  xx <- on XX
  when xx $ () <$ takeWhileP Nothing (`elem` (" \t" :: String))

-- 🏛️ [:alpha:] and [:^alpha:] in a class. With (?aP), ASCII only;
--    [:digit:] and [:xdigit:] listen to (?aT), too. A [ that is not
--    followed by the whole shape is the character [.
-- 文字の集まりの中の [:alpha:] と [:^alpha:]。(?aP) なら ASCII だけ。
-- [:digit:] と [:xdigit:] は (?aT) も聞く。形が最後までそろっていない [ は、
-- 文字の [
posixItem :: Parser ClassItem
posixItem = do
  (neg, posixName) <- try shape
  at <- getOffset
  unless (posixName `elem` posixNames) $
    invalid at "unknown POSIX class name"  -- 130
  asciiP <- on AP
  asciiT <- on AT
  let ascii = asciiP || (asciiT && posixName `elem` ["digit", "xdigit"])
      reach = if ascii then AsciiOnly else AllUnicode
  pure (Builtin neg (Posix reach posixName))
  where
    shape = do
      _ <- string "[:"
      neg <- option False (True <$ char '^')
      written <- takeWhileP Nothing isAsciiLetter
      _ <- string ":]"
      pure (neg, written)
    isAsciiLetter c = isAscii c && isLetter c

-- 🔤 One character in [ ... ]: itself, or a \ that means one.
-- [ ... ] の中の1文字：その文字か、1文字を表す \
classChar :: Parser Char
classChar = do
  at <- getOffset
  c <- lookAhead anySingle
  next <- lookAhead (anySingle *> option ' ' anySingle)
  case c of
    ']' -> fail "the end of the class"
    -- \Q \E, and [.ch.] and [=ch=], are for later. Read them first,
    -- then stop: a parser in `many` that stops without reading
    -- anything only ends the `many`.
    -- \Q \E と、[.ch.]・[=ch=] は、まだ。先に読んでから止まる（many の
    -- 中のパーサーが、何も読まずに止まると、many が終わるだけになる）
    '\\' | next `elem` ("QE" :: String) -> do
      _ <- anySingle
      unsupported at (T.pack ['\\', next] <> " in []")
    '\\' -> escaped InsideClass
    '[' | next `elem` (".=" :: String) -> do
      _ <- anySingle
      unsupported at "[."
    _ -> anySingle
-- ANCHOR_END: class

-- ANCHOR: group
-- ANCHOR: groupfn
-- 🫧 ( ... ), (?: ... ), (?<name> ... ), (?i: ... ), (?| ... ), (?> ... ),
--    (?= ... ) (?! ... ) (?<= ... ) (?<! ... ); and the things in (? ... )
--    that are for later chapters. A flag changed in the group goes back
--    to what it was at the ).
-- ( ... )、(?: ... )、(?<name> ... )、(?i: ... )、(?| ... )、(?> ... )、
-- (?= ... ) (?! ... ) (?<= ... ) (?<! ... )。(? ... ) のうち、あとの章で
-- 読むもの。グループの中で変えたフラグは、) で元に戻る
group :: Parser Atom
group = pythonRef <|> do
  at <- getOffset
  saved <- gets flags
  wasLooking <- gets inLook
  _ <- char '('
  kind <- groupKind at
  -- one more group has opened (for \g{-1}); or we are in a look now
  -- グループが1つ開いた（\g{-1} のため）。または「見る」ものの中に入った
  case kind of
    Keeps c | keeps c -> modify' (\r -> r { opened = opened r + 1 })
    Looks _ _         -> modify' (\r -> r { inLook = True })
    _                 -> pure ()
  inner <- if kind == Keeps BranchReset then resetPattern else pattern
  closed <- option False (True <$ char ')')
  end <- getOffset
  when (not closed) $
    invalid end "missing closing parenthesis"  -- 114
  modify' (\r -> r { flags = saved, inLook = wasLooking })
  case kind of
    Keeps c        -> pure (Group c inner)
    Holds          -> pure (Atomic inner)
    Looks Behind s -> do
      measure at inner
      pure (Look Behind s inner)
    Looks Ahead s  -> pure (Look Ahead s inner)
  where
    keeps (Named _) = True
    keeps Numbered  = True
    keeps _         = False
-- ANCHOR_END: groupfn

-- 🏷️ What a ( opens: a group that may keep what it found, an atomic
--    group, or a look.
-- ( が開くもの：見つけたものを覚えうるグループ、アトミックグループ、
-- 「見る」もの
data Opening
  = Keeps Capture
  | Holds
  | Looks Direction Sense
  deriving Eq

-- ANCHOR: measure
-- 📏 Can a lookbehind be measured? PCRE2 wants a limit for each branch:
--    up to 255 characters for a branch whose length can change, and up
--    to 65535 for a fixed one. What is wrong is told at the end.
-- 後読みの長さを測れるか。PCRE2 は、枝ごとに上限を求める：長さが変わる
-- 枝は 255 文字まで、決まった長さの枝は 65535 文字まで。まちがいは、
-- 読み終えてから言う
measure :: Int -> Pattern -> Parser ()
measure at (Pattern bs) = case trouble of
  Just e  -> modify' (\r -> r { troubles = troubles r ++ [e] })
  Nothing -> pure ()
  where
    widths = map branchWidth (NE.toList bs)
    trouble
      | any hasBackref (NE.toList bs) =
          Just (Unsupported at "back reference in lookbehind")
      | any (\w -> longest w == Nothing) widths =
          Just (Invalid at "length of lookbehind assertion is not limited")
            -- 125
      | any tooLongToChange widths =
          Just (Invalid at
            "branch too long in variable-length lookbehind assertion") -- 200
      | any (\w -> longest w > Just 65535) widths =
          Just (Invalid at "lookbehind assertion is too long")  -- 187
      | otherwise = Nothing
    tooLongToChange (Width s l) = l /= Just s && l > Just 255
-- ANCHOR_END: measure

-- ANCHOR: kind
-- 🏷️ What is just after the (.
-- ( のすぐうしろに何があるか
groupKind :: Int -> Parser Opening
groupKind at = do
  rest <- lookAhead (takeWhileUpTo 3 (const True))
  n <- on N
  case rest of
    '?' : ':' : _       -> Keeps NonCapturing <$ string "?:"
    '?' : '|' : _       -> Keeps BranchReset <$ string "?|"
    '?' : '>' : _       -> Holds <$ string "?>"
    '?' : '=' : _       -> Looks Ahead Positive <$ string "?="
    '?' : '!' : _       -> Looks Ahead Negative <$ string "?!"
    '?' : '<' : '=' : _ -> Looks Behind Positive <$ string "?<="
    '?' : '<' : '!' : _ -> Looks Behind Negative <$ string "?<!"
    -- (?<* is a look-behind that may go back (PCRE2 10.34): not yet
    -- (?<* は、あともどりできる後読み（PCRE2 10.34 から）：まだ
    '?' : '<' : '*' : _ -> notYet "(?<*"
    '?' : '<' : _       -> Keeps <$> (string "?<"  *> name '>')
    '?' : '\'' : _      -> Keeps <$> (string "?'"  *> name '\'')
    '?' : 'P' : '<' : _ -> Keeps <$> (string "?P<" *> name '>')
    -- (?P and the end: the group is not closed (PCRE2 says it there)
    -- (?P で終わり：グループが閉じていない（PCRE2 も、そこで言う）
    ['?', 'P']          -> do
      _ <- string "?P"
      end <- getOffset
      invalid end "missing closing parenthesis"  -- 114
    '?' : 'P' : '>' : _ -> notYet "(?P>"
    '?' : 'P' : _       ->
      invalid (at + 4) "unrecognized character after (?P"  -- 141
    '?' : '-' : d : _ | isDigit d -> notYet "(?R"
    '?' : c : _
      | c `elem` ("([" :: String) -> notYet (T.pack ['(', '?', c])
      | c == 'R' || c == '&' || c == '+' || isDigit c -> notYet "(?R"
      | c == 'C' -> notYet "(?C"
      | c == '*' -> notYet "(?*"
    '?' : _             -> Keeps <$> optionGroup
    '*' : _             -> notYet "(*"
    _ | n               -> pure (Keeps NonCapturing)  -- (?n): ( keeps nothing
      | otherwise       -> pure (Keeps Numbered)
  where
    notYet = unsupported at
    -- (?i: ... ): the flags change from here to the )
    -- (?i: ... )：ここから ) まで、フラグが変わる
    optionGroup = do
      _ <- char '?'
      change <- optionLetters at
      end <- atEnd
      here <- getOffset
      when end $ invalid here "missing closing parenthesis"  -- 114
      _ <- char ':'
      modify' (\r -> r { flags = change (flags r) })
      pure NonCapturing
-- ANCHOR_END: kind

-- ANCHOR: options
-- 🔤 The letters of (?im-sx) or (?^i), up to ) or :, as a change of the
--    flags. Letters after - take the flag away; ^ first takes away
--    i, m, n, s and x. xx is x and more; a is all the a-flags.
-- (?im-sx) や (?^i) の文字を、) か : まで読み、フラグの変え方にする。
-- - のうしろの文字はフラグを外す。頭の ^ は i・m・n・s・x を外す。xx は
-- x の強い版。a だけなら、a の付くフラグを全部
optionLetters :: Int -> Parser (Set Flag -> Set Flag)
optionLetters at = do
  hat <- option False (True <$ char '^')
  ons  <- letters
  offs <- option [] $ do
    here <- getOffset
    _ <- char '-'
    when hat $ invalid (here + 1) "invalid hyphen in option setting"  -- 194
    letters
  extra <- option False (True <$ lookAhead (char '-'))
  when extra $ do
    here <- getOffset
    invalid (here + 1) "invalid hyphen in option setting"  -- 194
  let start | hat       = (`Set.difference` Set.fromList [I, M, N, S, X, XX])
            | otherwise = id
      add     = Set.union (Set.fromList (concat ons))
      -- taking x away takes xx away, too; so does setting x alone
      -- x を外すと、xx も外れる。x だけを付けても、xx は外れる
      offs'   = concat offs
                  ++ [XX | X `elem` concat offs]
                  ++ [XX | X `elem` concat ons, XX `notElem` concat ons]
      remove  = (`Set.difference` Set.fromList offs')
  pure (remove . add . start)
  where
    letters = many letter
    letter = do
      c <- satisfy (\c -> c /= ')' && c /= ':' && c /= '-')
      here <- getOffset
      case c of
        'i' -> pure [I]
        'm' -> pure [M]
        'n' -> pure [N]
        's' -> pure [S]
        'U' -> pure [U]
        'x' -> do
          two <- option False (True <$ char 'x')
          pure (if two then [X, XX] else [X])
        'a' -> do
          which <- option Nothing
                     (Just <$> satisfy (`elem` ("DSWPT" :: String)))
          pure $ case which of
            Just 'D' -> [AD]
            Just 'S' -> [AS]
            Just 'W' -> [AW]
            Just 'P' -> [AP, AT]
            Just _   -> [AT]
            Nothing  -> asciiFlags
        _ | c `elem` ("Jr" :: String) ->
              unsupported at (T.pack ['(', '?', c, ')'])
          | otherwise ->
              invalid here "unrecognized character after (? or (?-"  -- 111

-- ❓ Can this character be in (?...) of flags?
-- (?...) のフラグに書ける文字か
isOptionChar :: Char -> Bool
isOptionChar c = c `elem` ("imnsxUaDSWPTJr^-" :: String)
-- ANCHOR_END: options

-- ANCHOR: reset
-- 🔁 (?| ... ): every branch numbers its groups from the same place.
--    After the group, the numbers go on from the branch that had the
--    most.
-- (?| ... )：どの枝も、グループの番号を同じところから付ける。グループの
-- あとは、いちばん多かった枝の続きから
resetPattern :: Parser Pattern
resetPattern = do
  start <- gets opened
  wasIn <- gets inReset
  modify' (\r -> r { inReset = True })
  first <- resetBranch start
  rest  <- many (char '|' *> resetBranch start)
  let most = maximum (map snd (first : rest))
  modify' (\r -> r { opened = most, inReset = wasIn })
  pure (Pattern (fmap fst (first :| rest)))
  where
    resetBranch start = do
      modify' (\r -> r { opened = start })
      b <- branch
      o <- gets opened
      pure (b, o)
-- ANCHOR_END: reset

-- ANCHOR: names
-- 📛 A group's name, up to its end mark. A letter or _ first, then
--    letters, digits and _ (Unicode ones too, as PCRE2 with UTF).
--    At most 128 bytes in UTF-8; no name twice.
-- グループの名前を、終わりの印まで。頭は文字か _、そのあとは文字・
-- 数字・_（PCRE2 の UTF と同じく、Unicode の文字も）。UTF-8 で
-- 128 バイトまで。同じ名前は2回使えない
name :: Char -> Parser Capture
name close = do
  -- (?| ... ) with names is for later
  -- 名前のある (?| ... ) は、まだ
  reset <- gets inReset
  here <- getOffset
  when reset $ unsupported here "names in (?|"
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
  -- under (?x), a+ +  is a++ (the space may come between)
  -- (?x) のもとでは、a+ + は a++（あいだに空白があってもよい）
  skipIgnorable
  g <- option Greedy ((Lazy <$ char '?') <|> (Possessive <$ char '+'))
  -- (?U) swaps greedy and lazy
  -- (?U) は、欲ばりと控えめを入れ替える
  u <- on U
  pure (Quantifier c (if u then swap g else g))
  where
    swap Greedy = Lazy
    swap Lazy   = Greedy
    swap other  = other
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
  (lo, comma, hi) <- try braceShape
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
    -- read as an Integer, so that a long number does not wrap round
    -- Integer で読むので、長い数でも、ひと回りしない
    checked (n, at)
      | n > 65535 = invalid at "number too big in {} quantifier"  -- 105
      | otherwise = pure (fromInteger n, at)

-- 📐 The shape of {n} {n,} {n,m} {,m}: the numbers, each with the
--    place just after it, and whether there is a comma
-- {n} {n,} {n,m} {,m} の形：数（それぞれ、すぐうしろの場所つき）と、
-- コンマがあるか
braceShape :: Parser (Maybe (Integer, Int), Bool, Maybe (Integer, Int))
braceShape = do
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
  where
    -- the digits, and the place just after them
    -- 数字と、そのすぐうしろの場所
    number = do
      ds <- some (satisfy isDigit)
      at <- getOffset
      pure (readBase 10 ds :: Integer, at)
-- ANCHOR_END: quantifier
-- ANCHOR_END: all
