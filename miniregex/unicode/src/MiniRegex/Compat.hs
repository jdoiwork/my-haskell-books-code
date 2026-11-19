-- ANCHOR: all
-- | The compatibility meter: read the tests of PCRE2 (the output of its
--   test tool pcre2test), run each of them with MiniRegex, and count.
-- 互換度メーター：PCRE2 のテスト（試験の道具 pcre2test の出力）を読み、
-- 1つずつ MiniRegex で動かして、数える
module MiniRegex.Compat
  ( TestCase (..)
  , Verdict (..)
  , Tally (..)
  , Modifiers (..)
  , readTests
  , readTestFile
  , judge
  , judgeWithin
  , readable
  , tally
  , flagsOf
  , decodeSubject
  , showFound
  ) where

-- ANCHOR: imports
import Data.Char (chr, digitToInt, isHexDigit, isOctDigit, ord)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Control.Exception (evaluate)
import Numeric (showHex)
import System.IO
  (IOMode (ReadMode), hGetContents', hSetEncoding, latin1, withFile)
import System.Timeout (timeout)

import MiniRegex
import MiniRegex.Parser (parsePattern)
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🧪 One test: the line number of the subject, the pattern between the
--    two /s, the modifiers after the last /, the subject as written
--    (with its \ escapes), and the answer lines that pcre2test printed
--    (" 0: the quick brown fox", or "No match").
-- テスト1つ：入力の行の番号、2つの / のあいだのパターン、最後の / の
-- うしろの修飾子、書いてあるままの入力（\ もそのまま）、pcre2test が
-- 出した答えの行（" 0: the quick brown fox" や "No match"）
data TestCase = TestCase
  { testLine      :: Int
  , testPattern   :: Text
  , testModifiers :: Text
  , testSubject   :: Text
  , testExpected  :: [Text]
  }
  deriving (Show, Eq)

-- ⚖️ What happened: the same answer, another answer, or a test that
--    needs something MiniRegex does not have yet (and what).
-- 同じ答え、ちがう答え、MiniRegex がまだ持っていないものが要るテスト
-- （と、それが何か）
data Verdict = Pass | Fail | NotYet Text
  deriving (Show, Eq)
-- ANCHOR_END: types

-- ANCHOR: read
-- 📖 Read pcre2test's output. A pattern starts with / at the head of a
--    line and may go on for some lines; the indented lines after it
--    are the subjects, each followed by its answer; a blank line ends
--    the group. #pattern and #subject set modifiers for the patterns
--    and subjects after them.
-- pcre2test の出力を読む。パターンは行の頭の / で始まり、何行か続く
-- こともある。そのあとの字下げした行が入力で、それぞれのあとに答えが
-- 続く。空の行で、ひとまとまりが終わる。#pattern と #subject は、
-- そのあとのパターンと入力の修飾子を決める
readTests :: Text -> [TestCase]
readTests = outside "" "" . zip [1 ..] . T.lines
  where
    outside _ _ [] = []
    outside pat sub ((_, l) : rest)
      | Just m <- T.stripPrefix "#pattern" l = outside (setting m) sub rest
      | Just m <- T.stripPrefix "#subject" l = outside pat (setting m) rest
      | Just body <- T.stripPrefix "/" l     =
          let (src, mods, rest') = patternLines body rest
              allMods            = T.intercalate "," (filter (not . T.null)
                                     [mods, pat, sub])
              (block, after)     = break (blank . snd) rest'
          in  subjects src allMods block ++ outside pat sub after
      | otherwise = outside pat sub rest

    -- "#pattern mark" sets mark; "#pattern -mark" takes it away.
    -- "#pattern mark" で mark が付き、"#pattern -mark" で外れる
    setting m
      | "-" `T.isPrefixOf` T.strip m = ""
      | otherwise                    = T.strip m

    -- The subjects of one group: each indented line, with the lines
    -- after it until the next indented line as its answer.
    -- ひとまとまりの入力：字下げした行と、次の字下げした行までの答え
    subjects src mods ls = case dropWhile (not . isSubject . snd) ls of
      []              -> []
      (n, s) : rest ->
        let (answer, more) = break (isSubject . snd) rest
            answer'        = filter (not . isComment) (map snd answer)
        in  TestCase n src mods (T.strip s) answer'
              : subjects src mods more

    isSubject l = not (blank l)
                    && ("\t" `T.isPrefixOf` l || "  " `T.isPrefixOf` l)
    -- A line of only spaces ends the group, too (pcre2test does so).
    -- 空白だけの行も、ひとまとまりを終わらせる（pcre2test と同じ）
    blank = T.null . T.strip
    -- "\= Expect no match" is a note for people.
    -- "\= Expect no match" は、人のためのメモ
    isComment   = ("\\=" `T.isPrefixOf`)

-- 📂 Read the tests from a file. It is not UTF-8 but one byte for one
--    character (the tests are for PCRE2 without UTF), so read it as
--    Latin-1, where byte 0xe9 is the character U+00E9.
-- ファイルからテストを読む。UTF-8 ではなく、1バイトが1文字（UTF なしの
-- PCRE2 のテスト）なので、Latin-1 として読む（0xe9 のバイトは U+00E9 の文字）
readTestFile :: FilePath -> IO [TestCase]
readTestFile file = withFile file ReadMode $ \h -> do
  hSetEncoding h latin1
  readTests . T.pack <$> hGetContents' h

-- 🧶 The rest of a pattern after the first /: up to the / that has no
--    \ before it, maybe on a later line. Gives the pattern, the
--    modifiers, and the lines after it.
-- 最初の / のうしろ：\ の付いていない / まで（あとの行のこともある）。
-- パターン、修飾子、残りの行を返す
patternLines :: Text -> [(Int, Text)] -> (Text, Text, [(Int, Text)])
patternLines = go []
  where
    go acc l rest = case closing (T.unpack l) of
      Just (src, mods) ->
        (joinLines (T.pack src : acc), T.strip (T.pack mods), rest)
      Nothing -> case rest of
        (_, next) : more -> go (l : acc) next more
        []               -> (joinLines (l : acc), "", [])
    joinLines = T.intercalate "\n" . reverse

    closing :: String -> Maybe (String, String)
    closing ('\\' : c : cs) = (\(s, m) -> ('\\' : c : s, m)) <$> closing cs
    closing ('/' : cs)      = Just ([], cs)
    closing (c : cs)        = (\(s, m) -> (c : s, m)) <$> closing cs
    closing []              = Nothing
-- ANCHOR_END: read

-- ANCHOR: judge
-- ⚖️ Run one test with MiniRegex: the first match, or with /g all of
--    them, and print them the way pcre2test does. Every line must be
--    the same, the groups' lines too.
-- テストを1つ、MiniRegex で動かす。最初の一致か、/g なら全部の一致を
-- 探して、pcre2test と同じ形で書く。グループの行も、全部同じでなければ
-- ならない
judge :: TestCase -> Verdict
judge t = case (flagsOf (testModifiers t), decodeSubject (testSubject t)) of
  (Left m, _)             -> NotYet ("modifier " <> m)
  (_, Nothing)            -> NotYet "subject modifier"
  (Right mods, Just subj) ->
    case compileWith (modOptions mods) (testPattern t) of
      Left (Unsupported _ what) -> NotYet what
      Left (Invalid _ _)        -> Fail
      Right re
        | showFound mods subj (matches re subj) == testExpected t -> Pass
        | otherwise                                             -> Fail
      where
        matches re
          | modGlobal mods = findAll re
          | otherwise      = maybeToList . find re

-- ⏱️ Judge a test, but give up after this many microseconds: a
--    backtracking matcher can take for ever on some patterns. That
--    test is "not yet (slow)".
-- テストを判定する。ただし、このマイクロ秒で、あきらめる。バックトラックの
-- マッチャーは、パターンによっては終わらないことがある。そのテストは
-- 「まだ（slow）」
judgeWithin :: Int -> TestCase -> IO Verdict
judgeWithin micro t = do
  answer <- timeout micro (evaluate (judge t))
  pure (maybe (NotYet "slow") id answer)
-- ANCHOR_END: judge

-- ANCHOR: readable
-- 📖 Only the parser: can it read the pattern of this test? Pass when
--    it can, Fail when it calls a right pattern wrong, and "not yet"
--    with what it cannot read.
-- パーサーだけ：このテストのパターンを読めるか。読めたら Pass、
-- 正しいパターンを「まちがい」と言ったら Fail、読めなければ「まだ」と
-- 読めなかったもの
readable :: TestCase -> Verdict
readable t = case flagsOf (testModifiers t) of
  Left m     -> NotYet ("modifier " <> m)
  Right mods -> case parsePattern (modOptions mods) (testPattern t) of
    Right _                   -> Pass
    Left (Invalid _ _)        -> Fail
    Left (Unsupported _ what) -> NotYet what
-- ANCHOR_END: readable

-- ANCHOR: flags
-- 🚩 The modifiers MiniRegex knows: the flags i, m, s and x, g (all
--    the matches) and aftertext (print what is after each match, too).
-- MiniRegex が知っている修飾子：フラグの i・m・s・x、g（全部の一致）、
-- aftertext（一致のうしろの文字も書く）
data Modifiers = Modifiers
  { modOptions :: [Option]
  , modGlobal  :: Bool
  , modAfter   :: Bool
  }
  deriving (Show, Eq)

-- 🚩 Read the modifiers. Any other one is given back, as "not yet".
-- 修飾子を読む。ほかのものは「まだ」として返す
flagsOf :: Text -> Either Text Modifiers
flagsOf mods = foldr add (Right (Modifiers [] False False)) parts
  where
    parts = filter (not . T.null) (T.splitOn "," mods)
    add m acc = do
      ms <- acc
      case m of
        "g"         -> Right ms { modGlobal = True }
        "aftertext" -> Right ms { modAfter = True }
        _ | T.all (`elem` letters) m ->
              Right ms { modOptions = map flag (T.unpack m) ++ modOptions ms }
          | otherwise -> Left m
    letters = "imsx" :: String
    flag 'i' = Caseless
    flag 'm' = Multiline
    flag 's' = DotAll
    flag _   = Extended

-- 🖨️ What pcre2test would print for these matches. For each one, the
--    line " 0: …"; with aftertext, " 0+ …" (what comes after it); then
--    the groups, up to the last one that found something ("<unset>"
--    for a group that found nothing). No match at all: "No match".
--    Characters that are not plain ASCII are written as \xhh.
-- この一致なら、pcre2test は何と出すか。一致ごとに " 0: …" の行、
-- aftertext なら " 0+ …"（そのうしろの文字）、そのあとに、何かを
-- 見つけた最後のグループまでの行（見つけなかったグループは "<unset>"）。
-- 1つもなければ "No match"。ふつうの ASCII でない文字は \xhh と書く
showFound :: Modifiers -> Text -> [Match] -> [Text]
showFound _ _ [] = ["No match"]
showFound mods subj ms = concatMap one ms
  where
    one m = [line 0 (matchText m)]
         ++ [" 0+ " <> visible (T.drop (matchEnd m) subj) | modAfter mods]
         ++ zipWith groupLine [1 ..] (dropUnsetTail (matchGroups m))
    groupLine n Nothing  = label n <> "<unset>"
    groupLine n (Just s) = line n (spanText s)
    line n s = label n <> visible s
    label :: Int -> Text
    label n = T.justifyRight 2 ' ' (T.pack (show n)) <> ": "
    dropUnsetTail = reverse . dropWhile (== Nothing) . reverse
    visible = T.concatMap char
    char c
      | c >= ' ' && c <= '~' = T.singleton c
      | ord c < 0x100        = "\\x" <> hex2 (ord c)
      | otherwise            = "\\x{" <> T.pack (showHex (ord c) "") <> "}"
    hex2 n = T.justifyRight 2 '0' (T.pack (showHex n ""))
-- ANCHOR_END: flags

-- ANCHOR: decode
-- 🔓 Turn the \ escapes of a subject line into characters, the way
--    pcre2test does: \t \n \x41 \x{263a} \101 ... Nothing when the line
--    has \= (modifiers for this subject), which MiniRegex cannot follow
--    yet.
-- 入力の行の \ を、pcre2test と同じように文字にする：\t \n \x41
-- \x{263a} \101 など。\=（この入力だけの修飾子）があれば Nothing
-- （MiniRegex には、まだ従えない）
decodeSubject :: Text -> Maybe Text
decodeSubject = fmap T.pack . go . T.unpack
  where
    go :: String -> Maybe String
    go [] = Just []
    go ('\\' : '=' : _) = Nothing
    go ['\\'] = Nothing
    go ('\\' : 'x' : '{' : cs) = case span isHexDigit cs of
      (h@(_ : _), '}' : rest) -> (number 16 h :) <$> go rest
      _                       -> Nothing
    go ('\\' : 'x' : cs) =
      let (h, rest) = spanUpTo 2 isHexDigit cs
      in  (number 16 h :) <$> go rest
    go ('\\' : c : cs)
      | isOctDigit c =
          let (o, rest) = spanUpTo 2 isOctDigit cs
          in  (number 8 (c : o) :) <$> go rest
      | Just e <- lookup c escapes = (e :) <$> go cs
      | otherwise                  = (c :) <$> go cs
    go (c : cs) = (c :) <$> go cs

    escapes = [ ('a', '\a'), ('b', '\b'), ('e', '\ESC'), ('f', '\f')
              , ('n', '\n'), ('r', '\r'), ('t', '\t'), ('v', '\v') ]

-- ✂️ The first characters (at most n) that pass the test, and the rest.
-- 条件に合う頭の文字（n 文字まで）と、残り
spanUpTo :: Int -> (Char -> Bool) -> String -> (String, String)
spanUpTo n p cs =
  let (h, _) = span p (take n cs)
  in  (h, drop (length h) cs)

-- 🔢 The character with this number, written in base 8 or 16
--    (no digits at all: the character 0).
-- 8進か16進で書いた番号の文字（数字がなければ、0番の文字）
number :: Int -> String -> Char
number base = chr . foldl' (\acc d -> acc * base + digitToInt d) 0
-- ANCHOR_END: decode

-- ANCHOR: tally
-- 🧮 How many passed, failed, and could not be run yet.
-- 通った数、ちがった数、まだ動かせなかった数
data Tally = Tally
  { passed :: Int
  , failed :: Int
  , notYet :: Int
  }
  deriving (Show, Eq)

tally :: [Verdict] -> Tally
tally = foldl' add (Tally 0 0 0)
  where
    add t Pass       = t { passed = passed t + 1 }
    add t Fail       = t { failed = failed t + 1 }
    add t (NotYet _) = t { notYet = notYet t + 1 }
-- ANCHOR_END: tally
-- ANCHOR_END: all
