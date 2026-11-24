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
  , Encoding (..)
  , judge
  , judgeWithin
  , readable
  , tally
  , flagsOf
  , decodeSubject
  , showFound
  ) where

-- ANCHOR: imports
import Data.Char (chr, digitToInt, isAlphaNum, isHexDigit, isOctDigit, ord)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Control.Exception (evaluate)
import Numeric (showHex)
import System.IO
  (IOMode (ReadMode), hGetContents', hSetEncoding, latin1, utf8, withFile)
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

-- 📂 Read the tests from a file. The tests for PCRE2 without UTF
--    (testoutput1) are one byte for one character: read them as Latin-1,
--    where byte 0xe9 is the character U+00E9. The tests with UTF
--    (testoutput4) are UTF-8.
-- ファイルからテストを読む。UTF なしの PCRE2 のテスト（testoutput1）は、
-- 1バイトが1文字なので、Latin-1 として読む（0xe9 のバイトは U+00E9 の
-- 文字）。UTF のあるテスト（testoutput4）は UTF-8
data Encoding = Latin1 | Utf8
  deriving (Show, Eq)

readTestFile :: Encoding -> FilePath -> IO [TestCase]
readTestFile enc file = withFile file ReadMode $ \h -> do
  hSetEncoding h (if enc == Latin1 then latin1 else utf8)
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
    case compileWith (modOptions mods) (patternOf mods t) of
      Left (Unsupported _ what) -> NotYet what
      Left (Invalid _ _)        -> Fail
      Right re
        | oneByteCase mods (testPattern t) subj ->
            NotYet "caseless without UTF"
        | showFound mods subj (matches re subj) == testExpected t -> Pass
        | otherwise                                             -> Fail
      where
        matches re
          | modGlobal mods = findAll re
          | otherwise      = maybeToList . find re

-- ✍️ The pattern of a test, with (?xx) before it for the modifier xx.
-- テストのパターン。修飾子 xx があれば、頭に (?xx) を付ける
patternOf :: Modifiers -> TestCase -> Text
patternOf mods t
  | modMore mods = "(?xx)" <> testPattern t
  | otherwise    = testPattern t

-- 🔡 Without UTF, PCRE2 tells é from É even under (?i): only ASCII
--    letters have another case. MiniRegex is always Unicode, so a
--    caseless test with a character from 128 on is "not yet".
-- UTF なしの PCRE2 は、(?i) のもとでも é と É を区別する（ほかの形が
-- あるのは ASCII の文字だけ）。MiniRegex はいつも Unicode なので、
-- 128 から先の文字がある、大文字と小文字を区別しないテストは「まだ」
oneByteCase :: Modifiers -> Text -> Text -> Bool
oneByteCase mods pat subj =
  not (modUtf mods)
    && (Caseless `elem` modOptions mods || inlineCaseless)
    && T.any (>= '\x80') (pat <> subj)
  where
    -- (?i), (?mi-s:...) and the like: an i among the letters of a (?...
    -- (?i)・(?mi-s:...) など：(? のうしろのフラグの文字に i がある
    inlineCaseless = any (T.elem 'i' . T.takeWhile flagLetter . T.drop 2)
                         (map snd (T.breakOnAll "(?" pat))
    flagLetter c = c `elem` ("imnsxUaDSWPTJr^-" :: String)

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
  Right mods -> case parsePattern (modOptions mods) (patternOf mods t) of
    Right _                   -> Pass
    Left (Invalid _ _)        -> Fail
    Left (Unsupported _ what) -> NotYet what
-- ANCHOR_END: readable

-- ANCHOR: flags
-- 🚩 The modifiers MiniRegex knows: the flags i, m, s and x, g (all
--    the matches), aftertext (print what is after each match, too),
--    utf (the subject is Unicode; print it as \x{...}) and ucp (\d,
--    \w ... are Unicode). Without ucp, PCRE2's \d and \w are ASCII:
--    the test is run with the flag Ascii.
-- MiniRegex が知っている修飾子：フラグの i・m・s・x、g（全部の一致）、
-- aftertext（一致のうしろの文字も書く）、utf（入力が Unicode。\x{...} で
-- 書く）、ucp（\d・\w などが Unicode）。ucp がなければ、PCRE2 の \d・\w は
-- ASCII なので、テストはフラグ Ascii を付けて動かす
data Modifiers = Modifiers
  { modOptions :: [Option]
  , modGlobal  :: Bool
  , modAfter   :: Bool
  , modUtf     :: Bool
  , modMore    :: Bool
  }
  deriving (Show, Eq)

-- 🚩 Read the modifiers. Any other one is given back, as "not yet".
-- 修飾子を読む。ほかのものは「まだ」として返す
flagsOf :: Text -> Either Text Modifiers
flagsOf mods =
  withUcp <$> foldr add (Right (Modifiers [] False False False False)) parts
  where
    parts = filter (not . T.null) (T.splitOn "," mods)
    withUcp ms
      | "ucp" `elem` parts = ms
      | otherwise          = ms { modOptions = modOptions ms ++ [Ascii] }
    add m acc = do
      ms <- acc
      case m of
        "g"         -> Right ms { modGlobal = True }
        "aftertext" -> Right ms { modAfter = True }
        "utf"       -> Right ms { modUtf = True }
        "ucp"       -> Right ms
        -- xx is not one of our Options: the test writes (?xx) instead
        -- xx は Option にないので、テストはかわりに (?xx) を書く
        "xx"        -> Right ms { modMore = True }
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
--    Characters that are not plain ASCII are written as \xhh (with
--    utf: \x{h}, as pcre2test does).
-- この一致なら、pcre2test は何と出すか。一致ごとに " 0: …" の行、
-- aftertext なら " 0+ …"（そのうしろの文字）、そのあとに、何かを
-- 見つけた最後のグループまでの行（見つけなかったグループは "<unset>"）。
-- 1つもなければ "No match"。ふつうの ASCII でない文字は \xhh と書く
-- （utf なら、pcre2test と同じく \x{h}）
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
      | modUtf mods          = "\\x{" <> hex2 (ord c) <> "}"
      | ord c < 0x100        = "\\x" <> hex2 (ord c)
      | otherwise            = "\\x{" <> T.pack (showHex (ord c) "") <> "}"
    hex2 n = T.justifyRight 2 '0' (T.pack (showHex n ""))
-- ANCHOR_END: flags

-- ANCHOR: decode
-- 🔓 Turn the \ escapes of a subject line into characters, the way
--    pcre2test does: \t \n \x41 \x{263a} \o{11064} \N{U+1234} \101,
--    and \$ is $. A \ at the end of the line is left out (a line of just
--    \ is the empty subject). Nothing for what MiniRegex cannot read
--    yet: \= (modifiers for this subject), \[ab]{3} (a piece written
--    again and again), and a \ before any other letter or digit. A line
--    read wrongly would be counted as a wrong answer of the engine.
-- 入力の行の \ を、pcre2test と同じように文字にする：\t \n \x41
-- \x{263a} \o{11064} \N{U+1234} \101、\$ は $。行の終わりの \ は捨てる
-- （\ だけの行は、空の入力）。MiniRegex がまだ読めないものは Nothing：
-- \=（この入力だけの修飾子）、\[ab]{3}（くり返し）、ほかの英字や数字の
-- 前の \。読みまちがえると、エンジンのまちがいに数えてしまう
decodeSubject :: Text -> Maybe Text
decodeSubject = fmap T.pack . go . T.unpack
  where
    go :: String -> Maybe String
    go [] = Just []
    go ['\\'] = Just []
    go ('\\' : '=' : _) = Nothing
    go ('\\' : '[' : _) = Nothing
    go ('\\' : 'x' : '{' : cs) = braced 16 isHexDigit cs
    go ('\\' : 'o' : '{' : cs) = braced 8 isOctDigit cs
    go ('\\' : 'N' : '{' : 'U' : '+' : cs) = braced 16 isHexDigit cs
    go ('\\' : 'x' : cs) =
      let (h, rest) = spanUpTo 2 isHexDigit cs
      in  (number 16 h :) <$> go rest
    go ('\\' : c : cs)
      | isOctDigit c =
          let (o, rest) = spanUpTo 2 isOctDigit cs
          in  (number 8 (c : o) :) <$> go rest
      | Just e <- lookup c escapes = (e :) <$> go cs
      | isAlphaNum c               = Nothing
      | otherwise                  = (c :) <$> go cs
    go (c : cs) = (c :) <$> go cs

    -- the digits up to }, as one character
    -- } までの数字を、1文字に
    braced base isDigitOf cs = case span isDigitOf cs of
      (h@(_ : _), '}' : rest) -> (number base h :) <$> go rest
      _                       -> Nothing

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
