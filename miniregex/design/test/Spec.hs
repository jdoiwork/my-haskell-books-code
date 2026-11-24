module Main (main) where

-- ANCHOR: imports
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (once)
import Text.Regex.Pcre2 qualified as Pcre2

import MiniRegex
import MiniRegex.Compat
import MiniRegex.Examples
import MiniRegex.Syntax
import RegexAnswers
-- ANCHOR_END: imports

-- ANCHOR: gens
-- 🎲 Short texts of a, b and 日 (a character of three bytes in UTF-8,
--    so that counting characters and counting bytes would differ).
-- a・b・日 でできた短い文字列（日は UTF-8 で3バイトなので、文字で
-- 数えるのとバイトで数えるのとで、答えが変わる）
genText :: Gen Text
genText = T.pack <$> listOf (elements "ab日")

-- 🎲 Short texts of a and b only.
-- a と b だけの短い文字列
genAscii :: Gen Text
genAscii = T.pack <$> listOf (elements "ab")

-- 🎲 Patterns of a and b, now and then with a sign that has a meaning.
-- a と b のパターン。ときどき、意味を持つ記号がまざる
genPattern :: Gen Text
genPattern = T.pack <$> resize 4 (listOf piece)
  where
    piece = frequency
      [ (8, elements "ab")
      , (1, elements ".*+?|()[]{}^$")
      ]

-- 🔦 The places of the pieces of a plain text (not empty) in a text,
--    found the simple way: try every place from the left, and jump
--    over each piece found.
-- ふつうの文字列（空でない）が出てくる場所を、素朴に求める：左から
-- 1つずつ試し、見つけたら、その分だけ飛ばす
placesOf :: Text -> Text -> [Int]
placesOf needle = go 0
  where
    go at t
      | T.null t                 = []
      | needle `T.isPrefixOf` t  = at : go (at + n) (T.drop n t)
      | otherwise                = go (at + 1) (T.drop 1 t)
    n = T.length needle
-- ANCHOR_END: gens

-- ANCHOR: main
main :: IO ()
main = hspec $ do
-- ANCHOR_END: main
  -- ANCHOR: syntax
  describe "the tree / パターンの木" $ do
    -- {2,5} is at least 2, then up to 3 more
    -- {2,5} は「少なくとも2回、そこから、あと3回まで」
    it "{2,5} is 2 and up to 3 more" $
      between 2 5 `shouldBe` Just (Quantifier (Count 2 (Just 3)) Greedy)
    -- {5,2} cannot be made
    -- {5,2} は作れない
    it "{5,2} cannot be made" $
      between 5 2 `shouldBe` Nothing
    -- a plain text is one branch of single characters
    -- ふつうの文字列は、1文字ずつの部品を並べた1本の枝
    it "a plain text is one branch" $
      literal "ab" `shouldBe` Pattern (Branch
        [Quantified (Lit 'a') once, Quantified (Lit 'b') once] :| [])
  -- ANCHOR_END: syntax

  -- ANCHOR: compile
  describe "compile / パターンを作る" $ do
    -- a plain pattern can be compiled
    -- ふつうの文字のパターンは作れる
    it "takes plain characters" $
      fmap source (compile "dragon") `shouldBe` Right "dragon"
    -- \ before a sign is the sign itself
    -- 記号の前の \ は、その記号そのもの
    it "takes \\ before a sign" $
      fmap matchText (find (surely "1\\+1") "1+1=2") `shouldBe` Just "1+1"
    -- a sign with a meaning is "not yet", with its place
    -- 意味を持つ記号は「まだ」。場所も言う
    it "says where it stopped" $
      fmap source (compile "ab*c") `shouldBe` Left (Unsupported 2 "*")
    -- the place after a \ counts both characters
    -- \ のあとの場所は、2文字ぶん進めて数える
    it "says where it stopped, after a \\" $
      fmap source (compile "\\.*") `shouldBe` Left (Unsupported 2 "*")
    -- a \ at the end is wrong, at the same place as PCRE2
    -- 最後の \ はまちがい。場所は PCRE2 と同じ
    it "refuses a \\ at the end, like PCRE2" $
      fmap source (compile "ab\\")
        `shouldBe` Left (Invalid 3 "\\ at end of pattern")
    -- flags are not yet
    -- フラグは、まだ
    it "refuses flags for now" $
      compileWith [Caseless] "a" `shouldSatisfy` isLeft
  -- ANCHOR_END: compile

  -- ANCHOR: face
  describe "the face / 使いやすい顔" $ do
    -- the examples of the chapter
    -- 章の例
    it "counts, renames and splits" $ do
      countDragons records `shouldBe` Right 2
      fmap (T.isInfixOf "wyvern scale") (renameDragons records)
        `shouldBe` Right True
      splitLoot "herb, sword, shield"
        `shouldBe` Right ["herb", "sword", "shield"]
    -- the date pattern needs the next chapters
    -- 日付のパターンは、次の章から
    it "cannot find dates yet" $
      findDates records `shouldSatisfy` isLeft
    -- by hand, a room number looks like a date, too
    -- 手で探すと、部屋の番号も日付に見えてしまう
    it "finds dates by hand (and a room number)" $
      datesByHand records `shouldBe`
        ["2026-11-19", "2026-11-20", "2026-11-21", "1234-56-78"]
    -- an empty pattern is found between all the characters
    -- 空のパターンは、すべての文字のあいだに見つかる
    it "finds an empty pattern everywhere" $ do
      map matchStart (findAll (surely "") "日本") `shouldBe` [0, 1, 2]
      split (surely "") "abc" `shouldBe` ["", "a", "b", "c", ""]
    -- find gives the first one, not any one
    -- find が返すのは、どれかではなく、最初の1つ
    it "finds the first one" $
      find (surely "a") "banana" `shouldBe` Just (Match "a" 1 2)
    -- isMatch says yes and no
    -- isMatch は「ある」も「ない」も言う
    it "tells if it is there" $ do
      isMatch (surely "nan") "banana" `shouldBe` True
      isMatch (surely "nab") "banana" `shouldBe` False
    -- places are counted in characters
    -- 場所は文字で数える
    it "counts places in characters" $
      find (surely "b") "日本b" `shouldBe` Just (Match "b" 2 3)
    -- findAll finds the same places as the simple way
    -- findAll は、素朴なやり方と同じ場所を見つける
    prop "findAll agrees with the simple way" $
      forAll genText $ \t -> forAll (genText `suchThat` (not . T.null)) $
        \needle ->
          map matchStart (findAll (surely needle) t)
            === placesOf needle t
    -- split and replaceAll agree with Data.Text
    -- split と replaceAll は、Data.Text と同じ答え
    prop "split and replaceAll agree with Data.Text" $
      forAll genText $ \t -> forAll (genText `suchThat` (not . T.null)) $
        \needle ->
          split (surely needle) t === T.splitOn needle t
            .&&. replaceAll (surely needle) "<>" t
                   === T.replace needle "<>" t
    -- replace changes only the first one
    -- replace は最初の1つだけ
    it "replace changes only the first one" $
      replace (surely "a") "A" "banana" `shouldBe` "bAnana"
    -- replaceWith gets each match
    -- replaceWith には、見つけたものが1つずつ渡る
    it "replaceWith gets each match" $
      replaceWith (surely "an") (T.toUpper . matchText) "banana"
        `shouldBe` "bANANa"
  -- ANCHOR_END: face

  -- ANCHOR: meter
  describe "the compatibility meter / 互換度メーター" $ do
    -- a pattern, its subjects and their answers are read
    -- パターン、入力、答えを読む
    it "reads a group of pcre2test's output" $
      readTests (T.unlines
        [ "/the quick/i"
        , "    the quick fox"
        , " 0: the quick"
        , "\\= Expect no match"
        , "    THE SLOW FOX"
        , "No match"
        , ""
        , "/a"
        , "b/"
        , "\ta\\nb"
        , " 0: a\\x0ab"
        ])
        `shouldBe`
          [ TestCase 2 "the quick" "i" "the quick fox" [" 0: the quick"]
          , TestCase 5 "the quick" "i" "THE SLOW FOX" ["No match"]
          , TestCase 10 "a\nb" "" "a\\nb" [" 0: a\\x0ab"]
          ]
    -- \ escapes of a subject are read like pcre2test
    -- 入力の \ は、pcre2test と同じように読む
    it "decodes a subject" $ do
      decodeSubject "a\\tb\\x41\\x{263a}\\101\\0" `shouldBe`
        Just "a\tbA\x263a\&A\0"
      decodeSubject "abc\\=notbol" `shouldBe` Nothing
      -- what MiniRegex cannot read yet is not read as something else
      -- まだ読めない書き方を、別の文字列と読まない
      decodeSubject "\\[a]{2}" `shouldBe` Nothing
      decodeSubject "\\d" `shouldBe` Nothing
      decodeSubject "a\\" `shouldBe` Just "a"
      decodeSubject "\\" `shouldBe` Just ""
      decodeSubject "\\o{101}\\$" `shouldBe` Just "A$"
    -- answers are written like pcre2test
    -- 答えは、pcre2test と同じ形で書く
    it "shows an answer like pcre2test" $
      showFound (Just (Match "a\tb\xe9" 0 4)) `shouldBe` [" 0: a\\x09b\\xe9"]
    -- a line of only spaces ends a group (it is not a subject)
    -- 空白だけの行で、ひとまとまりが終わる（入力ではない）
    it "ends a group at a line of spaces" $
      readTests (T.unlines
        [ "/X{}/"
        , "    ZZX{}YZ"
        , " 0: X{}"
        , "    "
        , "/X{1234/"
        , "    ZZX{123456"
        , " 0: X{1234"
        ])
        `shouldBe`
          [ TestCase 2 "X{}" "" "ZZX{}YZ" [" 0: X{}"]
          , TestCase 6 "X{1234" "" "ZZX{123456" [" 0: X{1234"]
          ]
    -- the whole of testoutput1: no wrong answers
    -- testoutput1 の全部：ちがう答えは1つもない
    it "passes 19 of PCRE2's testoutput1, and fails none" $ do
      tests <- readTestFile "testdata/testoutput1"
      tally (map judge tests) `shouldBe` Tally 19 0 2629
  -- ANCHOR_END: meter

  -- ANCHOR: pcre2
  describe "the real PCRE2 / 本物の PCRE2" $ do
    -- when MiniRegex can run a pattern, it finds the first match that
    -- PCRE2 finds, at the same place (replacing it shows where it is)
    -- MiniRegex が動かせるパターンなら、PCRE2 と同じ最初の一致を、同じ
    -- 場所に見つける（置き換えてみると、場所もわかる）
    prop "finds the first match PCRE2 finds" $
      forAll genPattern $ \p -> forAll genText $ \t ->
        case compile p of
          Left _   -> label "not yet" True
          Right re -> label "compared" $
            fmap matchText (find re t) === Pcre2.match p t
              .&&. replace re "<>" t === Pcre2.sub p "<>" t
    -- and all the matches; only a and b here, as the pcre2 library
    -- steps into the middle of 日 after an empty match
    -- 全部の一致も。ここでは a と b だけ（pcre2 のライブラリは、空の
    -- 一致のあと、日の途中のバイトへ進んでしまう）
    prop "finds all the matches PCRE2 finds" $
      forAll genPattern $ \p -> forAll genAscii $ \t ->
        case compile p of
          Left _   -> label "not yet" True
          Right re -> label "compared" $
            map matchText (findAll re t) === Pcre2.match p t
    -- often enough, it compares (not only "not yet")
    -- 「まだ」ばかりでなく、ちゃんとくらべている
    prop "compares often enough" $
      checkCoverage $ forAll genPattern $ \p ->
        cover 40 (either (const False) (const True) (compile p))
          "compared" True
    -- an escaped text is found as it is, by both
    -- escape した文字列は、どちらでも、そのまま見つかる
    prop "escape finds the text as it is (answer of step 3)" $
      \s -> let t = T.pack s
            in  fmap matchText (find (surely (escape t)) t) === Just t
                  .&&. take 1 (Pcre2.match (escape t) t) === [t]
  -- ANCHOR_END: pcre2

  -- ANCHOR: answers
  describe "the training / トレーニング" $ do
    -- colou?r has an optional u
    -- colou?r の u は、あってもなくてもよい
    it "colou?r" $ do
      let Pattern (Branch ps :| []) = colourTree
      length ps `shouldBe` 6
      ps !! 4 `shouldBe` Quantified (Lit 'u') optional
    -- (?:ab)+|c has two branches
    -- (?:ab)+|c は枝が2本
    it "(?:ab)+|c" $ do
      let Pattern branches = choiceTree
      length branches `shouldBe` 2
  -- ANCHOR_END: answers
