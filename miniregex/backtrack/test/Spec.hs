module Main (main) where

-- ANCHOR: imports
import Control.Exception (SomeException, evaluate, try)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (once)
import Text.Regex.Pcre2 qualified as Pcre2

import MiniRegex
import MiniRegex.Candidates (ends)
import MiniRegex.Compat
import MiniRegex.Cursor (Cursor (..))
import MiniRegex.Engine (searchFromList)
import MiniRegex.Examples
import MiniRegex.Parser
import MiniRegex.Pretty
import MiniRegex.Syntax
import MiniRegex.Types (options, tree)
import RegexAnswers
-- ANCHOR_END: imports

-- ANCHOR: gens
-- 🎲 Short texts of a, b and 日 (a character of three bytes in UTF-8,
--    so that counting characters and counting bytes would differ).
-- a・b・日 でできた短い文字列（日は UTF-8 で3バイトなので、文字で
-- 数えるのとバイトで数えるのとで、答えが変わる）
genText :: Gen Text
genText = T.pack <$> listOf (elements "ab日")

-- 🎲 Short texts of a, b and newlines (for ^, $ and .).
-- a・b と改行の短い文字列（^・$・. のため）
genLines :: Gen Text
genLines = T.pack <$> listOf (frequency [(3, elements "ab"), (1, pure '\n')])

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
      , (2, elements ".*+?|()[]{}^$")
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

-- 🚩 Our flags m and s, as the pcre2 library's options.
-- MiniRegex のフラグ m と s を、pcre2 のライブラリのオプションに
pcre2Options :: [Option] -> Pcre2.Option
pcre2Options = foldMap one
  where
    one Multiline = Pcre2.Multiline
    one DotAll    = Pcre2.DotAll
    one _         = mempty

-- 🧪 surely, with flags.
-- フラグつきの surely
surelyWith :: [Option] -> Text -> Regex
surelyWith opts src = either (error . show) id (compileWith opts src)

-- 🙈 Does the tree have a group repeated 0 times? The real PCRE2 may
--    answer those differently; the next chapter finds out why.
-- 0回くり返すグループがある木か。本物の PCRE2 と答えがちがうことが
-- ある。わけは次の章で
hasZeroGroup :: Pattern -> Bool
hasZeroGroup (Pattern bs) = any branchHas bs
  where
    branchHas (Branch ps) = any pieceHas ps
    pieceHas (Quantified (Group _ p) q) =
      count q == Count 0 (Just 0) || hasZeroGroup p
    pieceHas _ = False
-- ANCHOR_END: gens

-- ANCHOR: wild
-- 🎲 Wilder patterns, to look for wrong ones: signs, digits, \ and
--    some letters that mean something after a \ or a (?.
-- 荒っぽいパターン。まちがったパターンを探すため：記号、数字、\ と、
-- \ や (? のうしろで意味を持つ文字
genWild :: Gen Text
genWild = T.pack <$> resize 8 (listOf piece)
  where
    piece = frequency
      [ (6, elements "ab")
      , (4, elements "()[]{}|*+?^$.")
      , (2, elements "\\")
      , (1, elements "0129,-:<>=!#P'")
      , (1, elements "dQExcnyi")
      ]

-- 🚧 What PCRE2 says about a pattern: Nothing when it compiles, or the
--    message of its error.
-- PCRE2 がパターンについて言うこと：コンパイルできれば Nothing、
-- できなければ、そのエラーの文
pcre2Error :: Text -> IO (Maybe Text)
pcre2Error p = do
  result <- try (evaluate (Pcre2.matches p ""))
  pure $ case result of
    Right _ -> Nothing
    Left (e :: SomeException) ->
      let firstLine = T.takeWhile (/= '\n') (T.pack (show e))
      in  Just (T.strip (T.drop 1 (T.dropWhile (/= ':') firstLine)))
-- ANCHOR_END: wild

-- ANCHOR: trees
-- 🌳 Random trees, to write back and read again. Names are left out
--    (two of the same name would be wrong); a test below has them.
-- でたらめな木。書き戻して、読み直すため。名前のグループは入れない
-- （同じ名前が2つあると、まちがいになる）。下のテストで別に確かめる
genTree :: [Greed] -> Int -> Gen Pattern
genTree greeds depth = do
  n <- chooseInt (1, 3)
  bs <- vectorOf n genBranch
  pure (Pattern (NE.fromList bs))
  where
    genBranch = Branch <$> resize 4 (listOf genPiece)
    genPiece = frequency
      [ (1, Anchor <$> elements [LineStart, LineEnd])
      , (6, Quantified <$> genAtom <*> genQuantifier)
      ]
    genAtom = frequency $
      [ (6, Lit <$> genChar)
      , (1, pure AnyChar)
      , (2, Class <$> genClass)
      ] ++
      [ (2, Group <$> elements [Numbered, NonCapturing]
                  <*> genTree greeds (depth - 1))
      | depth > 0 ]
    genClass = do
      neg <- arbitrary
      first <- genItem
      rest <- resize 3 (listOf genItem)
      pure (CharClass neg (first :| rest))
    genItem = oneof
      [ Single <$> genChar
      , do a <- genChar
           b <- genChar
           pure (Range (min a b) (max a b))
      ]
    genQuantifier = do
      c <- frequency
        [ (4, pure (Count 1 (Just 0)))
        , (1, Count <$> small <*> oneof [pure Nothing, Just <$> small])
        ]
      Quantifier c <$> elements greeds
    small = fromIntegral <$> chooseInt (0, 3)

-- 🎲 The characters of the trees: plain ones, and every sign.
-- 木の文字：ふつうの文字と、記号の全部
genChar :: Gen Char
genChar = elements "ab日 .*+?|()[]{}^$\\-,]0#:"
-- ANCHOR_END: trees

main :: IO ()
main = hspec $ do
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
    -- the matcher runs *; only a*+ and the flag i wait
    -- マッチャーは * を動かせる。待つのは a*+ とフラグ i だけ
    it "stops only a*+ and the flag i" $ do
      fmap source (compile "ab*c") `shouldBe` Right "ab*c"
      fmap source (compile "a(?:b*+)c")
        `shouldBe` Left (Unsupported 0 "matcher: possessive")
      fmap source (compileWith [Caseless] "a")
        `shouldBe` Left (Unsupported 0 "matcher: flag i")
      fmap source (compileWith [Multiline, DotAll] "^a.")
        `shouldBe` Right "^a."
    -- a \ at the end is wrong, at the same place as PCRE2
    -- 最後の \ はまちがい。場所は PCRE2 と同じ
    it "refuses a \\ at the end, like PCRE2" $
      fmap source (compile "ab\\")
        `shouldBe` Left (Invalid 3 "\\ at end of pattern")
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
    -- the date pattern works now (and finds the room number, too)
    -- 日付のパターンが動く（部屋の番号も見つけてしまう）
    it "finds dates (and a room number)" $
      findDates records `shouldBe`
        Right ["2026-11-19", "2026-11-20", "2026-11-21", "1234-56-78"]
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
    -- the whole of testoutput1: no wrong answers, and the parser
    -- calls no right pattern wrong
    -- testoutput1 の全部：ちがう答えは1つもなく、パーサーは正しい
    -- パターンを「まちがい」と言わない
    it "reads 804 patterns and passes 428 tests, and fails none" $ do
      tests <- readTestFile "testdata/testoutput1"
      tally (map readable tests) `shouldBe` Tally 804 0 1844
      verdicts <- mapM (judgeWithin 1000000) tests
      tally verdicts `shouldBe` Tally 428 0 2220
    -- with groups printed, the line " 0:" decides only Fail
    -- グループの行があれば、" 0:" の行で決まるのは Fail だけ
    it "waits for the groups when PCRE2 printed them" $ do
      judge (TestCase 1 "(a)b" "" "ab" [" 0: ab", " 1: a"])
        `shouldBe` NotYet "captures"
      judge (TestCase 1 "(a)c" "" "ac" [" 0: ab", " 1: a"])
        `shouldBe` Fail
      judge (TestCase 1 "(a)|b" "" "b" [" 0: b"]) `shouldBe` Pass
  -- ANCHOR_END: meter

  -- ANCHOR: matcher
  describe "the matcher / マッチャー" $ do
    -- each answer was checked with pcre2test 10.49
    -- 答えは、どれも pcre2test 10.49 で確かめた
    let finds opts p t answer = it (show (opts, p, t)) $
          fmap matchText (find (surelyWith opts p) t) `shouldBe` answer
    -- the left branch first
    -- 左の枝が先
    finds [] "a|ab"      "abc"   (Just "a")
    finds [] "ab|a"      "abc"   (Just "ab")
    -- greedy and lazy
    -- 欲ばりと控えめ
    finds [] "a*"        "aaab"  (Just "aaa")
    finds [] "a*"        "baaa"  (Just "")
    finds [] "a*?"       "aaab"  (Just "")
    finds [] "a*?b"      "aaab"  (Just "aaab")
    finds [] "a{2,3}"    "aaaa"  (Just "aaa")
    finds [] "a{2,3}?"   "aaaa"  (Just "aa")
    -- repeats that can take nothing
    -- 何も食べずに回れるくり返し
    finds [] "(a*)*"     "b"     (Just "")
    finds [] "(a*)*"     "aab"   (Just "aa")
    finds [] "(?:a*)*b"  "aaac"  Nothing
    finds [] "(?:|a)*"   "aaa"   (Just "")
    finds [] "(?:|a)*x"  "aax"   (Just "aax")
    -- anchors, and the flags m and s
    -- アンカーと、フラグ m・s
    finds [] "^abc$"     "abc\n" (Just "abc")
    finds [] "a$"        "a\n\n" Nothing
    finds [] "^b"        "a\nb"  Nothing
    finds [Multiline] "^b" "a\nb" (Just "b")
    finds [] "a.c"       "a\nc"  Nothing
    finds [DotAll] "a.c" "a\nc"  (Just "a\nc")
    it "finds ^ and $ at each line with m" $ do
      map matchStart (findAll (surelyWith [Multiline] "^") "a\nb\n")
        `shouldBe` [0, 2]
      map matchStart (findAll (surelyWith [Multiline] "$") "a\nb\n")
        `shouldBe` [1, 3, 4]
    -- (a+)+ lists 2^n - 1 places on n a's
    -- (a+)+ は、a が n 個なら 2^n - 1 個の場所を並べる
    it "lists 2^n - 1 ends for (a+)+" $
      [endsAtStart "(a+)+" (T.replicate n "a" <> "c") | n <- [1 .. 10]]
        `shouldBe` [2 ^ n - 1 | n <- [1 .. 10 :: Int]]
    -- the list of candidates and the continuations agree
    -- 候補のリストと継続は、同じ答え
    prop "the two matchers agree" $
      -- (short texts: a backtracking matcher can take very long on
      --  a long text that does not match)
      -- （短い文字列で。バックトラックのマッチャーは、合わない長い
      --   文字列に、とても長くかかることがある）
      forAll (genTree [Greedy, Lazy] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (sublistOf [Multiline, DotAll]) $ \opts ->
          case compileWith opts (render tr) of
            Left e   -> counterexample (show e) False
            Right re -> searchFromList re t 0 === find re t
  -- ANCHOR_END: matcher

  -- ANCHOR: pcre2
  describe "the real PCRE2 / 本物の PCRE2" $ do
    -- when MiniRegex can run a pattern, it finds the first match that
    -- PCRE2 finds
    -- MiniRegex が動かせるパターンなら、PCRE2 と同じ最初の一致を見つける
    prop "finds the first match PCRE2 finds" $
      forAll genPattern $ \p -> forAll genText $ \t ->
        case compile p of
          Left _   -> label "not yet" True
          Right re -> label "compared" $
            fmap matchText (find re t) === Pcre2.match p t
    -- and all the matches; only a and b here, as the pcre2 library
    -- steps into the middle of 日 after an empty match. Empty matches
    -- are left out: what comes after them waits for the chapter on
    -- groups (the library, MiniRegex and pcre2test all differ there)
    -- 全部の一致も。ここでは a と b だけ（pcre2 のライブラリは、空の
    -- 一致のあと、日の途中のバイトへ進んでしまう）。空の一致があるものは
    -- 外す。そのあとの進み方は、グループの章で（ライブラリと MiniRegex と
    -- pcre2test が、みんなちがう）
    prop "finds all the matches PCRE2 finds" $
      forAll genPattern $ \p -> forAll genAscii $ \t ->
        case compile p of
          Left _ -> label "not yet" True
          Right re
            | any (\m -> matchStart m == matchEnd m) (findAll re t) ->
                label "empty" True
            | otherwise -> label "compared" $
                map matchText (findAll re t) === Pcre2.match p t
    -- random trees, too, and the flags m and s; trees with a group
    -- repeated 0 times are only counted, and set aside
    -- でたらめな木でも。フラグ m と s も。0回くり返すグループがある木は、
    -- 数えるだけで、脇に置く
    prop "finds what PCRE2 finds, on random trees" $
      forAll (genTree [Greedy, Lazy] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
          forAll (sublistOf [Multiline, DotAll]) $ \opts ->
            let p = render tr
            in  case compileWith opts p of
                  Left e -> counterexample (show e) False
                  Right re
                    | hasZeroGroup tr ->
                        label "set aside: a group repeated 0 times" True
                    | otherwise -> label "compared" $
                        fmap matchText (find re t)
                          === Pcre2.matchOpt (pcre2Options opts) p t
    -- often enough, it compares (not only "not yet")
    -- 「まだ」ばかりでなく、ちゃんとくらべている
    prop "compares often enough" $
      checkCoverage $ forAll genPattern $ \p ->
        cover 70 (either (const False) (const True) (compile p))
          "compared" True
    -- an escaped text is found as it is, by both
    -- escape した文字列は、どちらでも、そのまま見つかる
    prop "escape finds the text as it is (answer of step 3)" $
      \s -> let t = T.pack s
            in  fmap matchText (find (surely (escape t)) t) === Just t
                  .&&. take 1 (Pcre2.match (escape t) t) === [t]
  -- ANCHOR_END: pcre2

  -- ANCHOR: parser
  describe "the parser / パーサー" $ do
    -- the date pattern of the design chapter
    -- 設計の章の、日付のパターン
    it "reads [0-9]{4}" $
      parsePattern [] "[0-9]{4}" `shouldBe` Right (Pattern (Branch
        [ Quantified (Class (CharClass False (Range '0' '9' :| [])))
                     (Quantifier (Count 4 (Just 0)) Greedy) ] :| []))
    -- the trees written by hand in the design chapter's training
    -- 設計の章のトレーニングで、手で書いた木
    it "reads the trees of the design chapter's training" $ do
      parsePattern [] "colou?r" `shouldBe` Right colourTree
      parsePattern [] "(?:ab)+|c" `shouldBe` Right choiceTree
    -- a { that is not a quantifier is the character {
    -- 量指定子でない { は、文字の {
    it "reads a { that is not a quantifier as itself" $ do
      fmap render (parsePattern [] "x{a}") `shouldBe` Right "x\\{a}"
      fmap render (parsePattern [] "a{,}") `shouldBe` Right "a\\{,}"
      fmap render (parsePattern [] "a{ 2 , 3 }") `shouldBe` Right "a{2,3}"
      fmap render (parsePattern [] "a{,3}") `shouldBe` Right "a{0,3}"
    -- lazy and possessive
    -- 控えめと、所有的
    it "reads ? and + after a quantifier" $
      fmap render (parsePattern [] "a*?b++c{1}?")
        `shouldBe` Right "a*?b++c{1}?"
    -- \Q...\E, comments, and escapes for one character
    -- \Q...\E、コメント、1文字を表す \
    it "reads \\Q...\\E, comments and escapes" $ do
      fmap render (parsePattern [] "a\\Q.*\\E+") `shouldBe` Right "a\\.\\*+"
      fmap render (parsePattern [] "a(?#note)*") `shouldBe` Right "a*"
      parsePattern [] "\\x41\\x{263a}\\o{101}\\t\\cA"
        `shouldBe` Right (literal "A\x263a\&A\t\SOH")
    -- named groups, the three ways of writing them
    -- 名前のグループ。3つの書き方
    it "reads named groups" $
      fmap render (parsePattern [] "(?<y>a)(?'m'b)(?P<d>c)")
        `shouldBe` Right "(?<y>a)(?<m>b)(?<d>c)"
    -- what the tree has no place for yet
    -- 木に、まだ居場所のないもの
    it "says not yet to what is for later chapters" $ do
      parsePattern [] "a\\d" `shouldBe` Left (Unsupported 1 "\\d")
      parsePattern [] "x(?=y)" `shouldBe` Left (Unsupported 1 "(?=")
      parsePattern [] "(?i)a" `shouldBe` Left (Unsupported 0 "(?flags)")
      parsePattern [Extended] "a b" `shouldBe` Left (Unsupported 0 "flag x")
  -- ANCHOR_END: parser

  -- ANCHOR: errors
  describe "wrong patterns, like PCRE2 10.49 / まちがい" $ do
    -- each place and message was checked with pcre2test 10.49
    -- 場所と文は、どれも pcre2test 10.49 で確かめた
    let wrong p at m = it (T.unpack p) $
          parsePattern [] p `shouldBe` Left (Invalid at m)
        e104 = "numbers out of order in {} quantifier"
        e105 = "number too big in {} quantifier"
        e106 = "missing terminating ] for character class"
        e108 = "range out of order in character class"
        e109 = "quantifier does not follow a repeatable item"
    wrong "ab\\"    3  "\\ at end of pattern"                      -- 101
    wrong "\\y"     2  "unrecognized character follows \\"         -- 103
    wrong "a{5,2}"   5  e104
    wrong "a{ 5 , 2 }" 8 e104
    wrong "{5,2}"    4  e104
    wrong "a{70000}" 7  e105
    wrong "[abc"     4  e106
    wrong "[]"       2  e106
    wrong "[z-a]"    4  e108
    wrong "*a"       1  e109
    wrong "a**"      3  e109
    wrong "a**?"     3  e109
    wrong "^*"       2  e109
    wrong "a|*b"     3  e109
    wrong "a{2}{3}"  7  e109
    wrong "(?#c)*"   6  e109
    wrong "(ab"      3  "missing closing parenthesis"              -- 114
    wrong "(?#abc"   6  "missing ) after (?# comment"              -- 118
    wrong "a)b)"     2  "unmatched closing parenthesis"            -- 122
    wrong "\\x{110000}" 9
      "character code point value in \\x{} or \\o{} is too large" -- 134
    wrong "(?<name"  7
      "syntax error in subpattern name (missing terminator?)"     -- 142
    wrong "(?<n>a)(?<n>b)" 12
      "two named subpatterns have the same name (PCRE2_DUPNAMES not set)"
    wrong "(?<1a>x)" 4  "subpattern name must start with a non-digit"
    wrong "(?<>x)"   3  "subpattern name expected"                 -- 162
    wrong "\\x{4g}"  5
      "non-hex character in \\x{} (closing brace missing?)"         -- 167
    wrong "\\x{d800}" 7
      "disallowed Unicode code point (>= 0xd800 && <= 0xdfff)"    -- 173
    wrong "\\x{"     3
      "digits missing after \\x or in \\x{} or \\o{} or \\N{U+}"  -- 178
    wrong "\\xg"     2
      "digits missing after \\x or in \\x{} or \\o{} or \\N{U+}"  -- 178
  -- ANCHOR_END: errors

  -- ANCHOR: roundtrip
  describe "writing back / 書き戻す" $ do
    -- write a tree back, read it again: the same tree
    -- 木を書き戻して、読み直すと、同じ木
    prop "reads back what render wrote" $
      forAll (genTree [Greedy, Lazy, Possessive] 2) $ \tr ->
        parsePattern [] (render tr) === Right tr
    -- and named groups
    -- 名前のグループも
    it "writes back named groups" $ do
      let tree = parsePattern [] "(?<year>[0-9]{4})-(?<m>..)"
      (tree >>= parsePattern [] . render) `shouldBe` tree
  -- ANCHOR_END: roundtrip

  -- ANCHOR: pcre2errors
  describe "wrong patterns, against the real PCRE2 / 本物とくらべる" $ do
    -- when MiniRegex says "wrong", PCRE2 refuses it with the same
    -- words; when MiniRegex reads it, PCRE2 takes it
    -- MiniRegex が「まちがい」と言えば、PCRE2 も同じ文で断る。
    -- MiniRegex が読めたら、PCRE2 も受け取る
    prop "agrees with PCRE2 on what is wrong" $
      forAll genWild $ \p -> ioProperty $ do
        answer <- pcre2Error p
        pure $ case parsePattern [] p of
          Left (Unsupported _ _) -> label "not yet" True
          Left (Invalid _ m)     -> label "wrong" (answer === Just m)
          Right _                -> label "read" (answer === Nothing)
  -- ANCHOR_END: pcre2errors

  -- ANCHOR: answers
  describe "the training / トレーニング" $ do
    -- step 2: POSIX takes the longest
    -- ステップ2：POSIX は、いちばん長いものを取る
    it "finds the longest, like POSIX" $ do
      fmap matchText (findLongest (surely "a|ab") "abc") `shouldBe` Just "ab"
      fmap matchText (findLongest (surely "a*?") "aaab")
        `shouldBe` Just "aaa"
      findLongest (surely "x") "abc" `shouldBe` Nothing
    -- the longest starts where the first starts, and is not shorter
    -- いちばん長いものは、最初のものと同じ場所で始まり、短くない
    prop "the longest is never shorter than the first" $
      forAll genPattern $ \p -> forAll genAscii $ \t ->
        case (compile p, compile p) of
          (Right re, _) -> case (find re t, findLongest re t) of
            (Nothing, Nothing) -> property True
            (Just m, Just l)   ->
              matchStart l === matchStart m
                .&&. property (matchEnd l >= matchEnd m)
            other -> counterexample (show other) False
          _ -> property True
    -- step 3: the whole text, with the rest of the work
    -- ステップ3：残りの仕事で、文字列の全部
    it "matches the whole text" $ do
      fullMatch (surely "a|ab") "ab" `shouldBe` True
      fullMatch (surely "a*") "aab" `shouldBe` False
      fullMatch (surely "(?:ab|a)(?:c|bcd)") "abcd" `shouldBe` True
    -- the same as asking the list for a candidate at the end
    -- 候補のリストに、終わりの場所があるかと同じ
    prop "fullMatch agrees with the list of candidates" $
      forAll genPattern $ \p -> forAll genAscii $ \t ->
        case compile p of
          Left _   -> property True
          Right re -> fullMatch re t === any (T.null . rest)
                        (ends (options re) (tree re) (Cursor 0 Nothing t))
  -- ANCHOR_END: answers
