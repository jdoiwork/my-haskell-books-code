module Main (main) where

-- ANCHOR: imports
import Control.Exception (SomeException, evaluate, try)
import System.Timeout (timeout)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (once)
import Text.Regex.Pcre2 qualified as Pcre2

import Data.Map.Strict qualified as Map

import MiniRegex
import MiniRegex.Compat
import MiniRegex.Engine (nonEmptyAt, searchFromList)
import MiniRegex.Examples
import MiniRegex.Groups (groupCount)
import MiniRegex.Parser
import MiniRegex.Pretty
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

-- 🐢 The same, with PCRE2's start optimizations turned off. They can
--    change the answer when a group refers back to itself: (bb|\1*a)
--    finds nothing in "\na" with them, and "a" without them, as
--    MiniRegex does.
-- 同じものに、PCRE2 の「探し始める前の最適化」を切るオプションを足す。
-- グループが自分自身を参照すると、最適化で答えが変わることがある：
-- (bb|\1*a) は "\na" に、最適化ありだと何も見つけず、なしだと
-- MiniRegex と同じく "a" を見つける
pcre2Exact :: [Option] -> Pcre2.Option
pcre2Exact opts = Pcre2.NoStartOptimize <> pcre2Options opts

-- 🧵 a, x, b, y, c ...: one from each, in turn.
-- 交互に1つずつ並べる
weave :: [a] -> [a] -> [a]
weave (a : as) bs = a : weave bs as
weave []       bs = bs

-- 🧪 surely, with flags.
-- フラグつきの surely
surelyWith :: [Option] -> Text -> Regex
surelyWith opts src = either (error . show) id (compileWith opts src)
-- 🧺 The whole match and every group, a group that found nothing as
--    "" (the way the pcre2 library gives them).
-- 一致の全体と全部のグループ。何も見つけなかったグループは ""
-- （pcre2 のライブラリの渡し方）
allGroups :: Match -> NonEmpty Text
allGroups m = matchText m :| map (maybe "" spanText) (matchGroups m)

-- 🎲 A template with the groups 0 to n, $$ and plain characters.
-- 0 から n のグループ、$$、ふつうの文字でできたテンプレート
genTemplate :: Int -> Gen Text
genTemplate n = T.concat <$> resize 5 (listOf (elements parts))
  where
    parts = ["x", "-", "$$", "$0"]
         ++ concat [["$" <> num i, "${" <> num i <> "}"] | i <- [1 .. n]]
    num   = T.pack . show

-- ⏱️ The two answers are the same. But give up after a second, as the
--    meter does: a random tree can be of the doubling kind, like
--    (a+)+b. A pair that PCRE2 gave up on (its match limit) is set
--    aside, too.
-- 2つの答えが同じ。ただし、メーターと同じく、1秒であきらめる。でたらめな
-- 木が、(a+)+b のような倍々の木のこともあるから。PCRE2 があきらめた組
-- （ステップ数の上限）も、脇に置く
agreeWithin :: (Eq a, Show a) => a -> a -> Property
agreeWithin ours theirs = ioProperty $ do
  answer <- timeout 1000000 (try (evaluate (ours == theirs)))
  pure $ case answer of
    Nothing -> label "set aside: slow" True
    Just (Left (_ :: Pcre2.SomePcre2Exception)) ->
      label "set aside: PCRE2 gave up" True
    Just (Right same) ->
      counterexample (show ours ++ " /= " ++ show theirs) same
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
      | depth > 0 ] ++
      [ (1, Backref <$> chooseInt (1, 3)) ]
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

-- 🔗 Make every \n in a tree point at a group that is there (or, with
--    no group at all, turn it into the character a).
-- 木の中の \n が、どれも本当にあるグループを指すようにする（グループが
-- 1つもなければ、文字の a にする）
fixRefs :: Pattern -> Pattern
fixRefs tr = goP tr
  where
    count = groupCount tr
    goP (Pattern bs) = Pattern (fmap goB bs)
    goB (Branch ps)  = Branch (map goPiece ps)
    goPiece (Quantified a q) = Quantified (goA a) q
    goPiece other            = other
    goA (Group c p) = Group c (goP p)
    goA (Backref n)
      | count == 0 = Lit 'a'
      | otherwise  = Backref ((n - 1) `mod` count + 1)
    goA a = a

-- 🌳 Random trees whose references are all right.
-- 参照がどれも正しい、でたらめな木
genTreeRefs :: [Greed] -> Int -> Gen Pattern
genTreeRefs greeds depth = fixRefs <$> genTree greeds depth

-- 🌳 The same, without a group repeated {0}: PCRE2 calls (a|^){0}
--    "anchored" and looks only at the start (pcre2test's info says
--    so), which MiniRegex does not copy.
-- 同じもの。ただし {0} でくり返すグループはなし。PCRE2 は (a|^){0} を
-- 「頭に固定」とみなして、頭しか見ない（pcre2test の info で見える）。
-- MiniRegex は、それをまねしない
genTreeForPcre2 :: Gen Pattern
genTreeForPcre2 = genTreeRefs [Greedy, Lazy] 2 `suchThat` noZeroGroup
  where
    noZeroGroup (Pattern bs) = all branchOk bs
    branchOk (Branch ps) = all pieceOk ps
    pieceOk (Quantified (Group _ p) q) =
      count q /= Count 0 (Just 0) && noZeroGroup p
    pieceOk _ = True

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
    -- ANCHOR: matchfields
    -- find gives the first one, not any one
    -- find が返すのは、どれかではなく、最初の1つ
    it "finds the first one" $
      fmap (\m -> (matchText m, matchStart m, matchEnd m))
        (find (surely "a") "banana")
        `shouldBe` Just ("a", 1, 2)
    -- isMatch says yes and no
    -- isMatch は「ある」も「ない」も言う
    it "tells if it is there" $ do
      isMatch (surely "nan") "banana" `shouldBe` True
      isMatch (surely "nab") "banana" `shouldBe` False
    -- places are counted in characters
    -- 場所は文字で数える
    it "counts places in characters" $
      fmap (\m -> (matchText m, matchStart m, matchEnd m))
        (find (surely "b") "日本b") `shouldBe` Just ("b", 2, 3)
    -- ANCHOR_END: matchfields
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
    -- answers are written like pcre2test, with the groups
    -- 答えは、pcre2test と同じ形で、グループの行も書く
    it "shows an answer like pcre2test" $ do
      let plain = Modifiers [] False False
          withAfter = Modifiers [] False True
      showFound plain "" (findAll (surely "a\tb\xe9") "a\tb\xe9")
        `shouldBe` [" 0: a\\x09b\\xe9"]
      showFound plain "" (findAll (surely "(a)(x)?(c)?") "ab")
        `shouldBe` [" 0: a", " 1: a"]
      showFound plain "" (findAll (surely "(a)(x)?(b)") "ab")
        `shouldBe` [" 0: ab", " 1: a", " 2: <unset>", " 3: b"]
      showFound withAfter "xaybz" (findAll (surely "(a)") "xaybz")
        `shouldBe` [" 0: a", " 0+ ybz", " 1: a"]
      showFound plain "" [] `shouldBe` ["No match"]
    -- the modifiers g and aftertext are known now
    -- 修飾子 g と aftertext も、わかるようになった
    it "reads the modifiers" $ do
      flagsOf "g,aftertext,im"
        `shouldBe` Right (Modifiers [Caseless, Multiline] True True)
      flagsOf "mark" `shouldBe` Left "mark"
    -- the whole of testoutput1: no wrong answers, and the parser
    -- calls no right pattern wrong
    -- testoutput1 の全部：ちがう答えは1つもなく、パーサーは正しい
    -- パターンを「まちがい」と言わない
    it "reads the patterns of 939 tests, passes 750, and fails none" $ do
      tests <- readTestFile "testdata/testoutput1"
      tally (map readable tests) `shouldBe` Tally 939 0 1709
      verdicts <- mapM (judgeWithin 1000000) tests
      tally verdicts `shouldBe` Tally 750 0 1898
    -- every line counts now: the groups', too
    -- いまは全部の行をくらべる。グループの行も
    it "compares the groups' lines too" $ do
      judge (TestCase 1 "(a)b" "" "ab" [" 0: ab", " 1: a"]) `shouldBe` Pass
      judge (TestCase 1 "(a)b" "" "ab" [" 0: ab", " 1: b"]) `shouldBe` Fail
      judge (TestCase 1 "a*" "g" "ab" [" 0: a", " 0: ", " 0: "])
        `shouldBe` Pass
  -- ANCHOR_END: meter

  -- ANCHOR: matcher
  describe "the matcher / マッチャー" $ do
    -- each answer was checked with pcre2test 10.49
    -- 答えは、どれも pcre2test 10.49 で確かめた
    -- both matchers: the continuations (find) and the list
    -- 2つのマッチャーの両方：継続（find）と、候補のリスト
    let finds opts p t answer = it (show (opts, p, t)) $ do
          let re = surelyWith opts p
          fmap matchText (find re t) `shouldBe` answer
          fmap matchText (searchFromList re t 0) `shouldBe` answer
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
    -- after \1, ^ looks at the character the reference took
    -- \1 のあとの ^ は、参照が食べた文字を見る
    finds [Multiline] "(a)\n\\1^b" "a\nab" Nothing
    it "finds ^ and $ at each line with m" $ do
      map matchStart (findAll (surelyWith [Multiline] "^") "a\nb\n")
        `shouldBe` [0, 2]
      map matchStart (findAll (surelyWith [Multiline] "$") "a\nb\n")
        `shouldBe` [1, 3, 4]
      -- the next search starts just after a newline: ^ looks back at it
      -- 次の探しものが改行のすぐうしろから始まる。^ は、その改行を見る
      map matchText (findAll (surelyWith [Multiline] "a\n|^b") "a\nb")
        `shouldBe` ["a\n", "b"]
    -- (a+)+ lists 2^n - 1 places on n a's
    -- (a+)+ は、a が n 個なら 2^n - 1 個の場所を並べる
    it "lists 2^n - 1 ends for (a+)+" $
      [endsAtStart "(a+)+" (T.replicate n "a" <> "c") | n <- [1 .. 10]]
        `shouldBe` [2 ^ n - 1 | n <- [1 .. 10 :: Int]]
    -- the list of candidates and the continuations agree, groups and
    -- references too
    -- 候補のリストと継続は、同じ答え。グループと後方参照も
    prop "the two matchers agree" $
      -- (short texts: a backtracking matcher can take very long on
      --  a long text that does not match)
      -- （短い文字列で。バックトラックのマッチャーは、合わない長い
      --   文字列に、とても長くかかることがある）
      forAll (genTreeRefs [Greedy, Lazy] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (sublistOf [Multiline, DotAll]) $ \opts ->
          case compileWith opts (render tr) of
            Left e   -> counterexample (show e) False
            Right re -> agreeWithin (searchFromList re t 0) (find re t)
  -- ANCHOR_END: matcher

  -- ANCHOR: groups
  describe "groups / グループ" $ do
    -- each answer was checked with pcre2test 10.49
    -- 答えは、どれも pcre2test 10.49 で確かめた
    -- both matchers: the continuations (find) and the list
    -- 2つのマッチャーの両方：継続（find）と、候補のリスト
    let finds p t answer = it (show (p, t)) $ do
          fmap groups (find (surely p) t) `shouldBe` answer
          fmap groups (searchFromList (surely p) t 0) `shouldBe` answer
    -- a group that took no part is Nothing
    -- 使われなかったグループは Nothing
    finds "(a)|(b)"       "b"     (Just [Nothing, Just "b"])
    finds "(a)(b)?(c)"    "ac"    (Just [Just "a", Nothing, Just "c"])
    -- in a repeat, the last time
    -- くり返しの中なら、最後の回
    finds "(a|b)*c"       "abac"  (Just [Just "a"])
    finds "((a)|b)+"      "ab"    (Just [Just "b", Just "a"])
    -- a group keeps what it found, even if a later round skips it
    -- あとの回が通らなくても、前に見つけたものは残る
    finds "(?:(a)|b)*"    "ab"    (Just [Just "a"])
    finds "(?:(a)|(b))+"  "ab"    (Just [Just "a", Just "b"])
    -- the empty last round counts
    -- 最後の空回りの1回も数に入る
    finds "(a*)*"         "aab"   (Just [Just ""])
    finds "(a|)+"         "ab"    (Just [Just ""])
    -- with an upper end, the empty times count (both matchers)
    -- 上の端があれば、空の回も数える（2つのマッチャーで）
    finds "(|b){2,5}$"    "bb"    (Just [Just "b"])
    -- a group inside a group comes after it: i is 2
    -- 中のグループは外のあと。i は2番
    finds "(?<o>a(?<i>b))" "ab"   (Just [Just "ab", Just "b"])
    -- with an upper end, the empty times count too, up to the end
    -- 上の端があれば、空の回も数えて、端まで回る
    it "counts the empty times of (|b){2,5}" $
      fmap (\m -> (matchText m, groups m))
        (nonEmptyAt (surely "(|b){2,5}") "bbbx" 0)
        `shouldBe` Just ("b", [Just "b"])
    -- names and numbers, and where
    -- 名前と番号、場所
    it "takes groups by number and by name, with places" $ do
      let m = find datePattern "On 2026-11-19"
      (m >>= group 0) `shouldBe` Just "2026-11-19"
      (m >>= group 2) `shouldBe` Just "11"
      (m >>= named "day") `shouldBe` Just "19"
      (m >>= named "week") `shouldBe` Nothing
      (m >>= group 4) `shouldBe` Nothing
      (m >>= namedSpan "month") `shouldBe` Just (Span "11" 8 10)
      (find (surely "(?<o>a(?<i>b))") "ab" >>= named "i") `shouldBe` Just "b"
    -- the examples of the chapter
    -- 章の例
    it "takes the dates apart and turns them round" $ do
      take 2 (dateParts records)
        `shouldBe` [("2026", "11", "19"), ("2026", "11", "20")]
      T.lines (toDayFirst records) !! 0
        `shouldBe` "19/11/2026 Mira sold a dragon scale: 300 gold"
      fmap T.lines (toDayFirstT records)
        `shouldBe` Right (T.lines (toDayFirst records))
  -- ANCHOR_END: groups

  -- ANCHOR: backrefs
  describe "back references / 後方参照" $ do
    -- each answer was checked with pcre2test 10.49
    -- 答えは、どれも pcre2test 10.49 で確かめた
    -- both matchers: the continuations (find) and the list
    -- 2つのマッチャーの両方：継続（find）と、候補のリスト
    let finds p t answer = it (show (p, t)) $ do
          fmap matchText (find (surely p) t) `shouldBe` answer
          fmap matchText (searchFromList (surely p) t 0) `shouldBe` answer
    finds "(a)\\1"             "aa"     (Just "aa")
    finds "(a)\\1"             "ab"     Nothing
    -- a group that has found nothing makes \1 fail
    -- 何も見つけていないグループなら、\1 は失敗
    finds "(a)?\\1"            "b"      Nothing
    finds "(a)|\\1b"           "b"      Nothing
    finds "\\1(a)"             "aa"     Nothing
    finds "(a\\1)"             "aa"     Nothing
    -- inside its own repeat: the time before
    -- 自分のくり返しの中なら、前の回のもの
    finds "(a|b\\1)+"          "aba"    (Just "aba")
    -- a group can repeat itself 0 times (PCRE2 agrees with its start
    -- optimizations turned off: pcre2/nso.txt)
    -- 自分自身を0回くり返せる（PCRE2 も、最適化を切れば同じ：pcre2/nso.txt）
    finds "(bb|\\1*a)"         "\na"    (Just "a")
    -- the + of a group that refers to itself: no second round after an
    -- empty first one (the lastOfMany of repeatAtom)
    -- 自分を参照するグループの +：空の1回目のあとは回らない
    -- （repeatAtom の lastOfMany）
    finds "(?P<abn>(?P=abn)xxx|)+" "xxx"  (Just "")
    -- \10 is a reference when 10 groups have opened before it; if not,
    -- it is \010 (the backspace, \b)
    -- \10 は、その前に10のグループが開いていれば後方参照。でなければ
    -- \010（バックスペース、\b）
    finds "(a)(b)(c)(d)(e)(f)(g)(h)(i)\\10(j)" "abcdefghi\bj"
      (Just "abcdefghi\bj")
    finds "\\10(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)" "\babcdefghij"
      (Just "\babcdefghij")
    finds "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j\\10)" "abcdefghij\b" Nothing
    -- a name inside a group, (?: that does not count, and +1
    -- グループの中の名前、数に入らない (?:、+1
    finds "(?<o>a(?<i>b))\\k<i>" "abb"   (Just "abb")
    finds "(a)(?:b)\\g{-1}"     "aba"    (Just "aba")
    finds "(?:\\g{+1}x|(a))+"   "aax"    (Just "aax")
    -- the ways of writing it
    -- 書き方いろいろ
    finds "(?<y>[0-9]+)-\\k<y>" "12-12" (Just "12-12")
    finds "(?<x>a)\\k'x'\\k{x}\\g{x}(?P=x)\\g{1}\\g1\\g{-1}\\g-1"
                               "aaaaaaaaa" (Just "aaaaaaaaa")
    finds "(?'name'ab)\\k{ name }(?P=name)" "XabababX" (Just "ababab")
    finds "\\k<x>(?<x>a)"      "a"      Nothing
    -- \10 is a reference with ten groups, octal without
    -- \10 は、グループが10あれば後方参照、なければ8進
    finds "\\10(a)"            "\x08\&a" (Just "\x08\&a")
    finds (T.replicate 10 "(a)" <> "\\10") (T.replicate 11 "a")
                               (Just (T.replicate 11 "a"))
    finds "[\\1]"              "\x01"   (Just "\x01")
    -- a word written twice, and what \b would fix
    -- 2回書いた言葉と、\b が直すもの
    it "finds the words written twice (and a wrong one)" $
      doubledWords notes `shouldBe` ["the", "n"]
  -- ANCHOR_END: backrefs

  -- ANCHOR: referrors
  describe "wrong references, like PCRE2 10.49 / まちがった参照" $ do
    -- each place and message was checked with pcre2test 10.49
    -- 場所と文は、どれも pcre2test 10.49 で確かめた
    let wrong p at m = it (T.unpack p) $
          parsePattern [] p `shouldBe` Left (Invalid at m)
        e115 = "reference to non-existent subpattern"
    wrong "(a)\\2"      5 e115
    wrong "\\8"         2 e115
    wrong "(a)\\81"     6 e115
    wrong "(a)\\g2"     6 e115
    wrong "(a)\\g{2}"   8 e115
    wrong "(a)\\g-2"    7 e115
    wrong "(a)\\g{-2}"  5 e115
    wrong "(a)\\g{0}"   8 e115
    wrong "(a)\\g{+1}"  9 e115
    wrong "(a)\\k<x>"   6 e115
    wrong "(a)\\k'x'"   6 e115
    wrong "(a)\\k{x}"   6 e115
    wrong "(a)\\g{x}"   6 e115
    wrong "(a)(?P=x)"   7 e115
    wrong "(a)\\g{-0}"  5 "a relative value of zero is not allowed"  -- 126
    wrong "(a)\\g+0"    7 "a relative value of zero is not allowed"
    wrong "(a)\\g-0"    7 "a relative value of zero is not allowed"
    wrong "\\g{+0"      2 "a relative value of zero is not allowed"
    wrong "(a)\\99999"  9 "subpattern number is too big"            -- 161
    -- 65535 is the largest group number; above it, the number is told
    -- 65535 がいちばん大きいグループの番号。超えたら、番号のことを言う
    wrong "\\g{65535}"     9 e115
    wrong "\\g{65536}"     2 "subpattern number is too big"          -- 161
    wrong "\\g65536"       7 "subpattern number is too big"
    wrong "(a)\\g{-65536}" 5 "subpattern number is too big"
    wrong "(a)\\g-65536"  11 "subpattern number is too big"
    wrong "(a)\\k<1>"   7 "subpattern name must start with a non-digit"
    wrong "(a)\\k<>"    6 "subpattern name expected"                 -- 162
    wrong "(a)\\g{}"    6 "subpattern name expected"
    wrong "(a)\\g{-a}"  6 "subpattern name expected"
    wrong "(?P=)"       4 "subpattern name expected"
    wrong "(a)\\k<x"    7
      "syntax error in subpattern name (missing terminator?)"     -- 142
    wrong "(a)\\g{1x}"  7
      "syntax error in subpattern number (missing terminator?)"   -- 219
    wrong "(a)\\g"      5
      "\\g is not followed by a braced, angle-bracketed, or quoted\
      \ name/number or by a plain number"                          -- 157
    wrong "(a)\\k"      5
      "\\k is not followed by a braced, angle-bracketed, or quoted name"
    -- \g<1> calls a group: the chapter on recursion
    -- \g<1> はグループの呼び出し：再帰の章で
    it "leaves \\g<1> for later" $
      parsePattern [] "(a)\\g<1>" `shouldBe` Left (Unsupported 3 "\\g<")
  -- ANCHOR_END: referrors

  -- ANCHOR: findall
  describe "findAll and split / 全部の一致と切れ端" $ do
    -- each answer was checked with pcre2test 10.49 (/g)
    -- 答えは、どれも pcre2test 10.49 の /g で確かめた
    let all' p t answer = it (show (p, t)) $
          map matchText (findAll (surely p) t) `shouldBe` answer
    all' "x*"   "axbxx" ["", "x", "", "xx", ""]
    all' "a??"  "a"     ["", "a", ""]
    all' "$"    "a"     [""]
    all' "a*"   "abbab" ["a", "", "", "a", "", ""]
    all' ""     "日本"  ["", "", ""]
    -- the same as Python's re.split
    -- Python の re.split と同じ
    it "splits around empty matches, too" $
      split (surely "x*") "axbxx" `shouldBe` ["", "a", "", "b", "", ""]
    -- the pieces and the matches, one after the other, make the text
    -- 切れ端と見つけたものを交互に並べると、もとの文字列
    prop "pieces and matches, put together, are the text" $
      forAll genPattern $ \p -> forAll genText $ \t ->
        case compile p of
          Left _   -> property True
          Right re -> T.concat (weave (split re t)
                        (map matchText (findAll re t))) === t
  -- ANCHOR_END: findall

  -- ANCHOR: templates
  describe "templates / テンプレート" $ do
    -- each answer was checked with pcre2test 10.49 (replace=)
    -- 答えは、どれも pcre2test 10.49 の replace= で確かめた
    let ymd = surely "(?<y>[0-9]{4})-(?<m>[0-9]{2})"
        subs tpl t answer = it (show (tpl, t)) $
          fmap (\x -> replaceTemplate ymd x t) (template ymd tpl)
            `shouldBe` Right answer
        wrong tpl at m = it (show tpl) $
          template ymd tpl `shouldBe` Left (Invalid at m)
    subs "$m.$y.$$5.$1x.${1}x" "2026-11" "11.2026.$5.2026x.2026x"
    subs "<$01x>"              "2026-11" "<2026x>"
    subs "${0}!"               "2026-11" "2026-11!"
    wrong "${dya}"  6 "unknown substring"                         -- -49
    wrong "$9"      2 "unknown substring"
    wrong "<$12>"   3 "unknown substring"
    wrong "$x"      2 "unknown substring"
    wrong "${1"     3
      "expected closing curly bracket in replacement string"      -- -58
    -- after a digit, only digits, then }
    -- 数字のあとは数字だけ。そのあとに }
    wrong "${1x}"   3
      "expected closing curly bracket in replacement string"
    wrong "${12}"   3 "unknown substring"
    wrong "$"       1 "invalid replacement string"                -- -35
    wrong "$-"      1 "invalid replacement string"
    wrong "<${}>"   3 "invalid replacement string"
    wrong "<${ 1 }>" 3 "invalid replacement string"
    -- replaceTemplate changes only the first one
    -- replaceTemplate は最初の1つだけ
    it "replaceTemplate changes only the first one" $
      fmap (\x -> replaceTemplate (surely "(a)") x "aa")
        (template (surely "(a)") "<$1>")
        `shouldBe` Right "<a>a"
    -- a Japanese name; $*MARK is not yet
    -- 日本語の名前。$*MARK は、まだ
    it "takes a Japanese name, and says not yet to $*MARK" $ do
      fmap (\x -> replaceAllTemplate (surely "(?<年>a)") x "a")
        (template (surely "(?<年>a)") "<${年}>")
        `shouldBe` Right "<a>"
      template (surely "(a)") "<$*MARK>"
        `shouldBe` Left (Unsupported 1 "$*MARK")
    -- a group that found nothing puts in nothing
    -- 何も見つけなかったグループは、何も差しこまない
    it "puts nothing in for a group that found nothing" $
      fmap (\x -> replaceAllTemplate (surely "(a)|(b)") x "ab")
        (template (surely "(a)|(b)") "<$2>")
        `shouldBe` Right "<><b>"
  -- ANCHOR_END: templates

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
    -- and all the matches, through PCRE2's own global replace (the
    -- library's own way of finding them all steps into the middle of
    -- 日 after an empty match, but pcre2_substitute does it right)
    -- 全部の一致も、PCRE2 自身の「全部を置き換える」を通して
    -- くらべる（ライブラリの「全部を探す」は、空の一致のあと、日の
    -- 途中のバイトへ進んでしまうが、pcre2_substitute は正しく進む）
    prop "finds all the matches PCRE2 finds" $
      forAll genPattern $ \p -> forAll genText $ \t ->
        case compile p of
          Left _   -> label "not yet" True
          Right re -> label "compared" $
            replaceWith re (\m -> "<" <> matchText m <> ">") t
              === Pcre2.gsub p "<$0>" t
    -- random trees, too, with the flags m and s; the groups, too
    -- でたらめな木でも。フラグ m と s も。グループもくらべる
    -- (the library writes a group that found nothing as "")
    -- （ライブラリは、何も見つけなかったグループを "" と書く）
    prop "finds what PCRE2 finds, groups too, on random trees" $
      forAll genTreeForPcre2 $ \tr ->
        forAll (resize 12 genLines) $ \t ->
          forAll (sublistOf [Multiline, DotAll]) $ \opts ->
            let p = render tr
            in  case compileWith opts p of
                  Left e   -> counterexample (show e) False
                  Right re ->
                    agreeWithin (fmap allGroups (find re t))
                      (Pcre2.capturesOpt (pcre2Exact opts) p t)
    -- the templates, against pcre2_substitute (with unset groups as "")
    -- テンプレートも、pcre2_substitute とくらべる（何も見つけなかった
    -- グループは ""）
    prop "replaces what PCRE2 replaces, with a template" $
      forAll genTreeForPcre2 $ \tr ->
        forAll (genTemplate (groupCount tr)) $ \tpl ->
        forAll (resize 12 genAscii) $ \t ->
          let p = render tr
          in  counterexample (T.unpack p) $
              case (compile p, template (surely p) tpl) of
                (Right re, Right x) ->
                  agreeWithin (replaceAllTemplate re x t)
                    (Pcre2.subOpt
                      (pcre2Exact []
                        <> Pcre2.SubGlobal <> Pcre2.SubUnsetEmpty)
                      p tpl t)
                other -> counterexample (show other) False
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
      parsePattern [] "(?<*a)b" `shouldBe` Left (Unsupported 0 "(?<*")
    -- what looks like a POSIX class but is not one, and \ca
    -- POSIX の名前に見えて、そうではないもの。\ca
    it "reads look-alikes of POSIX classes, and \\ca" $ do
      parsePattern [] "[:a:b]" `shouldBe` parsePattern [] "[\\:a:b]"
      parsePattern [] "[^:alpha:]"
        `shouldSatisfy` either (const False) (const True)
      parsePattern [] "\\ca" `shouldBe` parsePattern [] "\\cA"
    -- render puts a \ before a : at the head of a set ([\:-b:])
    -- render は、集まりの頭の : に \ を付ける（[\:-b:]）
    it "writes [\\:-b:], not [:-b:]" $
      fmap render (parsePattern [] "[\\:-b:]") `shouldBe` Right "[\\:-b:]"
    -- a comment or a lone \\E between a quantifier and its ? or +
    -- 量指定子と ?・+ のあいだのコメントや、ひとりの \\E
    it "reads past a comment before ? and +" $ do
      parsePattern [] "a*(?#x)?" `shouldBe` parsePattern [] "a*?"
      -- ANCHOR: lonee
      -- (a lone \E there is not yet: PCRE2 10.47 and 10.49 differ)
      -- （ひとりの \E は、まだ。PCRE2 の 10.47 と 10.49 でちがう）
      parsePattern [] "a*\\E?"
        `shouldBe` Left (Unsupported 2 "\\E after a quantifier")
      -- ANCHOR_END: lonee
      parsePattern [] "a*\\Q\\E?" `shouldBe` parsePattern [] "a*?"
      parsePattern [] "a*(?#x)+" `shouldBe` parsePattern [] "a*+"
    -- the ^ is under the line of the trouble; a tab is 8 columns, and
    -- 名 and 前 are 2 each
    -- ^ は、つまずいた行の下。タブは8桁ごと、名と前は2桁ずつ
    it "points under the right line and column" $ do
      explain "a\n[z-a]" (Invalid 6 "x")
        `shouldBe` "a\n[z-a]\n    ^\nwrong pattern at 6: x\n"
      explain "\ta{5,2}" (Invalid 6 "x")
        `shouldBe` "\ta{5,2}\n             ^\nwrong pattern at 6: x\n"
      explain "名前(" (Invalid 3 "x")
        `shouldBe` "名前(\n     ^\nwrong pattern at 3: x\n"
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
    -- a number too big (or a surrogate) is told before a wrong digit
    -- or a missing }
    -- 番号の大きすぎ（とサロゲート）は、まちがった数字や } のなさより先
    wrong "\\x{123456g}" 9
      "character code point value in \\x{} or \\o{} is too large" -- 134
    wrong "\\x{110000" 9
      "character code point value in \\x{} or \\o{} is too large" -- 134
    wrong "\\x{d800g}" 7
      "disallowed Unicode code point (>= 0xd800 && <= 0xdfff)"    -- 173
    -- the other kinds the parser says, checked with pcre2test 10.49
    -- パーサーが言う、ほかの種類（pcre2test 10.49 で確かめた）
    wrong "\\c"      2  "\\c at end of pattern"                    -- 102
    wrong "[:alpha:]" 9
      "POSIX named classes are supported only within a class"     -- 112
    wrong "[.a.]"    5  "POSIX collating elements are not supported" -- 113
    wrong "\\L"      2
      "PCRE2 does not support \\F, \\L, \\l, \\N{name}, \\U, or \\u"   -- 137
    wrong ("(?<" <> T.replicate 129 "a" <> ">x)") 132
      "subpattern name is too long (maximum 128 code units)"      -- 148
    wrong "\\o"      2  "missing opening brace after \\o"         -- 155
    wrong "\\o{8}"   4
      "non-octal character in \\o{} (closing brace missing?)"      -- 164
    wrong "\\c\x01"  3
      "\\c must be followed by a printable ASCII character"         -- 168
    wrong "(?<a-b>x)" 4
      "syntax error in subpattern name (missing terminator?)"     -- 142
  -- ANCHOR_END: errors

  -- ANCHOR: roundtrip
  describe "writing back / 書き戻す" $ do
    -- write a tree back, read it again: the same tree
    -- 木を書き戻して、読み直すと、同じ木
    prop "reads back what render wrote" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
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
    -- step 2: split, keeping what the groups found, like Python
    -- ステップ2：グループが見つけたものも残す split（Python と同じ）
    it "splits and keeps the groups" $ do
      splitKeeping (surely "([,;]) *") "herb, sword;shield"
        `shouldBe` ["herb", ",", "sword", ";", "shield"]
      splitKeeping (surely "(,)|(;)") "a,b;c"
        `shouldBe` ["a", ",", "", "b", "", ";", "c"]
      splitKeeping (surely "x*") "axb"
        `shouldBe` ["", "a", "", "b", ""]
    -- with no group, it is split
    -- グループがなければ、split と同じ
    prop "is split when there is no group" $
      forAll genAscii $ \t ->
        splitKeeping (surely "a+") t === split (surely "a+") t
    -- step 3: the named groups as a table, and settings from lines
    -- ステップ3：名前付きのグループを表に。行から設定を読む
    it "makes a table of the named groups" $ do
      fmap namedGroups (find datePattern "2026-11-19")
        `shouldBe` Just (Map.fromList
          [("year", "2026"), ("month", "11"), ("day", "19")])
      fmap namedGroups (find (surely "(?<a>x)|(?<b>y)") "y")
        `shouldBe` Just (Map.fromList [("b", "y")])
      settings "name = Mira\n# a note\nlevel=7\nguild =  archive room\n"
        `shouldBe` Map.fromList
          [("name", "Mira"), ("level", "7"), ("guild", "archive room")]
  -- ANCHOR_END: answers
