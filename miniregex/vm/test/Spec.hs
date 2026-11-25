module Main (main) where

-- ANCHOR: imports
import Control.Exception (SomeException, evaluate, try)
import System.Timeout (timeout)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (GaveUp, Negative, Positive, once)
import Text.Regex.Pcre2 qualified as Pcre2


import MiniRegex
import MiniRegex.Compat
import MiniRegex.Compile (compileTree, listing, programSize)
import MiniRegex.Program (instructions)
import MiniRegex.Engine
  (nonEmptyAt, searchCounting, searchFromCont, searchFromList)
import MiniRegex.Examples
import MiniRegex.Groups (groupCount)
import MiniRegex.Parser
import MiniRegex.Pretty
import MiniRegex.Syntax
import MiniRegex.Types (program)
import MiniRegex.Unicode (caseVariants, fold)
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

-- 🚩 Our flags, as the pcre2 library's options. MiniRegex's \d and \w
--    are Unicode: that is PCRE2's Ucp.
-- MiniRegex のフラグを、pcre2 のライブラリのオプションに。MiniRegex の
-- \d・\w は Unicode なので、PCRE2 の Ucp を付ける
pcre2Options :: [Option] -> Pcre2.Option
pcre2Options opts = Pcre2.Ucp <> foldMap one opts
  where
    one Multiline = Pcre2.Multiline
    one DotAll    = Pcre2.DotAll
    one Caseless  = Pcre2.Caseless
    one _         = mempty

-- 🎲 Characters from blocks that Unicode 16 and 17 agree on (the pcre2
--    library has PCRE2 10.47, of Unicode 16; GHC and PCRE2 10.49 are
--    Unicode 17): letters with and without case, digits of many
--    kinds, spaces, signs, kana and kanji.
-- Unicode 16 と 17 で変わっていない区画の文字（pcre2 のライブラリの
-- 中は PCRE2 10.47 で Unicode 16。GHC と PCRE2 10.49 は Unicode 17）：
-- 大文字と小文字のある文字とない文字、いろいろな数字、空白、記号、
-- かなと漢字
genUnicodeChar :: Gen Char
genUnicodeChar = frequency
  [ (3, chooseEnum (' ', '~'))
  , (2, chooseEnum ('\xA0', '\x24F'))     -- Latin-1, Latin Extended
  , (2, chooseEnum ('\x370', '\x4FF'))    -- Greek, Cyrillic
  , (1, chooseEnum ('\x10A0', '\x10FF'))  -- Georgian
  , (1, chooseEnum ('\x13A0', '\x13F5'))  -- Cherokee
  , (1, chooseEnum ('\x1E00', '\x1FFF'))  -- Latin and Greek extended
  -- (but U+20C1, the Saudi riyal sign, is new in Unicode 17)
  -- （ただし U+20C1 のサウジ・リヤルの記号は、Unicode 17 で足された）
  , (1, chooseEnum ('\x2000', '\x215F') `suchThat` (/= '\x20C1'))
  , (1, chooseEnum ('\x3000', '\x30FF'))  -- CJK signs, kana
  , (1, chooseEnum ('\x4E00', '\x4E80'))  -- kanji
  , (1, chooseEnum ('\xFF00', '\xFF5E'))  -- full width
  , (1, elements "\x1D7CE\x1D7CF\x10400\x10428\x212A\x17F\x130\x131")
  ]

-- 🎲 Short texts of letters with other cases, digits, kana and \n.
-- ほかの形のある文字、数字、かな、改行の短い文字列
genUnicodeLines :: Gen Text
genUnicodeLines = T.pack <$> listOf (elements "aAbBkK\x212Asſé É1１日の \n_")

-- 🧺 The classes to try one character on.
-- 1文字ずつ試す、文字の集まり
unicodeClasses :: [Text]
unicodeClasses =
  [ "\\d", "\\w", "\\s", "\\h", "\\v", "\\D", "\\W"
  , "[[:alpha:]]", "[[:alnum:]]", "[[:punct:]]", "[[:graph:]]"
  , "[[:print:]]", "[[:xdigit:]]", "[[:space:]]", "[[:word:]]"
  , "[[:cntrl:]]", "[[:upper:]]", "[[:lower:]]", "[[:blank:]]"
  , "\\p{L}", "\\p{Lu}", "\\p{Lc}", "\\p{N}", "\\p{P}", "\\p{S}"
  , "\\p{Zs}", "\\p{Han}", "\\p{Greek}", "\\p{sc:Latin}"
  , "\\p{Hiragana}", "\\p{Katakana}", "\\p{Common}"
  , "\\p{Xan}", "\\p{Xwd}", "\\p{Xps}", "\\p{Xsp}"
  , "(?i)\\p{Lu}", "(?i)[[:upper:]]", "(?a)\\w", "(?aP)[[:alpha:]]" ]

-- ✍️ One character as a pattern: \x{...}.
-- 1文字をパターンに：\x{...}
charPattern :: Char -> Text
charPattern c = "\\x{" <> T.pack (showHex (fromEnum c) "") <> "}"

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
agreeWithin :: (Eq a, Show a) => a -> a -> Test.QuickCheck.Property
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
      , (1, elements "dQExcnyiKbR")
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
      [ (1, Anchor <$> elements
              [LineStart, LineEnd, TextStart, TextEnd, LastLineEnd])
      -- \b and \B, and \K
      -- \b と \B、\K
      , (1, Anchor <$> (elements [WordBoundary, NotWordBoundary]
                          <*> elements [AsciiOnly, AllUnicode]))
      , (1, pure KeepOut)
      , (6, Quantified <$> genAtom <*> genQuantifier)
      ]
    genAtom = frequency $
      [ (6, Lit <$> genChar)
      , (1, pure AnyChar)
      , (1, pure Anything)
      , (2, Class <$> genClass)
      -- (?i): only what the parser would wrap
      -- (?i)：パーサーが包むものだけ
      , (2, NoCase . Lit <$> elements "abkKsé")
      , (1, NoCase . Class <$> genClass)
      ] ++
      [ (2, Group <$> elements [Numbered, NonCapturing, BranchReset]
                  <*> genTree greeds (depth - 1))
      | depth > 0 ] ++
      -- atomic groups and looks
      -- アトミックグループと「見る」もの
      [ (1, Atomic <$> genTree greeds (depth - 1))
      | depth > 0 ] ++
      [ (2, Look <$> elements [Ahead, Behind]
                 <*> elements [Positive, Negative]
                 <*> genTree greeds (depth - 1))
      | depth > 0 ] ++
      [ (1, Backref <$> chooseInt (1, 3))
      , (1, NoCase . Backref <$> chooseInt (1, 3)) ]
    -- one reach for a whole class: (?aD) cannot be on and off at once
    -- 集まり1つに、届く範囲は1つ：(?aD) を付けて、同時に外すことはできない
    genClass = do
      neg <- arbitrary
      reach <- elements [AsciiOnly, AllUnicode]
      first <- genItem reach
      rest <- resize 3 (listOf (genItem reach))
      pure (CharClass neg (first :| rest))
    genItem reach = frequency
      [ (3, Single <$> genChar)
      , (2, do a <- genChar
               b <- genChar
               pure (Range (min a b) (max a b)))
      , (2, Builtin <$> arbitrary <*> genType reach)
      ]
    -- (no [:upper:], [:lower:], \p{Lu}: (?i) turns them into others)
    -- （[:upper:]・[:lower:]・\p{Lu} は入れない。(?i) がほかのものにする）
    genType reach =
      elements
        [ Digit reach, WhiteSpace reach, Word reach, HSpace, VSpace
        , Posix reach "alpha", Posix reach "digit", Posix reach "punct"
        , Property (Category "L"), Property (Category "Nd")
        , Property (Category "Lc"), Property AnyCode ]
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
    goA (Atomic p)  = Atomic (goP p)
    goA (Look d s p) = Look d s (goP p)
    goA (Backref n)
      | count == 0 = Lit 'a'
      | otherwise  = Backref ((n - 1) `mod` count + 1)
    goA (NoCase (Backref n))
      | count == 0 = Lit 'a'
      | otherwise  = NoCase (Backref ((n - 1) `mod` count + 1))
    goA a = a

-- ANCHOR: fixlooks
-- 👀 Make the looks right: no \K in a look, and a lookbehind with a
--    limit (no *, + or {n,} in it) and no back reference.
-- 「見る」ものを正しくする：中に \K を置かない。後読みには上限を付け
-- （* や + や {n,} を置かない）、後方参照を置かない
fixLooks :: Pattern -> Pattern
fixLooks = goP False False
  where
    goP look behind (Pattern bs) = Pattern (fmap (goB look behind) bs)
    goB look behind (Branch ps) =
      Branch [ goPiece look behind p | p <- ps, not (look && p == KeepOut) ]
    goPiece look behind (Quantified a q) =
      Quantified (goA look behind a) (if behind then limited q else q)
    goPiece _ _ other = other
    goA look behind a = case a of
      Group c p      -> Group c (goP look behind p)
      Atomic p       -> Atomic (goP look behind p)
      Look Behind s p -> Look Behind s (goP True True p)
      Look d s p     -> Look d s (goP True behind p)
      Backref _ | behind -> Lit 'a'
      NoCase (Backref _) | behind -> Lit 'a'
      _              -> a
    limited (Quantifier (Count n Nothing) g) = Quantifier (Count n (Just 2)) g
    limited q = q
-- ANCHOR_END: fixlooks

-- 🌳 Random trees whose references and looks are all right.
-- 参照と「見る」ものが、どれも正しい、でたらめな木
genTreeRefs :: [Greed] -> Int -> Gen Pattern
genTreeRefs greeds depth = fixLooks . fixRefs <$> genTree greeds depth

-- 🌳 The same, without a group repeated {0}: PCRE2 calls (a|^){0}
--    "anchored" and looks only at the start (pcre2test's info says
--    so), which MiniRegex does not copy.
-- 同じもの。ただし {0} でくり返すグループはなし。PCRE2 は (a|^){0} を
-- 「頭に固定」とみなして、頭しか見ない（pcre2test の info で見える）。
-- MiniRegex は、それをまねしない
genTreeForPcre2 :: Gen Pattern
genTreeForPcre2 =
  genTreeRefs [Greedy, Lazy, Possessive] 2 `suchThat` noZeroGroup
  where
    noZeroGroup (Pattern bs) = all branchOk bs
    branchOk (Branch ps) = all pieceOk ps
    pieceOk (Quantified (Group _ p) q) =
      count q /= Count 0 (Just 0) && noZeroGroup p
    pieceOk _ = True

-- 🐛 A range that reaches U+3000 or further, with (?i) somewhere: the
--    pcre2 library's PCRE2 10.47 gets those wrong.
-- U+3000 より先まで届く範囲と、どこかの (?i)：pcre2 のライブラリの中の
-- PCRE2 10.47 は、これをまちがえる
wideRangeNoCase :: [Option] -> Pattern -> Bool
wideRangeNoCase opts tr =
  any wide (rangesOf tr) && (Caseless `elem` opts || hasNoCase tr)
  where
    wide (_, hi) = hi >= '\x3000'
    rangesOf (Pattern bs) = concatMap branchRanges (NE.toList bs)
    branchRanges (Branch ps) = concatMap pieceRanges ps
    pieceRanges (Quantified a _) = atomRanges a
    pieceRanges _                = []
    atomRanges (Class c)    = [(a, b) | Range a b <- NE.toList (items c)]
    atomRanges (NoCase a)   = atomRanges a
    atomRanges (Group _ p)  = rangesOf p
    atomRanges (Atomic p)   = rangesOf p
    atomRanges (Look _ _ p) = rangesOf p
    atomRanges _            = []
    hasNoCase (Pattern bs) = any (\(Branch ps) -> any pieceNoCase ps) bs
    pieceNoCase (Quantified a _) = atomNoCase a
    pieceNoCase _                = False
    atomNoCase (NoCase _)  = True
    atomNoCase (Group _ p) = hasNoCase p
    atomNoCase (Atomic p)   = hasNoCase p
    atomNoCase (Look _ _ p) = hasNoCase p
    atomNoCase _           = False

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
    -- the matcher runs *, the flag i, and a*+ too: nothing waits
    -- マッチャーは * も、フラグ i も、a*+ も動かせる。待つものはない
    it "stops nothing at the door" $ do
      fmap source (compile "ab*c") `shouldBe` Right "ab*c"
      fmap source (compile "a(?:b*+)c") `shouldBe` Right "a(?:b*+)c"
      fmap source (compileWith [Caseless] "a") `shouldBe` Right "a"
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
    -- (and since the chapter on looking around, not the room number)
    -- （まわりを見る章から、部屋の番号は拾わない）
    it "finds dates (and not the room number)" $
      findDates records `shouldBe`
        Right ["2026-11-19", "2026-11-20", "2026-11-21"]
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
      decodeSubject "\\o{11064}\\N{U+1234}" `shouldBe` Just "\x1234\x1234"
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
      let plain = Modifiers [] False False False False
          withAfter = Modifiers [] False True False False
          utf = Modifiers [] False False True False
      showFound plain "" (findAll (surely "a\tb\xe9") "a\tb\xe9")
        `shouldBe` [" 0: a\\x09b\\xe9"]
      showFound plain "" (findAll (surely "(a)(x)?(c)?") "ab")
        `shouldBe` [" 0: a", " 1: a"]
      showFound plain "" (findAll (surely "(a)(x)?(b)") "ab")
        `shouldBe` [" 0: ab", " 1: a", " 2: <unset>", " 3: b"]
      showFound withAfter "xaybz" (findAll (surely "(a)") "xaybz")
        `shouldBe` [" 0: a", " 0+ ybz", " 1: a"]
      showFound plain "" [] `shouldBe` ["No match"]
      -- with utf, as \x{...}
      -- utf なら \x{...} で
      showFound utf "" (findAll (surely "\\w+") "日\x7fé")
        `shouldBe` [" 0: \\x{65e5}", " 0: \\x{e9}"]
    -- utf, ucp and xx are known now; without ucp, the flag Ascii
    -- utf・ucp・xx も、わかるようになった。ucp がなければ、フラグ Ascii
    it "reads the modifiers" $ do
      flagsOf "g,aftertext,im"
        `shouldBe` Right (Modifiers [Caseless, Multiline, Ascii]
                                    True True False False)
      flagsOf "utf,ucp" `shouldBe` Right (Modifiers [] False False True False)
      flagsOf "xx" `shouldBe` Right (Modifiers [Ascii] False False False True)
      flagsOf "mark" `shouldBe` Left "mark"
    -- the whole of testoutput1: no wrong answers, and the parser
    -- calls no right pattern wrong
    -- testoutput1 の全部：ちがう答えは1つもなく、パーサーは正しい
    -- パターンを「まちがい」と言わない
    it "reads the patterns of 1756 tests, passes 1751, and fails none" $ do
      tests <- readTestFile Latin1 "testdata/testoutput1"
      tally (map readable tests) `shouldBe` Tally 1756 0 892
      let verdicts = map judge tests
      tally verdicts `shouldBe` Tally 1751 0 897
    -- the second book, with UTF: no wrong answers either
    -- 2冊目（UTF あり）も、ちがう答えは1つもない
    it "passes 1181 tests of testoutput4, and fails none" $ do
      tests <- readTestFile Utf8 "testdata/testoutput4"
      tally (map readable tests) `shouldBe` Tally 1181 0 352
      let verdicts = map judge tests
      tally verdicts `shouldBe` Tally 1181 0 352
    -- every line counts now: the groups', too
    -- いまは全部の行をくらべる。グループの行も
    it "compares the groups' lines too" $ do
      judge (TestCase 1 "(a)b" "" "ab" [" 0: ab", " 1: a"]) `shouldBe` Pass
      judge (TestCase 1 "(a)b" "" "ab" [" 0: ab", " 1: b"]) `shouldBe` Fail
      judge (TestCase 1 "a*" "g" "ab" [" 0: a", " 0: ", " 0: "])
        `shouldBe` Pass
    -- a plain i in the pattern is not the flag i (only (?i) is)
    -- パターンの中の、ただの文字の i は、フラグの i ではない（(?i) だけ）
    it "takes only (?i) as caseless, not a letter i" $ do
      judge (TestCase 1 "i" "" "\\xe9i" [" 0: i"]) `shouldBe` Pass
      judge (TestCase 1 "i" "" "\\xe9i" ["No match"]) `shouldBe` Fail
      judge (TestCase 1 "(?i)e" "" "\\xe9" ["No match"])
        `shouldBe` NotYet "caseless without UTF"
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
    prop "the list and the continuations agree" $
      -- (short texts: a backtracking matcher can take very long on
      --  a long text that does not match)
      -- （短い文字列で。バックトラックのマッチャーは、合わない長い
      --   文字列に、とても長くかかることがある）
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (sublistOf [Multiline, DotAll]) $ \opts ->
          case compileWith opts (render tr) of
            Left e   -> counterexample (show e) False
            Right re ->
              agreeWithin (searchFromList re t 0) (searchFromCont re t 0)
  -- ANCHOR_END: matcher

  -- ANCHOR: vm
  describe "the VM / 仮想機械" $ do
    -- the program the chapter shows
    -- 章が見せるプログラム
    it "compiles colou?r|gr(a|e)y" $
      listing (program (surely "colou?r|gr(a|e)y")) `shouldBe` T.unlines
        [ "  0  split 1, 9", "  1  take c", "  2  take o", "  3  take l"
        , "  4  take o", "  5  split 6, 7", "  6  take u", "  7  take r"
        , "  8  jmp 18", "  9  take g", " 10  take r", " 11  open 1"
        , " 12  split 13, 15", " 13  take a", " 14  jmp 16", " 15  take e"
        , " 16  close 1", " 17  take y", " 18  match" ]
    -- ANCHOR: threeway
    -- the list, the continuations and the VM: the same answer, groups
    -- and \K too, with the flags m, s and i
    -- 候補のリスト、継続、VM：同じ答え。グループと \K も、フラグ m・s・i も
    prop "the three matchers agree" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (sublistOf [Multiline, DotAll, Caseless]) $ \opts ->
          case compileWith opts (render tr) of
            Left e   -> counterexample (show e) False
            Right re ->
              let vm = find re t
              in  agreeWithin (vm, vm)
                    (searchFromCont re t 0, searchFromList re t 0)
    -- ANCHOR_END: threeway
    -- ANCHOR: limits
    let field n = T.unwords (take n (cycle names5)) <> "!"
        names5 = ["Mira", "Ruby", "Lam", "GHC", "Perl"]
        names = surely "^(\\w+\\s?)+$"
    -- PCRE2 10.49 says "match limit exceeded" to the same, with 8 names
    -- PCRE2 10.49 も、名前が8つで「match limit exceeded」と言う
    it "gives up on ^(\\w+\\s?)+$ and 8 names" $ do
      findWithin defaultLimit names (field 8)
        `shouldBe` Left (GaveUp defaultLimit)
      evaluate (find names (field 8)) `shouldThrow` (== GaveUp defaultLimit)
      evaluate (length (findAll names (field 8)))
        `shouldThrow` (== GaveUp defaultLimit)
    it "answers when there are few names, or no !" $ do
      findWithin defaultLimit names (field 4) `shouldBe` Right Nothing
      fmap (fmap matchEnd) (findWithin defaultLimit names (T.init (field 30)))
        `shouldBe` Right (Just 137)
    it "gives up on (a+)+b, but not on (?>a+)+b" $ do
      let as = T.replicate 30 "a" <> "c"
      isLeft (findWithin 100000 (surely "(a+)+b") as) `shouldBe` True
      findWithin 100000 (surely "(?>a+)+b") as `shouldBe` Right Nothing
      findAllWithin 100000 (surely "(a+)+b") as
        `shouldBe` Left (GaveUp 100000)
    -- one more a, twice the steps
    -- a が1つ増えると、ステップ数は2倍
    it "takes twice the steps for one more a" $
      [ stepsOf "(a+)+b" (T.replicate n "a" <> "c") | n <- [10 .. 13] ]
        `shouldBe` [24480, 49048, 98192, 196488]
    -- the steps used are never more than the limit, and with enough of
    -- them, the answer is find's
    -- 使ったステップ数は上限以下。ステップ数が足りれば、答えは find と同じ
    prop "keeps to the limit" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (chooseInt (0, 300)) $ \limit ->
          let re = surely (render tr)
          in  case searchCounting limit re t 0 of
                (Left gaveUp, used) ->
                  label "gave up" $
                    gaveUp === GaveUp limit .&&. used === limit
                (Right m, used) ->
                  label "answered" $
                    m === find re t .&&. property (used <= limit)
    -- ANCHOR_END: limits
    -- a long text: no stack of Haskell's grows with it
    -- 長い文字列：Haskell のスタックが、それにつれて伸びることはない
    -- a program too big to make is "not yet", before it is made (PCRE2
    -- says error 120 to this one); a big one that fits is made
    -- 作るには大きすぎるプログラムは、作る前に「まだ」（これには PCRE2 も
    -- エラー 120）。収まる大きなものは作る
    it "says not yet to a program too big to make" $ do
      fmap source (compile "(?:a{65535}){65535}")
        `shouldBe` Left (Unsupported 0 "pattern too large for the VM")
      fmap (`isMatch` "b") (compile "(?:a{100}){1000}") `shouldBe` Right False
    -- the count before making is never less than the real one
    -- 作る前の見積もりは、本物より少なくならない
    prop "counts no fewer instructions than the program has" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        programSize tr
          >= fromIntegral (length (instructions (compileTree tr)))
    it "goes through 100000 characters" $ do
      let t = T.replicate 50000 "ab" <> "c"
      fmap matchEnd (find (surely "(?:a|b)*c") t) `shouldBe` Just 100001
    -- the window of the story
    -- 物語の窓口
    it "checks the name field" $ do
      checkName "Mira Ruby" `shouldBe` Right True
      checkName "Mira Ruby!" `shouldBe` Right False
      checkName nameField `shouldBe` Left (GaveUp windowLimit)
  -- ANCHOR_END: vm

  -- ANCHOR: groups
  describe "groups / グループ" $ do
    -- each answer was checked with pcre2test 10.49
    -- 答えは、どれも pcre2test 10.49 で確かめた
    -- both ways: the machine (find) and the list
    -- 両方で：機械（find）と、候補のリスト
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
    -- (since the chapter on looking around, whole words: \b)
    -- （まわりを見る章から、言葉まるごと：\b）
    it "finds the words written twice" $
      doubledWords notes `shouldBe` ["the", "near"]
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
    wrong "b(?P"        4 "missing closing parenthesis"              -- 114
    wrong "(?P)"        4 "unrecognized character after (?P"         -- 141
    wrong "(?Px)"       4 "unrecognized character after (?P"
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

  -- ANCHOR: unicode
  describe "Unicode and flags / Unicode とフラグ" $ do
    -- each answer was checked with pcre2test 10.49 (utf, and ucp unless
    -- Ascii)
    -- 答えは、どれも pcre2test 10.49 で確かめた（utf と、Ascii でなければ
    -- ucp）
    let cases =
          [ ("\\d+", [], "記録 ２０２６年と2026年", (Just "２０２６"))
          , ("\\d+", [Ascii], "記録 ２０２６年と2026年", (Just "2026"))
          , ("\\w+", [], "名前:ラムダ_1", (Just "名前"))
          , ("\\w+", [Ascii], "名前:ラムダ_1", (Just "_1"))
          , ("\\s+", [], "a\x3000\&\xa0\&b", (Just "\x3000\&\xa0\&"))
          , ("\\s+", [Ascii], "a\x3000\&\xa0\&\tb", (Just "\t"))
          , ("\\h+", [Ascii], "x\x3000\&\xa0\&y", (Just "\x3000\&\xa0\&"))
          , ("\\v", [], "a\x2028\&b", (Just "\x2028\&"))
          , ("\\D+", [], "２０ab", (Just "ab"))
          , ("\\W+", [], "名前 !?x", (Just " !?"))
          , ("(?a)\\d+", [], "２０２６と2026", (Just "2026"))
          , ("(?aD)\\d+\\w+", [], "２０２６と2026x", (Just "2026x"))
          , ("(*UCP)\\d+", [Ascii], "２０２６", (Just "２０２６"))
          , ("\\N+", [DotAll], "ab\ncd", (Just "ab"))
          , ("\\N{U+263a}", [], "x☺y", (Just "☺"))
          , ("[[:alpha:]]+", [], "123élan", (Just "élan"))
          , ("[[:alpha:]]+", [Ascii], "123élan", (Just "lan"))
          , ("[[:digit:][:upper:]]+", [], "abCD12x", (Just "CD12"))
          , ("[[:^digit:]]+", [], "12ab34", (Just "ab"))
          , ("[[:punct:]]+", [], "a!?、。§b", (Just "!?、。§"))
          , ("[[:punct:]]+", [], "a©¬b", Nothing)
          , ("[[:xdigit:]]+", [], "xＡ０fg", (Just "Ａ０f"))
          , ("[[:space:]]+", [], "a\x3000\&\x85\&b", (Just "\x3000\&\x85\&"))
          , ("\\p{Han}+", [], "記録室のノート", (Just "記録室"))
          , ("\\p{Hiragana}+", [], "記録室のノート", (Just "の"))
          , ("\\p{Katakana}+", [], "記録室のノート", (Just "ノート"))
          , ("\\p{sc:Katakana}+", [], "記録室のノート", (Just "ノ"))
          , ("\\p{Lu}+", [], "abCDe", (Just "CD"))
          , ("\\p{L}+", [], "123Ωmega!", (Just "Ωmega"))
          , ("\\pN+", [], "x①②3y", (Just "①②3"))
          , ("\\P{L}+", [], "ab12!c", (Just "12!"))
          , ("\\p{^L}+", [], "ab12!c", (Just "12!"))
          , ("\\p{ sc = greek }+", [], "abΩμc", (Just "Ωμ"))
          , ("[\\p{Han}\\p{Hira}]+", [], "ノート記録のx", (Just "ー"))
          , ("\\p{Any}", [DotAll], "\n", (Just "\n"))
          , ("\\p{Common}", [], "〉", (Just "〉"))
          , ("\\p{Xuc}+", [], "a$@`\xa0\&b", (Just "$@`\xa0\&"))
          , ("\\p{Qaai}\\p{qaac}", [], "a\x301\&\x2C80\&",
             (Just "\x301\&\x2C80\&"))
          , ("[[:graph:]]", [], "\x61C\&\x180E\&\x2066\&", Nothing)
          , ("[[:print:]]", [], "\x61C\&\x180E\&\x2066\&", (Just "\x180E\&"))
          , ("dragon", [Caseless], "a DRAGON", (Just "DRAGON"))
          , ("é", [Caseless], "CAFÉ", (Just "É"))
          , ("k", [Caseless], "K", (Just "K"))
          , ("K", [Caseless], "k", (Just "k"))
          , ("s", [Caseless], "ſ", (Just "ſ"))
          , ("ß", [Caseless], "ẞ", (Just "ẞ"))
          , ("ß", [Caseless], "SS", Nothing)
          , ("i", [Caseless], "İ", Nothing)
          , ("ı", [Caseless], "I", Nothing)
          , ("σ+", [Caseless], "Σςσ", (Just "Σςσ"))
          , ("[a-z]+", [Caseless], "MiniRegex", (Just "MiniRegex"))
          , ("[^k]", [Caseless], "K", Nothing)
          , ("[k-m]+", [Caseless], "KLM", (Just "KLM"))
          , ("\\p{Lu}+", [Caseless], "abC", (Just "abC"))
          , ("[[:upper:]]+", [Caseless], "abC", (Just "C"))
          , ("[[:upper:]]+", [Caseless, Ascii], "abC", (Just "abC"))
          , ("(?i)(é\n)\\1^b", [Multiline], "é\nÉ\nb", (Just "é\nÉ\nb"))
          , ("(a)\\1", [Caseless], "aA", (Just "aA"))
          , ("(\\w+) \\1", [Caseless], "Lam LAM", (Just "Lam LAM"))
          , ("(?i)lam", [], "LAM", (Just "LAM"))
          , ("a(?i)b", [], "aB", (Just "aB"))
          , ("a(?i)b", [], "AB", Nothing)
          , ("(a(?i)b|c)", [], "C", (Just "C"))
          , ("(?i:a)b", [], "AB", Nothing)
          , ("(?i:a)b", [], "Ab", (Just "Ab"))
          , ("(?i)a(?-i)b", [], "AB", Nothing)
          , ("(?i)a(?^)b", [], "Ab", (Just "Ab"))
          , ("(?a)(?^)\\d+", [], "２2", (Just "2"))
          , ("(?U)(?^)a+", [], "aaa", (Just "a"))
          , ("(?m)^b", [], "a\nb", (Just "b"))
          , ("^b", [], "a\nb", Nothing)
          , ("(?s).+", [], "a\nb", (Just "a\nb"))
          , (".+", [], "a\nb", (Just "a"))
          , ("(?s:.)\\N", [], "\nab", (Just "\na"))
          , ("(?x) a b c # comment", [], "abc", (Just "abc"))
          , ("(?x) a \\  b", [], "a b", (Just "a b"))
          , ("(?x)[ ]", [], "a b", (Just " "))
          , ("(?xx)[a b]+", [], "a b", (Just "a"))
          , ("\\d+(?#year)-\\d+", [Extended], "2026 - 11", Nothing)
          , ("(?n)(a)(?<n>b)", [], "ab", (Just "ab"))
          , ("(?U)a+", [], "aaa", (Just "a"))
          , ("(?U)a+?", [], "aaa", (Just "aaa"))
          , ("(?|(a)|(b)(c))(d)", [], "bcd", (Just "bcd"))
          , ("(?|(\\d+)年|(\\d+)/)", [], "11/19", (Just "11/"))
          , ("\\Aab", [Multiline], "x\nab", Nothing)
          , ("ab\\z", [], "ab\n", Nothing)
          , ("ab\\Z", [], "ab\n", (Just "ab"))
          , ("ab$", [], "ab\n", (Just "ab"))
          , ("[\\w-]+", [], "a-b c", (Just "a-b"))
          ]
    -- both matchers: the continuations (find) and the list
    -- 2つのマッチャーの両方：継続（find）と、候補のリスト
    mapM_ (\(p, opts, t, answer) ->
             it (show (p, opts, t)) $ do
               let re = surelyWith opts p
               fmap matchText (find re t) `shouldBe` answer
               fmap matchText (searchFromList re t 0) `shouldBe` answer)
      cases
    -- (?| ... ): the branches share numbers, and the group after it
    -- is numbered after the branch with the most groups
    -- (?| ... )：枝どうしで番号を分け合い、うしろのグループは、
    -- グループのいちばん多い枝の次の番号
    let resets p t answer = it (show (p, t)) $ do
          fmap groups (find (surely p) t) `shouldBe` answer
          fmap groups (searchFromList (surely p) t 0) `shouldBe` answer
    resets "(?|(a)|(b)(c))(d)" "bcd"
      (Just [Just "b", Just "c", Just "d"])
    resets "(?|(a)|(b)(c))(d)" "ad" (Just [Just "a", Nothing, Just "d"])
    resets "(?|(a)|(b)(c))\\g{-1}" "bcc" (Just [Just "b", Just "c"])
    resets "(?|(a)|(b)(c))\\g{-1}" "aa" Nothing
    it "counts the groups of (?| ... ) by its largest branch" $
      fmap groupCount (parsePattern [] "(?|(a)|(b)(c))(d)") `shouldBe` Right 3
    it "takes the month in three ways" $
      map monthOf ["11月19日", "2026-11-19", "11/19"]
        `shouldBe` [Just "11", Just "11", Just "11"]
    -- the few characters whose fold is not lower(upper(c))
    -- 代表が「大文字の小文字」でない、いくつかの文字
    it "folds like CaseFolding.txt" $ do
      map fold "aAK\x212Aſ\x130\x131\x1FD3\xFB06"
        `shouldBe` "aakks\x130\x131\x390\xFB05"
      caseVariants 'k' `shouldBe` "Kk\x212A"
      caseVariants '日' `shouldBe` "日"
    -- places in bytes: 名 and 前 are three bytes each in UTF-8
    -- バイトで数えた場所：名と前は、UTF-8 でそれぞれ3バイト
    it "gives places in bytes" $ do
      let t = "名前: Lam"
          m = maybe (error "no match") id (find (surely "Lam") t)
      (matchStart m, matchEnd m) `shouldBe` (4, 7)
      byteSpan t (matchSpan m) `shouldBe` (8, 11)
      byteOffset t 2 `shouldBe` 6
      -- the end in bytes, too: 前 is three bytes
      -- 終わりもバイトで：前は3バイト
      let spanOf p = fmap (byteSpan t . matchSpan) (find (surely p) t)
      spanOf "名前" `shouldBe` Just (0, 6)
      spanOf "前: L" `shouldBe` Just (3, 9)
    -- names of groups, and in templates, may be Unicode letters
    -- グループの名前とテンプレートの名前に、Unicode の文字を使える
    it "takes ${年} in a template" $ do
      let re = surely "(?<年>\\d+)年"
      fmap (\x -> replaceAllTemplate re x "2026年") (template re "<${年}>")
        `shouldBe` Right "<2026>"
      -- Ⅻ is a number, but not a decimal digit: not in a name
      -- Ⅻ は数字の仲間だが、10進の数字ではない：名前に入らない
      fmap (const ()) (template (surely "(a)") "<${Ⅻ}>")
        `shouldBe` Left (Invalid 3 "invalid replacement string")
    -- the flags are in the tree: (?i) changes the atoms after it
    -- フラグは木の中にある。(?i) は、うしろのアトムを変える
    it "writes the flags into the tree" $
      parsePattern [] "a(?i)b."
        `shouldBe` Right (Pattern (Branch
          [ Quantified (Lit 'a') once
          , Quantified (NoCase (Lit 'b')) once
          , Quantified AnyChar once ] :| []))
    -- ANCHOR: unicodeprops
    -- one character, against the real PCRE2 with Ucp: \d, \w ...
    -- 1文字ずつ、Ucp を付けた本物の PCRE2 とくらべる：\d・\w など
    prop "agrees with PCRE2 on the classes of one character" $
      forAll genUnicodeChar $ \c ->
        forAll (elements unicodeClasses) $ \p ->
          counterexample (T.unpack p) $
            isMatch (surely p) (T.singleton c)
              === Pcre2.matchesOpt Pcre2.Ucp p (T.singleton c)
    -- and the same, but for case: (?i) with one character
    -- 大文字と小文字のちがいだけ：(?i) と1文字
    prop "agrees with PCRE2 on which characters are the same but case" $
      forAll genUnicodeChar $ \c ->
        forAll (oneof [elements (caseVariants c), genUnicodeChar]) $ \d ->
          let p = charPattern c
          in  counterexample (T.unpack p) $
                isMatch (surelyWith [Caseless] p) (T.singleton d)
                  === Pcre2.matchesOpt (Pcre2.Ucp <> Pcre2.Caseless) p
                        (T.singleton d)
    -- ANCHOR_END: unicodeprops
  -- ANCHOR_END: unicode

  -- ANCHOR: unicodeerrors
  describe "wrong patterns of this chapter, like PCRE2 10.49 / まちがい" $ do
    -- each place and message was checked with pcre2test 10.49
    -- 場所と文は、どれも pcre2test 10.49 で確かめた
    let wrong p at m = it (T.unpack p) $
          parsePattern [] p `shouldBe` Left (Invalid at m)
    wrong "a(?z)b"        4 "unrecognized character after (? or (?-"  -- 111
    wrong "a(?i)*"        6
      "quantifier does not follow a repeatable item"  -- 109
    wrong "(?i"           3 "missing closing parenthesis"  -- 114
    wrong "(?i:a"         5 "missing closing parenthesis"  -- 114
    wrong "(?"            2 "missing closing parenthesis"  -- 114
    wrong "\\p{Letter}"  10 "unknown property after \\P or \\p"  -- 147
    wrong "\\p{L"         4 "malformed \\P or \\p sequence"  -- 146
    wrong "\\p"           2 "malformed \\P or \\p sequence"  -- 146
    wrong "\\px"          3 "unknown property after \\P or \\p"  -- 147
    -- PCRE2 stops at a character that cannot be in a name
    -- 名前に使えない文字のところで、PCRE2 は止まる
    wrong "\\p{|}"        4 "malformed \\P or \\p sequence"  -- 146
    wrong "^\\p{P|}"      6 "malformed \\P or \\p sequence"  -- 146
    wrong "\\P{#a"        4 "malformed \\P or \\p sequence"  -- 146
    wrong "\\N{5,2}"      2 "numbers out of order in {} quantifier"  -- 104
    wrong "[[:foo:]]"     8 "unknown POSIX class name"  -- 130
    wrong "[\\d-z]"       4 "invalid range in character class"  -- 150
    wrong "[a-\\d]"       5 "invalid range in character class"  -- 150
    wrong "[\\N]"         3 "\\N is not supported in a class"  -- 171
    wrong "[\\R]"         3
      "escape sequence is invalid in character class"  -- 107
    wrong "\\N{abc}"      3
      "PCRE2 does not support \\F, \\L, \\l, \\N{name}, \\U, or \\u"  -- 137
    wrong "(?i-m-s)a"     6 "invalid hyphen in option setting"  -- 194
    wrong "(?^-i)a"       4 "invalid hyphen in option setting"  -- 194
    wrong "\\N{U+}"       5
      "digits missing after \\x or in \\x{} or \\o{} or \\N{U+}"  -- 178
  -- ANCHOR_END: unicodeerrors

  -- ANCHOR: looking
  describe "looking around / まわりを見る" $ do
    -- each answer was checked with pcre2test 10.49 (pcre2/*.txt, and
    -- more)
    -- 答えは、どれも pcre2test 10.49 で確かめた（pcre2/*.txt など）
    -- first: both matchers, the continuations (find) and the list
    -- first：2つのマッチャーの両方。継続（find）と、候補のリスト
    let all' p t answer = it (show (p, t)) $
          map matchText (findAll (surely p) t) `shouldBe` answer
        first p t answer = it (show (p, t)) $ do
          fmap allGroups (find (surely p) t) `shouldBe` answer
          fmap allGroups (searchFromList (surely p) t 0) `shouldBe` answer
        places p t = map (\m -> (matchStart m, matchEnd m))
                         (findAll (surely p) t)
    -- \b and \B
    -- \b と \B
    all' "\\b[0-9]{4}-[0-9]{2}-[0-9]{2}\\b" "Room 1234-56-789 is closed" []
    all' "\\b([a-z]+) \\1\\b" notes ["the the", "near near"]
    all' "\\bcafé\\b" "a café." ["café"]
    all' "(?aW)\\bcafé\\b" "a café." []
    it "finds \\b and \\B where PCRE2 does" $ do
      map matchStart (findAll (surely "\\b") "ab cd") `shouldBe` [0, 2, 3, 5]
      map matchStart (findAll (surely "\\B") "ab cd") `shouldBe` [1, 4]
    -- lookahead
    -- 先読み
    all' "\\d+(?= gold)" "for 300 gold, a herb for 5 silver" ["300"]
    all' "\\d+(?! gold)" "300 gold, 5 silver" ["30", "5"]
    first "\\d+(?=(gold))" "300gold" (Just ("300" :| ["gold"]))
    first "(?!(a))b\\1?" "b" (Just ("b" :| [""]))
    first "(?=(a))a\\1?" "aa" (Just ("aa" :| ["a"]))
    -- a look with no upper end goes round once more than it must, at
    -- most (PCRE2 reads {n,} on a look as {n,n+1}): both matchers
    -- 上の端のない「見る」ものは、「必ず」より多くても1回だけ回る（PCRE2 は
    -- 見るものの {n,} を {n,n+1} と読む）。2つのマッチャーの両方で
    let both p t answer = it (show (p, t)) $ do
          fmap allGroups (find (surely p) t) `shouldBe` answer
          fmap allGroups (searchFromList (surely p) t 0) `shouldBe` answer
    both "^(?=(\\1b|a))+\\1$" "ab" (Just ("ab" :| ["ab"]))
    both "(?=(\\1b|a)){2,}" "abb" (Just ("" :| ["abb"]))
    both "(?=(a))+b" "ab" Nothing
    -- what the chapter tells, kept as examples (each answer checked
    -- with pcre2test 10.49)
    -- 章が話すことを、例として固めておく（答えは pcre2test 10.49 で確かめた）
    first "(?<=ab?)c" "axc" Nothing                -- ends just here
    first "(?<=(a|aa))b" "aab" (Just ("b" :| ["aa"]))  -- farthest first
    first "a(?!b)" "ab" Nothing
    it "puts a\\K| where PCRE2 does (the night's counterexample)" $
      places "a\\K|" "aaba" `shouldBe` [(1, 1), (2, 2), (4, 4)]
    it "finds every newline with \\R" $
      length (findAll (surely "\\R") "a\vb\fc\x2028\&d\x2029\&e\x85\&f")
        `shouldBe` 5
    it "reads \\B with (?aW) as ASCII" $ do
      map fst (places "(?aW)\\B" "é") `shouldBe` [0, 1]
      places "\\B" "é" `shouldBe` []
    it "checks a word for the vault" $
      map goodPassword ["Lambda2026", "lambda2026", "Lambda-dragon-7", "L2a"]
        `shouldBe` [True, False, False, False]
    -- lookbehind, with a length that can change
    -- 後読み。長さが変わってもよい
    all' "(?<=price:\\s?)\\d+" "price:1200 gold, price: 80 gold, price:   7"
      ["1200", "80"]
    all' "(?<=colour|color)s" "colors colours" ["s", "s"]
    first "(?<=(a|aa))b" "aab" (Just ("b" :| ["aa"]))
    first "(?<=a|(aa))b" "aab" (Just ("b" :| [""]))
    first "(?<=(a{1,3}?))b" "aaab" (Just ("b" :| ["aaa"]))
    all' "(?<=^|,)\\w+" "a,bb,c" ["a", "bb", "c"]
    all' "(?<=(?<=a)b)c" "abc xbc" ["c"]
    all' "(?<=x(?=y))y" "xy" ["y"]
    all' "(?<=\\R)x" "a\r\nx" ["x"]
    -- atomic groups and possessive quantifiers
    -- アトミックグループと所有的な量指定子
    all' "(?>a|ab)c" "abc" []
    all' "(?:a|ab)c" "abc" ["abc"]
    all' "a*+a" "aaa" []
    all' "a++b" "aaab" ["aaab"]
    all' "(?>a+)b" "aaab" ["aaab"]
    it "lists one end for (?>a+)+ and (a++)+" $ do
      endsAtStart "(?>a+)+" "aaaaaac" `shouldBe` 1
      endsAtStart "(a++)+" "aaaaaac" `shouldBe` 1
      endsAtStart "(a+)+" "aaaaaac" `shouldBe` 63
    -- \K
    all' "price:\\s*\\K\\d+" "price:1200 gold, price:   7 gold" ["1200", "7"]
    first "(price):\\K\\d+" "price:1200" (Just ("1200" :| ["price"]))
    it "finds a\\K where PCRE2 does" $ do
      map (\m -> (matchStart m, matchEnd m)) (findAll (surely "a\\K") "aaa")
        `shouldBe` [(1, 1), (2, 2), (3, 3)]
      map matchStart (findAll (surely "(?=a)\\K") "aa") `shouldBe` [0, 1]
      replaceAll (surely "a\\Kb") "-" "abab" `shouldBe` "a-a-"
    -- \R
    it "splits at any newline with \\R" $
      linesR windowsNote `shouldBe` ["Mira", "Ruby", "Lam", "GHC"]
    all' "\\R\\n" "\r\n" []
    all' "(?:\r\n|\n|\r)\n" "\r\n" ["\r\n"]
    -- the examples of the chapter
    -- 章の例
    it "finds the prices of the ledger" $ do
      goldAmounts ledger `shouldBe` ["300", "1200", "80", "7", "1500000"]
      prices ledger `shouldBe` ["1200", "80"]
      pricesK ledger `shouldBe` ["1200", "80", "7"]
      map commas ["7", "300", "1200", "1500000"]
        `shouldBe` ["7", "300", "1,200", "1,500,000"]
    it "leaves the room number alone" $ do
      findDates records
        `shouldBe` Right ["2026-11-19", "2026-11-20", "2026-11-21"]
      doubledWords notes `shouldBe` ["the", "near"]
      drop 3 (T.lines (toDayFirst records))
        `shouldBe` ["Room 1234-56-789 is closed"]
  -- ANCHOR_END: looking

  -- ANCHOR: lookerrors
  describe "wrong looks, like PCRE2 10.49 / まちがった「見る」もの" $ do
    -- each place and message was checked with pcre2test 10.49
    -- 場所と文は、どれも pcre2test 10.49 で確かめた
    let wrong p at m = it (T.unpack p) $
          parsePattern [] p `shouldBe` Left (Invalid at m)
        unlimited = "length of lookbehind assertion is not limited"  -- 125
        tooLong =
          "branch too long in variable-length lookbehind assertion"  -- 200
        bsk = "\\K is not allowed in lookarounds"
                <> " (but see PCRE2_EXTRA_ALLOW_LOOKAROUND_BSK)"  -- 199
    wrong "x(?<=a+)b"              1 unlimited
    wrong "(?<=price:\\s*)\\d+"     0 unlimited
    wrong "(?<=a|b+)"              0 unlimited
    wrong "(?<=a{1,300}|b+)"       0 unlimited
    wrong "x(?<=x(?<=a+))"         6 unlimited
    wrong "xy(?<=a{1,300})b"       2 tooLong
    wrong "(?<=b{1,300})(?<=a+)"   0 tooLong
    -- with a branch whose length changes, a fixed branch too must be
    -- up to 255
    -- 長さが変わる枝があれば、決まった長さの枝も 255 文字まで
    wrong "(?<=a{256}|b?)c"        0 tooLong
    -- (?:)* has no upper end, even taking nothing; a look's * is {0,1}
    -- (?:)* は、何も食べなくても上の端がない。「見る」ものの * は {0,1}
    wrong "(?<=(?:)*b)c"           0 unlimited
    wrong "(?<=()*b)c"             0 unlimited
    wrong "(?=(?>a\\K))"           11 bsk
    wrong "xy(?<=a{65535}b)b"      2
      "lookbehind assertion is too long"  -- 187
    wrong "xyz(?<=\\Ka)bcdefg"    17 bsk
    wrong "xyz(?!a\\K)bcdefg"     16 bsk
    wrong "(?<=a+)\\K(?=\\K)"       0 unlimited
    wrong "(?<=a+)("               8 "missing closing parenthesis"  -- 114
    wrong "x\\b*"                  4
      "quantifier does not follow a repeatable item"  -- 109
    wrong "x\\K+y"                 4
      "quantifier does not follow a repeatable item"  -- 109
    -- PCRE2 takes some back references in a lookbehind; MiniRegex not yet
    -- PCRE2 は、後読みの中の後方参照を受け取ることがある。MiniRegex は、まだ
    -- all fixed: up to 65535, even when the lengths differ
    -- 全部が決まった長さなら 65535 文字まで（長さがちがっても）
    it "takes (?=a)* in a lookbehind (a look's * is {0,1})" $
      fmap matchText (find (surely "(?<=(?=a)*b)c") "bc") `shouldBe` Just "c"
    it "takes long fixed branches when no branch changes" $ do
      fmap matchText (find (surely "(?<=a{256}|b)c") "bc") `shouldBe` Just "c"
      isMatch (surely "(?<=a{256})c") "c" `shouldBe` False
    it "says not yet to a back reference in a lookbehind" $ do
      parsePattern [] "(?<=(a)\\1)b"
        `shouldBe` Left (Unsupported 0 "back reference in lookbehind")
      -- with (?i), and inside a look inside the lookbehind, too
      -- (?i) のもとでも、後読みの中の「見る」ものの中でも
      parsePattern [] "(?<=(a)(?i)\\1)b"
        `shouldBe` Left (Unsupported 0 "back reference in lookbehind")
      parsePattern [] "(?<=(?=(a)\\1)aa)b"
        `shouldBe` Left (Unsupported 0 "back reference in lookbehind")
    -- an atomic group is as long as its inside
    -- アトミックグループの長さは、中の長さ
    it "measures an atomic group in a lookbehind" $
      fmap matchText (find (surely "(?<=(?>ab))c") "abc") `shouldBe` Just "c"
  -- ANCHOR_END: lookerrors

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
        forAll (resize 12 genUnicodeLines) $ \t ->
          forAll (sublistOf [Multiline, DotAll, Caseless]) $ \opts ->
            let p = render tr
            in  counterexample (T.unpack p) $ case compileWith opts p of
                  -- PCRE2 10.47 (in the library) misses σ and K in
                  -- (?i)[$-日]; 10.49 (pcre2test) finds them, as MiniRegex
                  -- ライブラリの中の PCRE2 10.47 は (?i)[$-日] で σ や K を
                  -- 見落とす。10.49（pcre2test）は、MiniRegex と同じく
                  -- 見つける
                  _ | wideRangeNoCase opts tr -> label "PCRE2 10.47 bug" True
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
      parsePattern [] "a\\G" `shouldBe` Left (Unsupported 1 "\\G")
      parsePattern [] "x(?(1)y)" `shouldBe` Left (Unsupported 1 "(?(")
      parsePattern [] "(?J)a" `shouldBe` Left (Unsupported 0 "(?J)")
      parsePattern [] "\\p{Alphabetic}"
        `shouldBe` Left (Unsupported 0 "\\p{binary property}")
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
    -- under (?xx), spaces before the ^ of a class do not hide it
    -- (?xx) のもとでは、文字の集まりの ^ の前の空白が ^ を隠さない
    it "reads [ ^a] under (?xx) as [^a]" $ do
      isMatch (surely "(?xx)[ ^a]") "a" `shouldBe` False
      isMatch (surely "(?xx)[ ^a]") "b" `shouldBe` True
      isMatch (surely "(?xx)[\t^a]") "a" `shouldBe` False
    -- render puts back aT after (?-aP) when it is still needed
    -- render は、(?-aP) のあと、まだ要る aT を付け直す
    it "keeps aT when render takes aP away" $ do
      let p = "(?aP)[[:alpha:]](?-aP)(?aT)[[:alpha:][:digit:]]"
      fmap render (parsePattern [] p) `shouldBe` Right p
      isMatch (surely p) "a\xff11" `shouldBe` False
      isMatch (surely p) "a1" `shouldBe` True
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
    -- step 2: catch GaveUp
    -- ステップ2：GaveUp を受け止める
    it "catches GaveUp" $ do
      findIO wordsOnly nameField >>= (`shouldBe` Left (GaveUp defaultLimit))
      r <- findIO wordsOnly "Mira Ruby"
      fmap (fmap matchText) r `shouldBe` Right (Just "Mira Ruby")
    -- step 3: the smallest limit is the steps that the search takes
    -- ステップ3：いちばん小さい上限は、その探しものが使うステップ数
    it "finds the smallest limit" $ do
      smallestLimit wordsOnly "Mira Ruby!" `shouldBe` Just 1969
      smallestLimit wordsOnly nameField `shouldBe` Nothing
    prop "the smallest limit is the steps taken" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
          let p = render tr
          in  smallestLimit (surely p) t === Just (stepsOf p t)
  -- ANCHOR_END: answers
