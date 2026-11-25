module Main (main) where

-- ANCHOR: imports
import Control.Exception
  (SomeException, evaluate, fromException, throwIO, try)
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
  ( nonEmptyAt, nonEmptyWithinBy, searchCounting, searchCountingBy
  , searchFromCont, searchFromList )
import MiniRegex.Pike (scanPike)
import MiniRegex.Examples
import MiniRegex.Groups (calls, groupCount)
import MiniRegex.Parser
import MiniRegex.Pretty
import MiniRegex.Syntax
import MiniRegex.Types (program)
import MiniRegex.STVM (scanVMST, scanVMSTFresh)
import MiniRegex.Unicode (caseVariants, fold)
import MiniRegex.VM (scanVM)
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
  case answer of
    Nothing -> pure (label "set aside: slow" True)
    Just (Left e)
      | Just (_ :: Pcre2.SomePcre2Exception) <- fromException e ->
          pure (label "set aside: PCRE2 gave up" True)
      -- the VM's own limit on the steps (GaveUp), from this chapter on
      -- VM 自身のステップ数の上限（GaveUp）。命令の列の章から
      | Just (GaveUp _) <- fromException e ->
          pure (label "set aside: MiniRegex gave up" True)
      | otherwise -> throwIO e
    Just (Right same) ->
      pure (counterexample (show ours ++ " /= " ++ show theirs) same)
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

-- ANCHOR: genlinear
-- 🚂 Random trees the Pike VM can run: an atomic group becomes a plain
--    group, a look or a back reference becomes a character, and a
--    possessive repeat a greedy one.
-- Pike VM で動かせる、でたらめな木：アトミックグループはふつうの
-- グループに、「見る」ものと後方参照は文字に、所有的なくり返しは欲ばりに
genTreeLinear :: Gen Pattern
genTreeLinear = goP <$> genTree [Greedy, Lazy] 2
  where
    goP (Pattern bs) = Pattern (fmap goB bs)
    goB (Branch ps)  = Branch (map goPiece ps)
    goPiece (Quantified a (Quantifier c g)) =
      Quantified (goA a) (Quantifier c (if g == Possessive then Greedy else g))
    goPiece other = other
    goA a = case a of
      Group c p          -> Group c (goP p)
      Atomic p           -> Group NonCapturing (goP p)
      Look {}            -> Lit 'b'
      Backref _          -> Lit 'a'
      NoCase (Backref _) -> Lit 'a'
      _                  -> a
-- ANCHOR_END: genlinear

-- 🌳 Random trees whose references and looks are all right.
-- 参照と「見る」ものが、どれも正しい、でたらめな木
genTreeRefs :: [Greed] -> Int -> Gen Pattern
genTreeRefs greeds depth = fixLooks . fixRefs <$> genTree greeds depth

-- ANCHOR: gencalls
-- 📞 Random trees with calls: now and then, a character becomes a call
--    of a group that is there (or of the whole pattern). No call in a
--    lookbehind (MiniRegex says "not yet" to that).
-- 呼び出しのある、でたらめな木：ときどき、文字を、あるグループ（か
-- パターン全体）の呼び出しにする。後読みの中には置かない（MiniRegex は
-- 「まだ」と言う）
genTreeCalls :: Gen Pattern
genTreeCalls = withCalls (genTreeRefs [Greedy, Lazy, Possessive] 2)

withCalls :: Gen Pattern -> Gen Pattern
withCalls trees = do
  tr <- trees
  let count = groupCount tr
      goP behind (Pattern bs) = Pattern <$> traverse (goB behind) bs
      goB behind (Branch ps) = Branch <$> traverse (goPiece behind) ps
      goPiece behind (Quantified a q) = (`Quantified` q) <$> goA behind a
      goPiece _ other = pure other
      goA behind a = case a of
        Group c p       -> Group c <$> goP behind p
        Atomic p        -> Atomic <$> goP behind p
        Look Behind s p -> Look Behind s <$> goP True p
        Look d s p      -> Look d s <$> goP behind p
        Lit _ | not behind -> frequency
          [ (5, pure a), (1, Call <$> chooseInt (0, count)) ]
        _               -> pure a
  goP False tr

-- ❓ Does the tree call a group somewhere?
-- 木のどこかで、グループを呼んでいるか
hasCalls :: Pattern -> Bool
hasCalls = not . null . calls
-- ANCHOR_END: gencalls

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
    it "reads the patterns of 1938 tests, passes 1934, and fails none" $ do
      tests <- readTestFile Latin1 "testdata/testoutput1"
      tally (map readable tests) `shouldBe` Tally 1938 0 710
      let verdicts = map judge tests
      tally verdicts `shouldBe` Tally 1934 0 714
    -- the second book, with UTF: no wrong answers either
    -- 2冊目（UTF あり）も、ちがう答えは1つもない
    it "passes 1184 tests of testoutput4, and fails none" $ do
      tests <- readTestFile Utf8 "testdata/testoutput4"
      tally (map readable tests) `shouldBe` Tally 1184 0 349
      let verdicts = map judge tests
      tally verdicts `shouldBe` Tally 1184 0 349
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
        names = backtracking (surely "^(\\w+\\s?)+$")
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
      isLeft (findWithin 100000 (backtracking (surely "(a+)+b")) as)
        `shouldBe` True
      findWithin 100000 (surely "(?>a+)+b") as `shouldBe` Right Nothing
      findAllWithin 100000 (backtracking (surely "(a+)+b")) as
        `shouldBe` Left (GaveUp 100000)
    -- one more a, twice the steps
    -- a が1つ増えると、ステップ数は2倍
    it "takes twice the steps for one more a" $
      [ stepsWith Backtracking "(a+)+b" (T.replicate n "a" <> "c")
      | n <- [10 .. 13] ]
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
      -- calls too: each called group's body is written once more
      -- 呼び出しも：呼ばれるグループの中身は、もう1度書かれる
      forAll (oneof [genTreeRefs [Greedy, Lazy, Possessive] 2, genTreeCalls])
        $ \tr ->
        programSize tr
          >= fromIntegral (length (instructions (compileTree tr)))
    -- the bodies of called groups are written once more, after Match
    -- 呼ばれるグループの中身は、Match のうしろに、もう1度書かれる
    it "counts the bodies of called groups" $ do
      let Right tr = parsePattern [] "(((((a{100})))))(?1)(?2)(?3)(?4)(?5)"
      programSize tr
        >= fromIntegral (length (instructions (compileTree tr)))
        `shouldBe` True
    -- a*+ is fenced: the count has room for the fences, too
    -- a*+ は区切りの中：見積もりは、区切りの命令も数える
    it "counts the fences of possessive repeats" $ do
      let Right tr = parsePattern [] "a*+b*+c*+"
      programSize tr
        >= fromIntegral (length (instructions (compileTree tr)))
        `shouldBe` True
    -- the limit is kept to the step: on the backtracking VM, abc takes
    -- 4 steps (3 takes and the match), so 3 is not enough and 4 is (the
    -- Pike VM, which findWithin chooses for abc, counts its own steps)
    -- 上限は1ステップまで守る：バックトラックの VM では、abc は4ステップ
    -- （take 3つと match）なので、3 では足りず、4 なら足りる（findWithin が
    -- abc に選ぶ Pike VM は、自分のステップを数える）
    it "stops just at the limit" $ do
      let byVM limit = fmap (fmap matchText)
            (fst (searchCountingBy scanVMST limit (surely "abc") "abc" 0))
      byVM 3 `shouldBe` Left (GaveUp 3)
      byVM 4 `shouldBe` Right (Just "abc")
    -- a long text: no stack of Haskell's grows with it
    -- 長い文字列：Haskell のスタックが、それにつれて伸びることはない
    it "goes through 100000 characters" $ do
      let t = T.replicate 50000 "ab" <> "c"
      fmap matchEnd (find (surely "(?:a|b)*c") t) `shouldBe` Just 100001
    -- the window of the story
    -- 物語の窓口
    it "checks the name field" $ do
      checkName "Mira Ruby" `shouldBe` Right True
      checkName "Mira Ruby!" `shouldBe` Right False
      checkNameSlowly nameField `shouldBe` Left (GaveUp windowLimit)
      -- from the linear-time chapter on, the Pike VM answers
      -- 線形時間の章からは、Pike VM が答える
      checkName nameField `shouldBe` Right False
  -- ANCHOR_END: vm

  -- ANCHOR: twovms
  describe "the VM with ST / ST の VM" $ do
    -- the same answer, and the same steps used, as the pure VM's, with
    -- any limit (with no limit, too)
    -- 純粋な VM と、同じ答え、同じステップ数。どんな上限でも（上限なしでも）
    prop "agrees with the pure VM, step for step" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (sublistOf [Multiline, DotAll, Caseless]) $ \opts ->
        forAll (elements [0, 1, 10, 100, 300, maxBound]) $ \limit ->
          case compileWith opts (render tr) of
            Left e   -> counterexample (show e) False
            Right re ->
              let st = searchCountingBy scanVMST limit re t 0
              in  agreeWithin (st, st)
                    ( searchCountingBy scanVM limit re t 0
                    , searchCountingBy scanVMSTFresh limit re t 0 )
    -- findAll's other search: a match that is not empty, from a place
    -- findAll のもう1つの探し方：その場所から、空でない一致
    prop "agrees on a match that is not empty" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (chooseInt (0, T.length t)) $ \from ->
          let re = surely (render tr)
          in  nonEmptyWithinBy scanVMST 300 re t from
                === nonEmptyWithinBy scanVM 300 re t from
    -- the numbers of the chapter before, again
    -- 前の章の数を、もう一度
    it "takes the same steps on the name field" $
      [ snd (searchCountingBy vm maxBound (surely "^(\\w+\\s?)+$") field 0)
      | vm <- [scanVM, scanVMST]
      , n <- [2, 3]
      , let field = T.unwords (take n ["Mira", "Ruby", "Lam"]) <> "!" ]
        `shouldBe` [1969, 8757, 1969, 8757]
    -- the chapter's examples, through all three VMs: going back must
    -- put back what a branch, a group or a look wrote (each answer was
    -- checked with pcre2test 10.49)
    -- 章の例を、3つの VM に通す：戻るときは、枝・グループ・「見る」ものが
    -- 書いたものを書き戻す（答えは、どれも pcre2test 10.49 で確かめた）
    let undone p t answer = it (show (p, t)) $
          [ fmap (fmap groups)
              (fst (searchCountingBy vm maxBound (surely p) t 0))
          | vm <- [scanVM, scanVMST, scanVMSTFresh] ]
            `shouldBe` replicate 3 (Right answer)
    undone "(a|ab)c" "abc" (Just [Just "ab"])
    undone "(a|ab)(c|bcd)(d*)" "abcd" (Just [Just "a", Just "bcd", Just ""])
    undone "(?>(a+))b|(a+)c" "aac" (Just [Nothing, Just "aa"])
    undone "(?=(\\w+))\\w" "Mira" (Just [Just "Mira"])
    undone "(?!(a)b)\\w" "ac" (Just [Nothing])
    undone "b|(a)c" "ac" (Just [Just "a"])
    undone "(a){2}b|aa." "aac" (Just [Nothing])
    -- what a place that failed wrote is put back before the next place,
    -- and \K's start, too
    -- 失敗した場所が書いたものは、次の場所の前に書き戻す。\K の始まりも
    undone "b|(a)c" "ab" (Just [Nothing])
    it "puts back \\K's start when a branch fails" $
      [ fmap (fmap matchText)
          (fst (searchCountingBy vm maxBound (surely "a\\Kb|ac") "ac" 0))
      | vm <- [scanVM, scanVMST, scanVMSTFresh] ]
        `shouldBe` replicate 3 (Right (Just "ac"))
  -- ANCHOR_END: twovms

  -- ANCHOR: pike
  describe "the Pike VM / Pike VM" $ do
    -- the engine compile chose, and why
    -- compile が選んだエンジンと、そのわけ
    it "chooses the Pike VM when it can" $ do
      map (engine . surely)
        [ "^(\\w+\\s?)+$", "(a+)+b", "\\bcolou?r\\b", "(a|)*\\K." ]
        `shouldBe` replicate 4 PikeVM
      map (engine . surely)
        [ "(a)\\1", "a(?=b)", "(?<=a)b", "(?>a+)b", "a++", "\\((?:a|(?R))\\)" ]
        `shouldBe` replicate 6 Backtracking
    it "says no with Linear, naming the instruction" $
      map safeCompile userPatterns `shouldSatisfy` \case
        [Right _, Right _, Left (NotLinear "ref 1"), Left (NotLinear "fence")]
          -> True
        _ -> False
    -- ANCHOR: pikeagree
    -- the same answer as the backtracking VM, groups and \K too, with
    -- the flags m, s and i
    -- バックトラックの VM と同じ答え。グループと \K も、フラグ m・s・i も
    prop "agrees with the backtracking VM" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (sublistOf [Multiline, DotAll, Caseless]) $ \opts ->
          case compileWith (Linear : opts) (render tr) of
            Left e   -> counterexample (show e) False
            Right re ->
              fst (searchCountingBy scanPike maxBound re t 0)
                === fst (searchCountingBy scanVMST maxBound re t 0)
    -- ANCHOR_END: pikeagree
    prop "agrees on findAll, and on a match that is not empty" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (chooseInt (0, T.length t)) $ \from ->
          let re = surely (render tr)
          in  findAll re t === findAll (backtracking re) t
                .&&. nonEmptyWithinBy scanPike maxBound re t from
                       === nonEmptyWithinBy scanVMST maxBound re t from
    prop "keeps to the limit" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (chooseInt (0, 300)) $ \limit ->
          let re = surely (render tr)
          in  case searchCountingBy scanPike limit re t 0 of
                (Left gaveUp, used) ->
                  label "gave up" $
                    gaveUp === GaveUp limit .&&. used === limit
                (Right m, used) ->
                  label "answered" $
                    m === find re t .&&. property (used <= limit)
    -- ANCHOR: pikesteps
    -- one more a, the same few steps more (not twice)
    -- a が1つ増えると、同じ数だけステップ数が増える（2倍ではない）
    it "takes the same few steps more for one more a" $ do
      let steps = [ stepsWith PikeVM "(a+)+b" (T.replicate n "a" <> "c")
                  | n <- [10 .. 13] ]
      steps `shouldBe` [128, 140, 152, 164]
      zipWith (-) (drop 1 steps) steps `shouldBe` [12, 12, 12]
    it "answers on the name field, with any number of names" $ do
      [ stepsWith PikeVM "^(\\w+\\s?)+$" (fieldOf n) | n <- [2 .. 7] ]
        `shouldBe` [135, 189, 243, 312, 381, 450]
      find wordsOnly (fieldOf 10000) `shouldBe` Nothing
    -- ANCHOR_END: pikesteps
    -- the empty round of a repeat sets the group, as in PCRE2
    -- くり返しの空の回もグループを書く（PCRE2 と同じ）
    it "keeps the rules of empty rounds" $ do
      fmap groups (find (surely "(a|)*b") "aab")
        `shouldBe` Just [Just ""]
      fmap groups (find (surely "(a|b|)+?c") "abc")
        `shouldBe` Just [Just "b"]
      fmap groups (find (surely "(?:(a)|b)*") "ab")
        `shouldBe` Just [Just "a"]
  -- ANCHOR_END: pike

  -- ANCHOR: recursion
  describe "calls and recursion / 呼び出しと再帰" $ do
    -- each answer is pcre2test 10.49's, as the meter writes it
    -- 答えは、どれも pcre2test 10.49 のもの。メーターと同じ形で書く
    -- the answer of pcre2test, by the engine (the VM with ST); and the
    -- pure VM and the continuations give the same
    -- pcre2test の答えを、エンジン（ST の VM）で。純粋な VM と継続も同じ答え
    let same p t answer = it (show (p, t)) $ do
          judge (TestCase 0 p "" t answer) `shouldBe` Pass
          let re = surely p
              byVM vm = fmap (fmap allGroups)
                             (fst (searchCountingBy vm maxBound re t 0))
          byVM scanVM `shouldBe` byVM scanVMST
          cont <- try (evaluate (fmap allGroups (searchFromCont re t 0)))
          either (\(e :: GaveUp) -> Left e) Right cont
            `shouldBe` byVM scanVMST
        loops = "Failed: error -52: nested recursion at the same subject\
                \ position"
    same "\\((?:[^()]++|(?R))*\\)" "x(a(b)c)y" [" 0: (a(b)c)"]
    same "\\((?:[^()]++|(?R))*\\)" "((()" [" 0: ()"]
    same "\\((?:[^()]++|(?R))*\\)" "(ab(cd)ef)" [" 0: (ab(cd)ef)"]
    same "(\\((?:[^()]++|(?1))*\\))" "x(a(b)c)y"
      [" 0: (a(b)c)", " 1: (a(b)c)"]
    same "^(.)(\\1|a(?2))" "bab" [" 0: bab", " 1: b", " 2: ab"]
    same "(sens|respons)e and (?1)ibility" "sense and responsibility"
      [" 0: sense and responsibility", " 1: sens"]
    same "^((.)(?1)\\2|.?)$" "abcba" [" 0: abcba", " 1: abcba", " 2: a"]
    same "^((.)(?1)\\2|.?)$" "abba" [" 0: abba", " 1: abba", " 2: a"]
    same "^((.)(?1)\\2|.?)$" "abca" ["No match"]
    same "(?R)" "abc" [loops]
    same "a|(?R)b" "xb" [loops]
    same "(a|(?1)b)" "abb" [" 0: a", " 1: a"]
    same "((?1)a|b)" "ba" [loops]
    same "(?1)(a)" "aa" [" 0: aa", " 1: a"]
    same "(a\\Kb)?c(?1)" "cab" [" 0: b"]
    same "(?<pn>\\((?:[^()]++|(?&pn))*\\))" "(x(y))"
      [" 0: (x(y))", " 1: (x(y))"]
    same "(?P<pn>\\((?:[^()]++|(?P>pn))*\\))" "(x(y))"
      [" 0: (x(y))", " 1: (x(y))"]
    same "\\g<1>-(\\d+)" "12-34" [" 0: 12-34", " 1: 34"]
    same "(\\d+)-\\g'1'" "12-34" [" 0: 12-34", " 1: 12"]
    same "(\\d+)-\\g<-1>" "12-34" [" 0: 12-34", " 1: 12"]
    same "(?+1)-(\\d+)" "12-34" [" 0: 12-34", " 1: 34"]
    same "(?<n>a){0}(?&n)b" "ab" [" 0: ab"]
    same "(a)(?1)\\1" "aaa" [" 0: aaa", " 1: a"]
    same "(?i:(?1))(abc)" "ABCabc" ["No match"]
    same "(?(DEFINE)(?<d>\\d+))(?&d)-(?&d)" "12-345" [" 0: 12-345"]
    same "(?(DEFINE)(?<d>a))(?&d)\\k<d>" "aa" ["No match"]
    same "(?(DEFINE)(?<a>x))+b" "b" [" 0: b"]
    same "(?R)*a" "aa" [loops]
    same "(a|b(?1))+" "bba" [" 0: bba", " 1: bba"]
    same "^(a|b(?1)c)$" "bbacc" [" 0: bbacc", " 1: bbacc"]
    same "(?|(a)|(b))(?1)" "ba" [" 0: ba", " 1: b"]
    same "x(?R)?y" "xxyy" [" 0: xxyy"]
    -- going back into a call: the call's own groups come back
    -- 呼び出しの中へ戻る：呼び出しの中のグループの値が戻ってくる
    same "(?(DEFINE)((.).*\\2))^(?1)x" "bxbxb" [" 0: bxbx"]
    -- an open group, and a call of 2 inside a call of 1
    -- 開いたグループと、1の呼び出しの中の2の呼び出し
    same ".((?R)?)" "aab" [" 0: aab", " 1: ab"]
    same "((?2))((?1))" "a" [loops]
    -- the flags of (?(DEFINE) ...) stay inside
    -- (?(DEFINE) ...) のフラグは、中だけ
    same "(?(DEFINE)(?i))a" "A" ["No match"]
    -- 22 calls deep, each one going back: the trail must not double at
    -- each return (it did, and ran out of memory)
    -- 22 段の呼び出しから、1段ずつ戻る：戻るたびに記録が倍になってはいけない
    -- （倍になって、メモリーを使い切っていた）
    same "^((a)(?1)?b)$" (T.replicate 22 "a" <> T.replicate 22 "b")
      [ " 0: " <> T.replicate 22 "a" <> T.replicate 22 "b"
      , " 1: " <> T.replicate 22 "a" <> T.replicate 22 "b", " 2: a" ]
    -- the same group at the same place, but the look made group 2 grow
    -- (a, aa, aaa): not a loop, and PCRE2 finds it
    -- 同じグループを同じ場所で呼ぶが、「見る」ものがグループ2を伸ばす
    -- （a・aa・aaa）：回り続けるのではないので、PCRE2 も見つける
    same "^(?<r>(?=(\\2a|a))(?:\\2$|(?&r)))$" "aaa"
      [" 0: aaa", " 1: aaa", " 2: a"]
    -- the program: the whole pattern, Match, then the subroutine
    -- プログラム：パターン全体、Match、そのあとにサブルーチン
    it "compiles a(?1)(b)" $
      listing (program (surely "a(?1)(b)")) `shouldBe` T.unlines
        [ "  0  take a", "  1  gosub (?1), 6", "  2  open 1", "  3  take b"
        , "  4  close 1", "  5  match", "  6  take b", "  7  return" ]
    -- a pattern with no call has the same program as before
    -- 呼び出しのないパターンは、前と同じプログラム
    it "leaves the other programs as they were" $
      stepsWith Backtracking "^(\\w+\\s?)+$" "Mira Ruby Lam!"
        `shouldBe` 8757
    -- going round at one place: the error, by find, findAll, findWithin
    -- 同じ場所で回る：find・findAll・findWithin のエラー
    -- findAll: an empty match, then going round at the same place; and
    -- at the end, nothing more is tried
    -- findAll：空の一致のあと、同じ場所で回る。終わりでは、それ以上探さない
    it "goes round after an empty match, but not at the end" $ do
      fmap (map matchText) (findAllWithin defaultLimit (surely "|(?R)b") "x")
        `shouldBe` Left (Looping 0 0)
      fmap (map matchText) (findAllWithin defaultLimit (surely "|(?R)b") "b")
        `shouldBe` Right ["", "b", ""]
    it "gives up on a recursion that goes round" $ do
      findWithin defaultLimit (surely "a|(?R)b") "xb"
        `shouldBe` Left (Looping 0 0)
      evaluate (find (surely "a|(?R)b") "xb") `shouldThrow` (== Looping 0 0)
      evaluate (length (findAll (surely "((?1)a|b)") "ba"))
        `shouldThrow` (== Looping 1 0)
      findWithin defaultLimit (surely "a|(?R)b") "ab"
        `shouldBe` Right (Just (Match "a" 0 1 [] mempty))
    -- PCRE2 stops when the place and how far it has read the text are
    -- the same; MiniRegex, when the place and the groups are. They
    -- agree here: (?!) reads nothing, and the call is not atomic
    -- PCRE2 は「場所と、文字列をどこまで読んだか」、MiniRegex は「場所と、
    -- グループの値」で止める。ここでは同じ：(?!) は何も読まない。呼び出しは
    -- アトミックでない
    it "stops where PCRE2 stops (pcre2test 10.49: error -52)" $ do
      findWithin defaultLimit (surely "(?1)(?!)|(|(?>(?1))y)") "a"
        `shouldBe` Left (Looping 1 0)
      findWithin defaultLimit (surely "(?1)x|(|(?1)y)") "a"
        `shouldBe` Left (Looping 1 0)
    -- ...and here they do not (see "Q. PCRE2 と MiniRegex は、同じ再帰を
    -- 止めますか？" in the chapter). x read the a, so PCRE2 goes on, and
    -- finds the empty match; MiniRegex stops
    -- ……ここではちがう（章の「Q. PCRE2 と MiniRegex は、同じ再帰を
    -- 止めますか？」）。x が a を読んだので、PCRE2 は進んで、空の一致を
    -- 見つける。MiniRegex は止める
    it "stops where PCRE2 finds the empty match (pcre2test: 0: and 1:)" $
      findWithin defaultLimit (surely "(?1)x|(|(?>(?1))y)") "a"
        `shouldBe` Left (Looping 1 0)
    -- .* has read the text to the end before the calls, so PCRE2 sees
    -- nothing new and stops (error -52; with disable_recurseloop_check
    -- it finds aaa); the groups grow, so MiniRegex goes on and finds it
    -- 呼ぶ前に .* が終わりまで読んだので、PCRE2 は何も変わらないと見て止める
    -- （error -52。disable_recurseloop_check を付けると aaa）。グループは
    -- 伸びるので、MiniRegex は進んで見つける
    it "finds aaa where PCRE2 stops (pcre2test: error -52)" $
      fmap (fmap allGroups)
           (findWithin defaultLimit
              (surely "^(?<r>(?=.*)(?=(\\2a|a))(?:\\2$|(?&r)))$") "aaa")
        `shouldBe` Right (Just ("aaa" :| ["aaa", "a"]))
    -- ANCHOR: agree
    -- the two VMs and the continuations agree, calls and all; the
    -- continuations throw what the VMs give back
    -- 2つの VM と継続が、呼び出しがあっても同じ答え。継続は、VM が返す
    -- ものを投げる
    prop "the VMs and the continuations agree on calls" $
      forAll genTreeCalls $ \tr ->
        forAll (resize 10 genLines) $ \t ->
          let re = surely (render tr)
              st = searchCountingBy scanVMST 20000 re t 0
          in  counterexample (T.unpack (render tr)) $
              st === searchCountingBy scanVM 20000 re t 0
                .&&. case fst st of
                  Left (GaveUp _) -> label "gave up" (property True)
                  answer -> ioProperty $ do
                    cont <- try (evaluate (searchFromCont re t 0))
                    let what = either (const "looped") (const "found") answer
                    pure $ label what (cont === answer)
    -- ANCHOR_END: agree
    -- and the real PCRE2 finds the same, when it does not give up
    -- 本物の PCRE2 も、あきらめなければ、同じものを見つける
    prop "finds what PCRE2 finds, with calls" $
      forAll (withCalls genTreeForPcre2 `suchThat` hasCalls) $ \tr ->
        forAll (resize 10 genLines) $ \t ->
          let p = render tr
              exact = Pcre2.Ucp <> Pcre2.NoStartOptimize
                        <> Pcre2.NoAutoPossess
          in  counterexample (T.unpack p) $
              case findWithin 20000 (surely p) t of
                Left (GaveUp _) -> label "MiniRegex gave up" True
                -- PCRE2 with its optimizations off: they look first at what
                -- a match needs, and may skip the places where it would
                -- run round, or change what a repeat gives back
                -- 最適化を止めた PCRE2 と：最適化は、一致に要るものを先に
                -- 見て、回るはずの場所を飛ばしたり、くり返しの返し方を変え
                -- たりする
                -- the two stop in different places (PCRE2 looks at how
                -- far it has read, MiniRegex at the groups): when only one
                -- stops, it is counted, not called a difference
                -- 2つは止める所がちがう（PCRE2 はどこまで読んだか、MiniRegex
                -- はグループの値を見る）。片方だけが止めたら、数えるだけで、
                -- ちがいとはしない
                Left (Looping _ _) -> ioProperty $ do
                  theirs <- try (evaluate (Pcre2.capturesOpt exact p t))
                  pure $ case theirs of
                    Left (_ :: SomeException) -> label "both looped" True
                    Right Nothing -> label "both found nothing" True
                    Right (Just _) -> label "only MiniRegex stopped" True
                Right m -> ioProperty $ do
                  theirs <- try (evaluate (Pcre2.capturesOpt exact p t))
                  pure $ case theirs of
                    Left (_ :: SomeException) ->
                      label "PCRE2 stopped or gave up" True
                    Right ts -> label "compared" (fmap allGroups m === ts)
    -- calls are written back, and read again
    -- 呼び出しも書き戻して、読み直せる
    prop "reads back calls" $
      forAll genTreeCalls $ \tr -> parsePattern [] (render tr) === Right tr
  -- ANCHOR_END: recursion

  -- ANCHOR: callerrors
  describe "wrong calls, like PCRE2 10.49 / まちがった呼び出し" $ do
    -- each place and message was checked with pcre2test 10.49
    -- 場所と文は、どれも pcre2test 10.49 で確かめた
    let wrong p at m = it (T.unpack p) $
          parsePattern [] p `shouldBe` Left (Invalid at m)
        e115 = "reference to non-existent subpattern"
        e161 = "subpattern number is too big"
    wrong "(?&nosuch)"     3 e115
    wrong "(?P>nosuch)"    4 e115
    wrong "\\g<nosuch>"    3 e115
    wrong "(?3)(a)"        3 e115
    wrong "(?1)"           3 e115
    wrong "\\g<2>"         5 e115
    wrong "(a)\\g'2'"      8 e115
    wrong "(a)\\g<-2>"     5 e115
    wrong "(a)\\g<+1>"     9 e115
    wrong "(a)(?-2)"       7 e115
    wrong "(a)(?+1)"       7 e115
    wrong "(?65535)"       7 e115
    wrong "(?65536)"       7 e161
    wrong "(?12345678901)" 13 e161
    wrong "\\g<65536>"     2 e161
    wrong "(?+65536)"      8 e161
    wrong "(a)(?-65536)"   11 e161
    -- too big is told before the missing ) or >
    -- 大きすぎることは、) や > がないことより先に言う
    wrong "(?65536"        7 e161
    wrong "(?+65536"       8 e161
    wrong "\\g<65536"      2 e161
    -- back past the first group: told at once, before what follows
    -- 最初のグループより前を指す：うしろより先に、すぐに言う
    wrong "(?-1)("         4 e115
    wrong "(?-1"           4 e115
    wrong "\\g<-1"         2 e115
    wrong "(?1)b(?-1)"     9 e115
    wrong "(a)(?-2)("      7 e115
    wrong "\\g{-1}("        2 e115
    wrong "\\g-1("          4 e115
    wrong "(?+0)"          4 "a relative value of zero is not allowed"
    wrong "\\g<-0>"        2 "a relative value of zero is not allowed"
    wrong "(?R"            3
      "(?R (recursive pattern call) must be followed by a closing\
      \ parenthesis"                                              -- 158
    wrong "(?1"            3 "missing closing parenthesis"        -- 114
    wrong "(?1x)(a)"       3 "missing closing parenthesis"
    wrong "(?&a"           4 "missing closing parenthesis"
    wrong "(?<n>a)(?&n )"  11 "missing closing parenthesis"
    wrong "(?(DEFINE)"     10 "missing closing parenthesis"
    wrong "(?&)"           3 "subpattern name expected"           -- 162
    wrong "(?P>)"          4 "subpattern name expected"
    wrong "\\g<>"          3 "subpattern name expected"
    wrong "\\g< 1 >"       3 "subpattern name expected"
    wrong "(?&1a)"         4 "subpattern name must start with a non-digit"
    wrong "\\g<1"          4
      "syntax error in subpattern number (missing terminator?)"   -- 219
    wrong "\\g<1x>"        4
      "syntax error in subpattern number (missing terminator?)"
    wrong "\\g<a-b>"       4
      "syntax error in subpattern name (missing terminator?)"     -- 142
    wrong "(?(DEFINE)(?<a>x)|y)" 3
      "DEFINE subpattern contains more than one branch"           -- 154
    -- PCRE2 can measure some calls in a lookbehind; MiniRegex not yet
    -- PCRE2 は後読みの中の呼び出しを測れることがある。MiniRegex は、まだ
    it "says not yet to a call in a lookbehind" $
      parsePattern [] "(?<=(?1))(a)"
        `shouldBe` Left (Unsupported 0 "call in lookbehind")
  -- ANCHOR_END: callerrors

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
    -- \g<1> calls a group (the chapter on recursion)
    -- \g<1> はグループの呼び出し（再帰の章）
    it "reads \\g<1> as a call" $
      fmap render (parsePattern [] "(a)\\g<1>") `shouldBe` Right "(a)(?1)"
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
    -- the chapter's other GHCi examples, kept as examples
    -- 章のほかの GHCi の例も、例として固めておく
    all' "\\Bx\\B" "axb x" ["x"]
    all' "(?<!price:)\\b\\d+" "price:12 34" ["34"]
    all' "(?<=-)\\d+" "1-2-3" ["2", "3"]  -- a lookbehind can look back
    all' "\\d+-\\K\\d+" "1-2-3" ["2"]           -- \\K's front is eaten
    all' "\\d+(?!\\d| gold)" "300 gold 5" ["5"]
    all' "(?<=a)b|a(?=b)" "ab" ["a", "b"]
    all' "x\\K\\d" "x1 x2 3" ["1", "2"]
    it "does not give back what (?>a*) took" $
      isMatch (surely "(?>a*)a") "aaa" `shouldBe` False
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
      parsePattern [] "(?1(1))(a)" `shouldBe` Left (Unsupported 3 "(?n(")
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
          Left (NotLinear _)     -> counterexample "not reached" False
          Right _                -> label "read" (answer === Nothing)
  -- ANCHOR_END: pcre2errors

  -- ANCHOR: answers
  describe "the training / トレーニング" $ do
    -- step 1: what the chapter asks you to guess
    -- ステップ1：予想してもらうもの
    it "answers the guesses" $ do
      engine (surely "colou?r") `shouldBe` PikeVM
      engine (surely "(\\w+)@\\1") `shouldBe` Backtracking
      fmap engine (compileWith [Linear] "a++b") `shouldBe` Left (NotLinear "fence")
      fmap matchText (find (surely "a|ab") "ab") `shouldBe` Just "a"
    -- step 2: the search box
    -- ステップ2：検索の欄
    it "runs only patterns in linear time" $ do
      searchBox "R\\w+" "Mira Ruby Lam" `shouldBe` Right ["Ruby"]
      searchBox "(a+)+!" "aaaa" `shouldBe` Right []
      searchBox "(\\w+) \\1" "a a"
        `shouldBe` Left "(\\w+) \\1\n^\nnot linear: the program has ref 1\n"
      searchBox "(a" "a" `shouldSatisfy` isLeft
    -- step 3: with a set of places, the same as isMatch
    -- ステップ3：場所の集合でも、isMatch と同じ
    prop "matches by a set, as isMatch does" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 genLines) $ \t ->
          let re = surely (render tr)
          in  matchesBySet re t === isMatch re t
    it "matches by a set on a long name field" $
      matchesBySet wordsOnly (fieldOf 10000) `shouldBe` False
  -- ANCHOR_END: answers
