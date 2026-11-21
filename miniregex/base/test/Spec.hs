module Main (main) where

-- ANCHOR: imports
import Control.Exception (SomeException, evaluate, try)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)
import Test.Hspec hiding (before)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (GaveUp, Negative, Positive, once)
import Text.Regex.Pcre2 qualified as Pcre2


import MiniRegex
import MiniRegex.Compat
import MiniRegex.Compile (listing)
import MiniRegex.Engine
  ( nonEmptyAt, nonEmptyWithinBy, scanSwitching, searchCounting
  , searchCountingBy, searchFrom, searchWithin
  , searchFromCont, searchFromList, budgetFor )
import MiniRegex.Pike (scanPike)
import MiniRegex.PikeST (scanPikeST)
import MiniRegex.Program (instructions)
import MiniRegex.Race (findRacing)
import MiniRegex.Prefilter
  (kmpPlaces, kmpTable, textPlaces)
import Data.Text.Unsafe (lengthWord8)
import MiniRegex.Cursor (Cursor, at, back, before, byte, cursorAt, rest, taken)
import Data.Foldable (toList)
import MiniRegex.Examples
import MiniRegex.Groups (calls, groupCount)
import MiniRegex.Parser
import MiniRegex.Pretty
import MiniRegex.Syntax
import MiniRegex.Types (Regex (..), program)
import MiniRegex.Bounded
  (boundedFits, canBound, scanBounded, scanBoundedAtTheDoor)
import MiniRegex.STVM (scanVMST, scanVMSTFresh)
import MiniRegex.Unicode (caseVariants, fold)
import MiniRegex.VM (scanVM)
import MiniRegex.LazyDFA (canDfa, dfaFind, dfaFor, reversePattern)
import Data.Maybe (fromMaybe, isJust)
import RegexAnswers
import RegexBaseAnswers
import Data.Array (elems, (!))
import Data.ByteString qualified as B
import Data.Text.Encoding (encodeUtf8)
import MiniRegex.Portable (countGold, datesIn, firstWord, salesIn)
import Text.Regex.MiniRegex qualified as RB
import Text.Regex.TDFA qualified as TDFA
import Neighbor qualified
import NeighborMini qualified
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

-- 🎲 For the chapter on learning from other lands: patterns with a
--    text inside, or of texts only, and characters to make texts of.
-- よその国に学ぶ章のため：内側に文字列のあるパターンと、文字列だけの
-- パターン。文字列を作る文字
lessonPatterns :: [Text]
lessonPatterns =
  [ "\\d{4}-\\d{2}", "\\d{2}-\\d", "[\\w.+-]+@[\\w-]+\\.[\\w.-]+"
  , "(\\w+) sold (\\d+)", "\\w+ s", "a\\w-", "(\\w+)@", "\\w+@\\w"
  , "(?i)\\w+@", "[a@]+@b", "Go|Gold", "Gold|Go", "ab|b|abc", "a|aa"
  , "(?i)ok", "(?i)sold|so", "日|日本" ]

lessonChars :: [Char]
lessonChars = "12a-@.b s+é日本soldGOgOKkK\x212A"

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

-- 📍 A cursor at the start of a text.
-- 文字列の頭に置いたしおり
lastOf :: Text -> Maybe Char
lastOf t
  | T.null t  = Nothing
  | otherwise = Just (T.last t)

cursorAtStart :: Text -> Cursor
cursorAtStart t = maybe (error "no cursor") id (cursorAt t 0)

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
      others <- resize 3 (listOf (genItem reach))
      pure (CharClass neg (first :| others))
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

-- 🎲 For the chapter on the lazy DFA: the same, without anchors and \K
--    (the DFA does not run them), so that more patterns go by the DFA.
-- lazy DFA の章のため：同じ木から、アンカーと \K を抜いたもの（DFA は
-- 動かさないので）。DFA を通るパターンが増える
genTreeDfa :: Gen Pattern
genTreeDfa = goP <$> genTreeLinear
  where
    goP (Pattern bs) = Pattern (fmap goB bs)
    goB (Branch ps)  = Branch [goPiece p | p <- ps, keep p]
    keep (Anchor _) = False
    keep KeepOut    = False
    keep _          = True
    goPiece (Quantified a q) = Quantified (goA a) q
    goPiece other            = other
    goA (Group c p) = Group c (goP p)
    goA a           = a

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
    -- places are counted in characters
    -- 場所は文字で数える
    it "counts places in characters" $
      fmap (\m -> (matchText m, matchStart m, matchEnd m))
        (find (surely "b") "日本b") `shouldBe` Just ("b", 2, 3)
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
    it "reads 1938 patterns and passes 1929 tests, and fails none" $ do
      tests <- readTestFile Latin1 "testdata/testoutput1"
      tally (map readable tests) `shouldBe` Tally 1938 0 710
      let verdicts = map judge tests
      tally verdicts `shouldBe` Tally 1929 0 719
    -- the second book, with UTF: no wrong answers either
    -- 2冊目（UTF あり）も、ちがう答えは1つもない
    it "passes 1181 tests of testoutput4, and fails none" $ do
      tests <- readTestFile Utf8 "testdata/testoutput4"
      tally (map readable tests) `shouldBe` Tally 1184 0 349
      let verdicts = map judge tests
      tally verdicts `shouldBe` Tally 1181 0 352
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
            Right re -> searchFromList re t 0 === searchFromCont re t 0
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
              in  vm === searchFromCont re t 0
                    .&&. vm === searchFromList re t 0
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
      isLeft (findWithin 100000 (backtracking (everywhere (surely "(a+)+b"))) as)
        `shouldBe` True
      findWithin 100000 (surely "(?>a+)+b") as `shouldBe` Right Nothing
      findAllWithin 100000 (backtracking (everywhere (surely "(a+)+b"))) as
        `shouldBe` Left (GaveUp 100000)
    -- one more a, twice the steps
    -- a が1つ増えると、ステップ数は2倍
    it "takes twice the steps for one more a" $
      [ stepsWith Backtracking "(a+)+b" (T.replicate n "a" <> "c")
      | n <- [10 .. 13] ]
        `shouldBe` [14276, 28607, 57274, 114613]
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
              in  st === searchCountingBy scanVM limit re t 0
                    .&&. st === searchCountingBy scanVMSTFresh limit re t 0
    -- findAll's other search: a match that is not empty, from a place
    -- findAll のもう1つの探し方：その場所から、空でない一致
    prop "agrees on a match that is not empty" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (chooseInt (0, T.length t)) $ \from ->
          let re = surely (render tr)
          in  nonEmptyWithinBy scanVMST 300 re t from
                === nonEmptyWithinBy scanVM 300 re t from
    -- fewer steps than the chapter before (1969 and 8757): no mark
    -- and no ifstill in the loop
    -- 前の章（1969 と 8757）より少ない。輪に mark と ifstill がない
    it "takes the same steps on the name field" $
      [ snd (searchCountingBy vm maxBound
               (everywhere (surely "^(\\w+\\s?)+$")) field 0)
      | vm <- [scanVM, scanVMST]
      , n <- [2, 3]
      , let field = T.unwords (take n ["Mira", "Ruby", "Lam"]) <> "!" ]
        `shouldBe` [1268, 5624, 1268, 5624]
  -- ANCHOR_END: twovms

  -- ANCHOR: pike
  describe "the Pike VM / Pike VM" $ do
    -- the engine compile chose, and why
    -- compile が選んだエンジンと、そのわけ
    it "chooses the Pike VM when it can (bounded, from this chapter)" $ do
      map (engine . surely)
        [ "^(\\w+\\s?)+$", "(a+)+b", "\\bcolou?r\\b", "(a|)*\\K." ]
        `shouldBe` replicate 4 Bounded
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
      steps `shouldBe` [64, 70, 76, 82]
      zipWith (-) (drop 1 steps) steps `shouldBe` [6, 6, 6]
    it "answers on the name field, with any number of names" $ do
      [ stepsWith PikeVM "^(\\w+\\s?)+$" (fieldOf n) | n <- [2 .. 7] ]
        `shouldBe` [82, 115, 148, 190, 232, 274]
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

  -- ANCHOR: prefilter
  describe "looking first / 先に探す" $ do
    -- what it learns from a pattern: like pcre2test's /I
    -- パターンから調べること：pcre2test の /I と同じように
    it "knows where a match can start, and what it needs" $
      map describePrefilter
        [ "Room (\\d+)", "\\b\\d{4}-\\d{2}-\\d{2}\\b", "^(\\w+\\s?)+$"
        , "colou?r|grey", "(\\w+)@(\\w+)\\.com", "(a+)*b", "x*"
        , "(?i)dragon", "(?=\\d)\\w+" ]
        `shouldBe`
          -- (the needed characters are sorted, from the chapter on
          -- measuring)
          -- （必ず要る文字は、測って比べる章から、並べ替えてある）
          [ ("prefix \"Room \"", "\" Rmo\"")
          , ("one of \\d", "\"-\"")
          , ("only the first place", "\"\"")
          , ("one of c g", "\"r\"")
          -- (from the chapter on learning from other lands: a text
          -- inside to step back from, and a pattern of texts only)
          -- （よその国に学ぶ章から：戻る内側の文字列と、文字列だけの
          -- パターン）
          , ("\\w+, then \"@\"", "\".@cmo\"")
          , ("one of a b", "\"b\"")
          , ("anywhere", "\"\"")
          , ("only 64 texts: \"DRAGON\" ... \"dragon\"", "\"\"")
          , ("one of \\d", "\"\"") ]
    -- the VM is handed fewer places
    -- VM に渡す場所が減る
    it "hands the VM only the places worth trying" $ do
      triedPlaces "Room (\\d+)" records `shouldBe` [135]
      triedPlaces "(a+)*b" (T.replicate 70 "a" <> "c") `shouldBe` []
      triedPlaces "^a" "aaa" `shouldBe` [0]
      stepsOf "(a+)+b" (T.replicate 30 "a" <> "c") `shouldBe` 0
      stepsOf "(\\w+)@(\\w+)\\.com" (archive 1000) `shouldBe` 0
    -- the same answers as trying every place; only a search that gave
    -- up, or went round, may now find its answer
    -- 全部の場所を試したときと同じ答え。あきらめた探しものと、回った
    -- 探しものだけが、答えを見つけることがある
    prop "finds what trying every place finds" $
      forAll (oneof [ genTreeRefs [Greedy, Lazy, Possessive] 2
                    , genTreeCalls ]) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (chooseInt (0, T.length t)) $ \from ->
          let re = surely (render tr)
          in  counterexample (T.unpack (render tr)) $
                case searchWithin 20000 (everywhere re) t from of
                  Right m -> searchWithin 20000 re t from === Right m
                  Left _  -> label "gave up" (property True)
    prop "finds all that trying every place finds" $
      forAll (genTreeRefs [Greedy, Lazy] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
          let re = surely (render tr)
          in  findAll re t === findAll (everywhere re) t
    -- KMP finds the same places as Text's search
    -- KMP は、Text の検索と同じ場所を見つける
    prop "finds the same places with KMP and with Text's search" $
      forAll genText $ \t ->
        forAll (genText `suchThat` (not . T.null)) $ \needle ->
          let c = cursorAtStart t
          in  map at (kmpPlaces needle c) === map at (textPlaces needle c)
    prop "finds the same with KMP" $
      forAll genText $ \t ->
        forAll (genText `suchThat` ((>= 2) . T.length)) $ \needle ->
          let re = surely (escape needle <> "(.?)")
          in  findAll (byKmp re) t === findAll re t
    -- what we look for overlaps itself: every place is found
    -- 探すものが自分と重なっても、全部の場所が見つかる
    it "finds places that overlap" $ do
      map at (textPlaces "aa" (cursorAtStart "aaaa")) `shouldBe` [0, 1, 2]
      map at (kmpPlaces "aa" (cursorAtStart "aaaa")) `shouldBe` [0, 1, 2]
      map at (kmpPlaces "abab" (cursorAtStart "xababab"))
        `shouldBe` [1, 3]
    it "makes KMP's table as the chapter on strings did" $
      toList (kmpTable "abcabd") `shouldBe` [0, 0, 0, 1, 2, 0]
  -- ANCHOR_END: prefilter

  -- ANCHOR: cursortests
  describe "the cursor in bytes / バイトのしおり" $ do
    -- the same as counting from the head with Text's functions
    -- Text の関数で頭から数えたのと同じ
    prop "knows what is behind and ahead" $
      forAll genText $ \t ->
        forAll (chooseInt (0, T.length t)) $ \n ->
          case cursorAt t n of
            Nothing -> property False
            Just c  ->
              rest c === T.drop n t
                .&&. before c === lastOf (T.take n t)
                .&&. byte c === lengthWord8 (T.take n t)
    prop "steps back as cursorAt counts" $
      forAll genText $ \t ->
        forAll (chooseInt (0, T.length t)) $ \n ->
          forAll (chooseInt (0, n + 2)) $ \k ->
            (cursorAt t n >>= back k) === cursorAt t (n - k)
    prop "takes the text between two cursors" $
      forAll genText $ \t ->
        forAll (chooseInt (0, T.length t)) $ \i ->
          forAll (chooseInt (i, T.length t)) $ \j ->
            (taken <$> cursorAt t i <*> cursorAt t j)
              === Just (T.take (j - i) (T.drop i t))
    it "counts 日本語 in characters and in bytes" $ do
      fmap byte (cursorAt "日本語" 2) `shouldBe` Just 6
      fmap rest (cursorAt "日本語" 2) `shouldBe` Just "語"
      fmap (fmap byte . back 1) (cursorAt "日本語" 2)
        `shouldBe` Just (Just 3)
  -- ANCHOR_END: cursortests

  -- ANCHOR: looptests
  describe "loops with no marks / 印のない輪" $ do
    -- a loop whose body always takes a character needs no mark
    -- 中身が必ず1文字以上食べる輪には、印が要らない
    it "compiles a*b with no mark" $
      listing (program (unpossessed (surely "a*b"))) `shouldBe` T.unlines
        [ "  0  split 1, 3", "  1  take a", "  2  split 1, 3"
        , "  3  take b", "  4  match" ]
    -- half the steps for a*b: two instructions for each a, not four
    -- a*b のステップ数は半分：a 1つに、4命令ではなく2命令
    it "takes fewer steps" $ do
      stepsPlain Backtracking "a*b" "aaaab" `shouldBe` 12
      stepsPlain Backtracking "a*b" (T.replicate 10000 "a" <> "b")
        `shouldBe` 20004
      stepsPlain Backtracking "^(\\w+\\s?)+$" (fieldOf 5) `shouldBe` 166401
    it "keeps the marks where a round may take nothing" $
      [ "mark" `T.isInfixOf` listing (program (surely p))
      | p <- ["a*", "(?:ab)+", "(\\w+\\s?)+", "(a|)*", "(?:a?b?)*", "(?:\\b)*"] ]
        `shouldBe` [False, False, False, True, True, True]
    -- the narrowing of the needed characters comes to an end
    -- 必ず要る文字の絞り込みが、終わる
    it "finds the needed characters of a*abc" $
      map describePrefilter ["a*abc", "a+ab"]
        `shouldBe` [("one of a", "\"abc\""), ("one of a", "\"ab\"")]
  -- ANCHOR_END: looptests

  -- ANCHOR: possesstests
  describe "possessive loops / 所有的な輪" $ do
    it "makes a*b one takeall" $
      listing (program (surely "a*b")) `shouldBe` T.unlines
        [ "  0  takeall a", "  1  take b", "  2  match" ]
    it "makes a loop possessive only where giving back cannot help" $
      [ "takeall" `T.isInfixOf` listing (program (surely p))
      | p <- [ "a*b", "\\d+ gold", "[^\"]*\"", "(?i)a+B"
             , "a*a", "\\w+\\s?$", "(a+)+b", "a*\\b", "a*?b", "(?i)a+A" ] ]
        `shouldBe` [True, True, True, True, False, False, False, False, False, False]
    -- a takeall is one step for each character, and one for itself
    -- takeall は、1文字に1ステップと、自分の1ステップ
    it "takes fewer steps" $ do
      stepsWith Backtracking "a*b" "aaaab" `shouldBe` 7
      stepsWith Backtracking "a*b" (T.replicate 10000 "a" <> "b")
        `shouldBe` 10003
    -- the same answers as the program with no possessive loops
    -- 所有的な輪のないプログラムと、同じ答え
    prop "gives the same answers as the plain program" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
          let re = surely (render tr)
          in  findAll (backtracking re) t === findAll (backtracking (unpossessed re)) t
  -- ANCHOR_END: possesstests

  -- ANCHOR: pikesttests
  describe "the Pike VM with ST / ST の Pike VM" $ do
    -- the same answers as the Pike VM of the linear-time chapter, and
    -- never more steps
    -- 線形時間の章の Pike VM と同じ答えで、ステップ数は多くならない
    prop "agrees with the Pike VM, in fewer steps or as many" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (sublistOf [Multiline, DotAll, Caseless]) $ \opts ->
          case compileWith (Linear : opts) (render tr) of
            Left e   -> counterexample (show e) False
            Right re ->
              let (old, n) = searchCountingBy scanPike maxBound re t 0
                  (new, k) = searchCountingBy scanPikeST maxBound re t 0
              in  new === old .&&. property (k <= n)
    prop "agrees on a match that is not empty" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 genLines) $ \t ->
        forAll (chooseInt (0, T.length t)) $ \from ->
          let re = surely (render tr)
          in  nonEmptyWithinBy scanPikeST maxBound re t from
                === nonEmptyWithinBy scanPike maxBound re t from
    it "takes as many steps as the Pike VM on the name field" $
      [ stepsWith PikeVM "^(\\w+\\s?)+$" (fieldOf n) | n <- [2 .. 7] ]
        `shouldBe` [82, 115, 148, 190, 232, 274]
  -- ANCHOR_END: pikesttests

  -- ANCHOR: switchtests
  describe "switching engines / エンジンの乗り換え" $ do
    -- (a pattern made at random can be too much for the backtracking
    -- VM alone: then there is nothing to compare with)
    -- （でたらめに作ったパターンは、バックトラックの VM だけでは手に
    -- 負えないことがある。そのときは、くらべる相手がない）
    prop "agrees with the backtracking VM" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 genLines) $ \t ->
          let re = surely (render tr)
          in  case findWithin defaultLimit (backtracking re) t of
                Left _  -> discard
                Right m -> find re t === m
    -- the budget: twice the instructions, for each byte and the end
    -- 予算：命令の数の2倍を、バイトごとと終わりのぶん
    it "gives the backtracking VM a budget" $
      fmap (budgetFor (program wordsOnly)) (cursorAt (fieldOf 5) 0)
        `shouldBe` Just (2 * length (instructions (program wordsOnly)) * 24)
    it "switches to the Pike VM on the name field" $ do
      checkName (fieldOf 5) `shouldBe` Right False
      find wordsOnly (fieldOf 10000) `shouldBe` Nothing
      snd (searchCountingBy scanSwitching maxBound wordsOnly (fieldOf 5) 0)
        `shouldBe` 647
      stepsOf "a*b" (T.replicate 10000 "a" <> "b") `shouldBe` 10003
    prop "races to the same answer" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 genLines) $ \t ->
          let re = surely (render tr)
          in  ioProperty ((=== find re t) <$> findRacing re t)
  -- ANCHOR_END: switchtests

  -- ANCHOR: boundedtests
  describe "remembering where it has been / 通った所を覚える" $ do
    -- the same answers as the backtracking VM, where it can run (to
    -- compare with, the Pike VM: it has the same answers, and never
    -- takes too long on a pattern made at random)
    -- 動かせるところでは、バックトラックの VM と同じ答え（くらべる相手は
    -- Pike VM。答えは同じで、でたらめに作ったパターンでも時間がかかり
    -- すぎない）
    prop "agrees with the backtracking VM" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 (oneof [genLines, genText])) $ \t ->
        forAll (sublistOf [Multiline, DotAll, Caseless]) $ \opts ->
          case compileWith opts (render tr) of
            Left e   -> counterexample (show e) False
            Right re ->
              canBound (program re) ==>
                let answer vm = fst (searchCountingBy vm maxBound re t 0)
                in  answer scanBounded === answer scanPikeST
                      .&&. answer scanBoundedAtTheDoor === answer scanPikeST
    prop "agrees on a match that is not empty" $
      forAll genTreeLinear $ \tr ->
        forAll (resize 12 (oneof [genLines, genText])) $ \t ->
        forAll (chooseInt (0, T.length t)) $ \from ->
          let re = surely (render tr)
          in  canBound (program re) ==>
                nonEmptyWithinBy scanBounded maxBound re t from
                  === nonEmptyWithinBy scanPikeST maxBound re t from
    -- every pair once: the steps grow in a line with the name field
    -- どの組も1度だけ：名前の欄が長くなっても、ステップ数はまっすぐ増える
    it "takes few steps on the name field" $
      [ stepsWith Bounded "^(\\w+\\s?)+$" (fieldOf n) | n <- [2 .. 7] ]
        `shouldBe` [90, 126, 162, 208, 254, 300]
    it "is what find uses on a short text" $ do
      stepsOf "^(\\w+\\s?)+$" (fieldOf 5) `shouldBe` 185
      find wordsOnly (fieldOf 10000) `shouldBe` Nothing
    it "takes only short texts" $ do
      canBound (program wordsOnly) `shouldBe` True
      canBound (program (surely "(a|)*")) `shouldBe` False
      fmap (boundedFits (program wordsOnly)) (cursorAt (fieldOf 5) 0)
        `shouldBe` Just True
      fmap (boundedFits (program wordsOnly)) (cursorAt (fieldOf 100000) 0)
        `shouldBe` Just False
  -- ANCHOR_END: boundedtests

  -- ANCHOR: guildtests
  describe "the guild's book / ギルドの帳面" $ do
    -- the same text and the same counts as the other languages
    -- (code/langs/regex-benchmarks/)
    -- ほかの言語と同じ文字列、同じ件数（code/langs/regex-benchmarks/）
    it "is the same book as the other languages'" $ do
      guildBook 3 `shouldBe` T.unlines
        [ "2026-11-01 00:00 Mira sold 1 herbs to Ruby for 0 gold "
            <> "<mira@guild.example>"
        , "2026-11-02 01:07 Ruby sold 2 potions to Perl for 37 gold"
        , "2026-11-03 02:14 Lam sold 3 swords to Go for 74 gold" ]
      T.length (guildBook 20000) `shouldBe` 1183102
    it "counts what the other languages count" $
      [ (length ms, sum [read (T.unpack g) | Just g <- map (group 2) ms])
      | p <- [ "\\d{4}-\\d{2}-\\d{2}", "[\\w.+-]+@[\\w-]+\\.[\\w.-]+"
             , "Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go", "(?i)perl"
             , "(\\w+) sold (\\d+) (\\w+)" ]
      , let ms = findAll (surely p) (guildBook 20000) ]
        `shouldBe` [ (20000, 0), (2858, 0), (40000, 0), (5357, 0)
                   , (20000, 99993 :: Int) ]
    it "agrees with pcre2 on the book" $
      let mail = Pcre2.match "[\\w.+-]+@[\\w-]+\\.[\\w.-]+" :: Text -> [Text]
      in  length (mail (guildBook 2000)) `shouldBe` 286
  -- ANCHOR_END: guildtests

  -- ANCHOR: walktests
  describe "findAll with a cursor / しおりで歩く findAll" $ do
    prop "finds what the walk of the chapter before found" $
      forAll (genTreeRefs [Greedy, Lazy, Possessive] 2) $ \tr ->
        forAll (resize 12 genLines) $ \t ->
          let re = surely (render tr)
          in  findAll re t === findAllSlowly re t
    it "finds every entry of a long tally" $
      length (findAll (surely "\\d+") (tallyOf 20000)) `shouldBe` 20000
  -- ANCHOR_END: walktests

  -- ANCHOR: recursion
  describe "calls and recursion / 呼び出しと再帰" $ do
    -- each answer is pcre2test 10.49's, as the meter writes it
    -- 答えは、どれも pcre2test 10.49 のもの。メーターと同じ形で書く
    let same p t answer = it (show (p, t)) $
          judge (TestCase 0 p "" t answer) `shouldBe` Pass
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
        `shouldBe` 5624
    -- going round at one place: the error, by find, findAll, findWithin
    -- 同じ場所で回る：find・findAll・findWithin のエラー
    it "gives up on a recursion that goes round" $ do
      findWithin defaultLimit (surely "a|(?R)b") "xb"
        `shouldBe` Left (Looping 0 0)
      evaluate (find (surely "a|(?R)b") "xb") `shouldThrow` (== Looping 0 0)
      evaluate (length (findAll (surely "((?1)a|b)") "ba"))
        `shouldThrow` (== Looping 1 0)
      findWithin defaultLimit (surely "a|(?R)b") "ab"
        `shouldBe` Right (Just (Match "a" 0 1 [] mempty))
    -- ANCHOR: agree
    -- the two VMs and the continuations agree, calls and all; the
    -- continuations throw what the VMs give back
    -- 2つの VM と継続が、呼び出しがあっても同じ答え。継続は、VM が返す
    -- ものを投げる
    -- (every place, as the continuations do: looking first can turn
    -- a loop into "not found", as in PCRE2)
    -- （継続と同じく全部の場所で。先に見ると、回るものが「見つからない」
    -- になることがある。PCRE2 と同じ）
    prop "the VMs and the continuations agree on calls" $
      forAll genTreeCalls $ \tr ->
        forAll (resize 10 genLines) $ \t ->
          let re = everywhere (surely (render tr))
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
          in  counterexample (T.unpack p) $
              case findWithin 20000 (surely p) t of
                Left (GaveUp _) -> label "MiniRegex gave up" True
                -- PCRE2 looks first at what a match needs (how long, which
                -- character first), and may say "no match" without ever
                -- running round; when it runs, it gives up, too
                -- PCRE2 は、一致に要るもの（長さ、最初の文字）を先に見て、
                -- 回る前に「ない」と言うことがある。動かせば、PCRE2 も
                -- あきらめる
                Left (Looping _ _) -> ioProperty $ do
                  theirs <- try (evaluate (Pcre2.capturesOpt Pcre2.Ucp p t))
                  pure $ case theirs of
                    Left (_ :: SomeException) -> label "both looped" True
                    Right ts -> label "PCRE2 saw no need to run"
                                  (ts === Nothing)
                Right m -> ioProperty $ do
                  theirs <- try (evaluate (Pcre2.capturesOpt Pcre2.Ucp p t))
                  pure $ case theirs of
                    Left (_ :: SomeException) -> label "PCRE2 gave up" True
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
    let finds p t answer = it (show (p, t)) $
          fmap groups (find (surely p) t) `shouldBe` answer
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
    let finds p t answer = it (show (p, t)) $
          fmap matchText (find (surely p) t) `shouldBe` answer
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
    wrong "$"       1 "invalid replacement string"                -- -35
    wrong "$-"      1 "invalid replacement string"
    wrong "<${}>"   3 "invalid replacement string"
    wrong "<${ 1 }>" 3 "invalid replacement string"
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
    mapM_ (\(p, opts, t, answer) ->
             it (show (p, opts, t)) $
               fmap matchText (find (surelyWith opts p) t) `shouldBe` answer)
      cases
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
    -- names of groups, and in templates, may be Unicode letters
    -- グループの名前とテンプレートの名前に、Unicode の文字を使える
    it "takes ${年} in a template" $ do
      let re = surely "(?<年>\\d+)年"
      fmap (\x -> replaceAllTemplate re x "2026年") (template re "<${年}>")
        `shouldBe` Right "<2026>"
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
    let all' p t answer = it (show (p, t)) $
          map matchText (findAll (surely p) t) `shouldBe` answer
        first p t answer = it (show (p, t)) $
          fmap allGroups (find (surely p) t) `shouldBe` answer
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
    it "says not yet to a back reference in a lookbehind" $
      parsePattern [] "(?<=(a)\\1)b"
        `shouldBe` Left (Unsupported 0 "back reference in lookbehind")
  -- ANCHOR_END: lookerrors

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
                  Right re -> ioProperty $ do
                    -- PCRE2 may give up (its match limit): skip those
                    -- PCRE2 があきらめる（ステップ数の上限）ものは飛ばす
                    theirs <- try (evaluate
                      (Pcre2.capturesOpt (pcre2Options opts) p t))
                    pure $ case theirs of
                      Left (_ :: SomeException) -> label "PCRE2 gave up" True
                      Right ts -> fmap allGroups (find re t) === ts
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
                (Right re, Right x) -> ioProperty $ do
                  theirs <- try (evaluate (Pcre2.subOpt
                    (Pcre2.SubGlobal <> Pcre2.SubUnsetEmpty) p tpl t))
                  pure $ case theirs of
                    Left (_ :: SomeException) -> label "PCRE2 gave up" True
                    Right ts -> replaceAllTemplate re x t === ts
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

  -- ANCHOR: regexbase
  describe "the face of regex-base / regex-base の顔" $ do
    let line = "2026-11-21 Mira sold 300 gold, Ruby sold 25 gold" :: Text
        gold = "(\\w+) sold (\\d+)" :: String
    -- one operator, an answer for each type
    -- 演算子は1つ、答えは型ごとに
    it "gives the answer the type asks for" $ do
      (line RB.=~ gold :: Bool) `shouldBe` True
      (line RB.=~ gold :: Int) `shouldBe` 2
      (line RB.=~ gold :: Text) `shouldBe` "Mira sold 300"
      (line RB.=~ gold :: (Int, Int)) `shouldBe` (11, 13)
      (line RB.=~ gold :: (Text, Text, Text, [Text]))
        `shouldBe` ( "2026-11-21 ", "Mira sold 300"
                   , " gold, Ruby sold 25 gold", ["Mira", "300"] )
      (line RB.=~ gold :: [[Text]])
        `shouldBe` [ ["Mira sold 300", "Mira", "300"]
                   , ["Ruby sold 25", "Ruby", "25"] ]
      RB.getAllTextMatches (line RB.=~ gold) `shouldBe`
        (["Mira sold 300", "Ruby sold 25"] :: [Text])
    -- with no match: the empty answers
    -- 見つからなければ、空の答え
    it "gives the empty answers when nothing is found" $ do
      (line RB.=~ ("dragon" :: String) :: Text) `shouldBe` ""
      (line RB.=~ ("dragon" :: String) :: (Int, Int)) `shouldBe` (-1, 0)
      (line RB.=~~ ("dragon" :: String) :: Maybe Text) `shouldBe` Nothing
    -- places are characters, as in MiniRegex
    -- 場所は文字で数える。MiniRegex と同じ
    it "counts the places in characters" $ do
      let magic = "魔法の 300 gold" :: Text
      (magic RB.=~ ("[0-9]+" :: String) :: (Int, Int))
        `shouldBe` (4, 3)
      (magic RB.=~ ("[0-9]+" :: String) :: (Text, Text, Text))
        `shouldBe` ("魔法の ", "300", " gold")
    -- a group that took no part is (-1, 0)
    -- 使われなかったグループは (-1, 0)
    it "marks a group that took no part" $
      fmap elems (RB.matchOnce (RB.makeRegex ("(a)|(b)" :: String) :: Regex)
                    ("b" :: Text))
        `shouldBe` Just [(0, 1), (-1, 0), (0, 1)]
    -- a String works the same as a Text
    -- String でも Text と同じ
    prop "gives the same with a String" $
      forAll (elements ["\\d+", "(\\w+) sold", "o", "(?i)RUBY", "x*"]) $ \p ->
        (T.unpack line RB.=~ (p :: String) :: [[String]])
          === map (map T.unpack) (line RB.=~ p :: [[Text]])
    -- the same matches as findAll
    -- findAll と同じ一致
    prop "finds what findAll finds" $
      forAll genPattern $ \p -> forAll genText $ \t ->
        case compile p of
          Left _   -> discard
          Right re ->
            map (\a -> a ! 0) (RB.matchAll re t)
              === [ (matchStart m, matchEnd m - matchStart m)
                  | m <- findAll re t ]
    -- a wrong pattern: an error for =~, a fail for =~~
    -- まちがったパターン：=~ なら error、=~~ なら fail
    it "tells a wrong pattern" $ do
      (line RB.=~~ ("[" :: String) :: Maybe Bool) `shouldBe` Nothing
      r <- try (evaluate (line RB.=~ ("[" :: String) :: Bool))
      either (\e -> show (e :: SomeException)) show r
        `shouldContain` "missing terminating ] for character class"
    -- the flags and the limit go in through the options
    -- フラグとステップ数の上限は、設定から入れる
    it "takes the flags and the limit from the options" $ do
      let caseless = RB.makeRegexOpts (RB.CompOption [Caseless])
                       RB.defaultExecOpt ("ruby" :: String) :: Regex
      RB.matchTest caseless line `shouldBe` True
      let tight = RB.setExecOpts (RB.ExecOption 100)
                    (RB.makeRegex ("^(\\w+\\s?)+$" :: String) :: Regex)
      RB.getExecOpts tight `shouldBe` RB.ExecOption 100
      r <- try (evaluate (RB.matchTest tight (fieldOf 5)))
      r `shouldBe` Left (GaveUp 100)
  -- ANCHOR_END: regexbase

  -- ANCHOR: neighbor
  describe "the jobs from the next town / 隣町の仕事" $ do
    -- only the import is changed: the same answers
    -- import だけを変えた。答えは同じ
    prop "gives the same answers with MiniRegex" $
      forAll (elements (T.lines (guildBook 50))) $ \l ->
        let line = T.unpack l
        in  NeighborMini.hasGold line === Neighbor.hasGold line
              .&&. NeighborMini.datesOf line === Neighbor.datesOf line
              .&&. NeighborMini.firstSale line === Neighbor.firstSale line
    it "finds the first sale" $
      NeighborMini.firstSale "2026-11-21 Mira sold 300 gold"
        `shouldBe` ("2026-11-21 ", "Mira sold 300", " gold", ["Mira", "300"])
  -- ANCHOR_END: neighbor

  -- ANCHOR: portable
  describe "the same code with two engines / 同じコードを2つのエンジンで" $ do
    let book = guildBook 300
    -- the jobs give the same answers with regex-tdfa and MiniRegex
    -- 仕事の答えは、regex-tdfa でも MiniRegex でも同じ
    it "gives the same answers with regex-tdfa" $ do
      datesIn @Regex book `shouldBe` datesIn @TDFA.Regex book
      countGold @Regex book `shouldBe` countGold @TDFA.Regex book
      salesIn @Regex book `shouldBe` salesIn @TDFA.Regex book
      length (datesIn @Regex book) `shouldBe` 300
      countGold @Regex book `shouldBe` 152450
    -- but for gold|golden, POSIX takes the longest
    -- ただし gold|golden では、POSIX はいちばん長いものを選ぶ
    it "differs where POSIX takes the longest" $ do
      firstWord @Regex "gold|golden" "a golden scale"
        `shouldBe` Just "gold"
      firstWord @TDFA.Regex "gold|golden" "a golden scale"
        `shouldBe` Just "golden"
  -- ANCHOR_END: portable

  describe "learning from other lands / よその国に学ぶ" $ do
    -- the new starts: a text inside to step back from, and texts only
    -- 新しい始まり：戻る内側の文字列と、文字列だけのパターン
    it "sees a text inside, and a pattern of texts only" $
      map describePrefilter
        [ "\\d{4}-\\d{2}-\\d{2}", "[\\w.+-]+@[\\w-]+\\.[\\w.-]+"
        , "(\\w+) sold (\\d+) (\\w+)", "Mira|Ruby|Lam|Go", "(?i)perl"
        , "[a@]+@b" ]
        `shouldBe`
          [ ("\\d \\d \\d \\d, then \"-\"", "\"-\"")
          , ("[\\w.+\\-]+, then \"@\"", "\".@\"")
          , ("\\w+, then \" sold \"", "\" dlos\"")
          , ("only \"Mira\" \"Ruby\" \"Lam\" \"Go\"", "\"\"")
          , ("only 16 texts: \"PERL\" ... \"perl\"", "\"\"")
          , ("one of [a@]", "\"@b\"") ]
    -- stepping back stops where the search began
    -- 戻るのは、探し始めた場所まで
    it "steps back no further than where the search began" $ do
      triedPlaces "\\w+@" "ab@ cd@" `shouldBe` [0, 4]
      fmap matchText (searchFrom (surely "\\w+@") "ab@" 1)
        `shouldBe` Just "b@"
      fmap matchText (searchFrom (surely "\\d{4}-") "12345-" 0)
        `shouldBe` Just "2345-"
    -- texts only: the first branch there wins, as with the VM
    -- 文字列だけ：VM と同じく、そこにある最初の枝が勝つ
    it "takes the first branch that is there" $ do
      map matchText (findAll (surely "Go|Gold") "Gold Go")
        `shouldBe` ["Go", "Go"]
      map matchText (findAll (surely "Gold|Go") "Gold Go")
        `shouldBe` ["Gold", "Go"]
      map matchText (findAll (surely "(?i)k") "kK\x212A")
        `shouldBe` ["k", "K", "\x212A"]
    -- the same answers as trying every place, with the VM
    -- 全部の場所を VM で試したときと同じ答え
    prop "finds what trying every place with the VM finds" $
      forAll (elements lessonPatterns) $ \p ->
        forAll (listOf (elements lessonChars)) $ \cs ->
        forAll (chooseInt (0, length cs)) $ \from ->
          let re = surely p
              t  = T.pack cs
          in  counterexample (T.unpack p) $
                (findAll re t === findAll (everywhere re) t)
                .&&. (searchWithin 20000 re t from
                        === searchWithin 20000 (everywhere re) t from)
    -- the guild's book: the same counts as before
    -- ギルドの帳面：前と同じ件数
    it "counts the same in the guild's book" $ do
      let book  = guildBook 2000
          count = length . flip findAll book
      [ count re == count (everywhere re)
        | p <- ["Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go", "(?i)perl"]
        , let re = surely p ]
        `shouldBe` [True, True]
  describe "the lazy DFA / lazy DFA" $ do
    -- what it takes, and what it leaves to the VM
    -- 引き受けるものと、VM にまかせるもの
    it "runs only what it can run" $
      map (canDfa . program . surely)
        [ "\\d{4}-\\d{2}-\\d{2}", "[\\w.+-]+@[\\w-]+\\.[\\w.-]+"
        , "Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go", "(?i)perl"
        , "(\\w+) sold (\\d+) (\\w+)", "\\bgold\\b", "(a)\\1"
        , "a(?=b)", "a++a", "x*" ]
        `shouldBe` replicate 5 True ++ replicate 5 False
    -- the pattern backwards
    -- 逆向きのパターン
    it "turns a pattern backwards" $
      render (reversePattern (tree (surely "ab(c|de)+f")))
        `shouldBe` "f(c|ed)+ba"
    -- the same matches as the VM, on random patterns and texts
    -- でたらめなパターンと文字列で、VM と同じ一致
    prop "finds all that the VM finds" $
      forAll genTreeDfa $ \tr ->
        forAll genLines $ \t ->
          case compile (render tr) of
            Left _   -> property True
            Right re -> counterexample (T.unpack (render tr)) $
              classify (isJust (dfaFor re)) "by the DFA" $
                findAll re t === findAllVM re t
    prop "finds the first that the VM finds" $
      forAll genTreeDfa $ \tr ->
        forAll genLines $ \t ->
        forAll (chooseInt (0, T.length t)) $ \from ->
          case compile (render tr) of
            Left _   -> property True
            Right re -> counterexample (T.unpack (render tr)) $
              fromMaybe (searchFrom re t from) (dfaFind re t from)
                === searchFrom (everywhere re) t from
    prop "finds the guild's jobs as the VM does" $
      forAll (elements lessonPatterns) $ \p ->
        forAll (listOf (elements lessonChars)) $ \cs ->
          let re = surely p
              t  = T.pack cs
          in  counterexample (T.unpack p) (findAll re t === findAllVM re t)
  -- ANCHOR: answers
  describe "the training / トレーニング" $ do
    -- step 2: in a ByteString, the places are bytes
    -- ステップ2：ByteString の中では、場所はバイト
    it "counts the places in bytes in a ByteString" $ do
      let bs = encodeUtf8 "魔法の 300 gold"
          re = RB.makeRegex ("[0-9]+" :: String) :: Regex
      (bs RB.=~ ("[0-9]+" :: String) :: (Int, Int)) `shouldBe` (10, 3)
      (bs RB.=~ ("[0-9]+" :: String) :: B.ByteString) `shouldBe` "300"
      fmap (\(_, m, _) -> fst (m ! 0)) (RB.matchOnceText re bs)
        `shouldBe` Just "300"
    -- step 3: the answers by name, with both engines
    -- ステップ3：名前で選ぶ答えを、2つのエンジンで
    it "chooses the answers by name" $ do
      let line = "2026-11-21 Mira sold 300 gold, Ruby sold 25 gold"
      matches @Regex "[0-9]+ gold" line `shouldBe` True
      firstMatch @Regex "[0-9]+ gold" line `shouldBe` Just "300 gold"
      allMatches @Regex "[0-9]+ gold" line `shouldBe` ["300 gold", "25 gold"]
      countMatches @Regex "gold" line `shouldBe` 2
      allMatches @TDFA.Regex "[0-9]+ gold" line
        `shouldBe` allMatches @Regex "[0-9]+ gold" line
  -- ANCHOR_END: answers
