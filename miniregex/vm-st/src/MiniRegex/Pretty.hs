-- ANCHOR: all
-- | Show a tree: draw it for people to read, or write it back as a
--   pattern that reads into the same tree.
-- 木を見せる：人が読むために描く。または、同じ木に読めるパターンの
-- 文字列に書き戻す
module MiniRegex.Pretty
  ( render
  , renderQuantifier
  , drawPattern
  ) where

-- ANCHOR: imports
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Char (isPrint)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))

import MiniRegex.Parser (Flag (..))
import MiniRegex.Syntax
import MiniRegex.Unicode (caseVariants)
-- ANCHOR_END: imports

-- ANCHOR: render
-- ✍️ Write a tree back as a pattern. Signs that have a meaning get a
--    \ before them, so that parsing the answer gives the same tree.
--    The flags are in the tree, so writing it back keeps track of the
--    flags in effect, the way the parser does, and writes (?i), (?-i)
--    and the like just before an atom that needs a change.
-- 木を、パターンの文字列に書き戻す。意味のある記号には \ を付けるので、
-- 書き戻したものを読むと、同じ木になる。フラグは木の中にあるので、
-- パーサーと同じように、いま効いているフラグを覚えながら書き、変える
-- 要るアトムのすぐ前に (?i)・(?-i) などを書く
render :: Pattern -> Text
render p = evalState (pattern p) Set.empty

type Writer = State (Set Flag)

pattern :: Pattern -> Writer Text
pattern (Pattern branches) =
  T.intercalate "|" <$> traverse branch (NE.toList branches)

branch :: Branch -> Writer Text
branch (Branch ps) = T.concat <$> traverse piece ps

piece :: Piece -> Writer Text
piece (Anchor a)       = anchor a
piece KeepOut          = pure "\\K"
piece (Quantified a q) = (<> renderQuantifier q) <$> atom a

anchor :: Anchor -> Writer Text
anchor a = case a of
  LineStart   -> (<> "^") <$> need [(M, True)]
  LineEnd     -> (<> "$") <$> need [(M, True)]
  TextStart   -> (<> "^") <$> need [(M, False)]
  TextEnd     -> pure "\\z"
  LastLineEnd -> (<> "$") <$> need [(M, False)]
  WordBoundary r    -> (<> "\\b") <$> need [(AW, r == AsciiOnly)]
  NotWordBoundary r -> (<> "\\B") <$> need [(AW, r == AsciiOnly)]

atom :: Atom -> Writer Text
atom a = case a of
  Lit c                    -> (<> escapeOutside c) <$> need (caseOf False c)
  NoCase (Lit c)           -> (<> escapeOutside c) <$> need (caseOf True c)
  AnyChar                  -> (<> ".") <$> need [(S, False)]
  Anything                 -> (<> ".") <$> need [(S, True)]
  Class cls                -> classWith False cls
  NoCase (Class cls)       -> classWith True cls
  Backref n                -> (<> backref n) <$> need [(I, False)]
  NoCase (Backref n)       -> (<> backref n) <$> need [(I, True)]
  NoCase other             -> atom other
  Group kind inner         -> inParens (open kind) inner
  Atomic inner             -> inParens "(?>" inner
  Look d sense inner       -> inParens (look d sense) inner
  where
    inParens opening inner = do
      saved <- get
      body <- pattern inner
      put saved   -- the flags go back at the ), as in the parser
      pure (opening <> body <> ")")
    look Ahead  Positive = "(?="
    look Ahead  Negative = "(?!"
    look Behind Positive = "(?<="
    look Behind Negative = "(?<!"
    -- a letter with another case needs the flag i set as it was
    -- ほかの形のある文字は、フラグ i が元のとおりであることが要る
    caseOf want c
      | length (caseVariants c) > 1 = [(I, want)]
      | otherwise                   = []
    classWith noCase cls = do
      changes <- need ((I, noCase) : concatMap itemNeeds (items cls))
      pure (changes <> renderClass cls)
    -- ANCHOR: renderref
    -- \g{n}, not \n: \12 could be read as octal, and a digit after
    -- \1 would join it
    -- \n ではなく \g{n}：\12 は8進と読まれうるし、\1 のうしろの
    -- 数字は、くっついてしまう
    backref n = "\\g{" <> T.pack (show n) <> "}"
    -- ANCHOR_END: renderref
    open Numbered     = "("
    open (Named n)    = "(?<" <> n <> ">"
    open NonCapturing = "(?:"
    open BranchReset  = "(?|"

-- 🚩 What flags an item of a class needs: \d needs (?aD) on or off ...
-- 文字の集まりの項目に要るフラグ：\d なら (?aD) が付いているか外れているか
itemNeeds :: ClassItem -> [(Flag, Bool)]
itemNeeds (Builtin _ t) = case t of
  Digit r   -> [(AD, r == AsciiOnly)]
  WhiteSpace r -> [(AS, r == AsciiOnly)]
  Word r    -> [(AW, r == AsciiOnly)]
  Posix AsciiOnly n
    | n `elem` ["digit", "xdigit"] -> [(AT, True)]
    | otherwise                    -> [(AP, True)]
  Posix AllUnicode n
    | n `elem` ["digit", "xdigit"] -> [(AP, False), (AT, False)]
    | otherwise                    -> [(AP, False)]
  _ -> []
itemNeeds _ = []

-- ANCHOR: need
-- 🚩 Make the flags what the atom needs: write (?i-aD) for the ones that
--    are not, and remember them. (?aP) sets aT too, and (?-aP) takes
--    both away, as in the parser.
-- アトムに要るフラグにする。ちがうものを (?i-aD) のように書いて、覚える。
-- パーサーと同じく、(?aP) は aT も付け、(?-aP) は両方を外す
need :: [(Flag, Bool)] -> Writer Text
need wants = do
  now <- get
  let changes = [ (f, w) | (f, w) <- wants, Set.member f now /= w ]
      ons  = [ f | (f, True)  <- changes ]
      offs = [ f | (f, False) <- changes ]
      both f = if f == AP then [AP, AT] else [f]
      now' = (now `Set.union` Set.fromList (concatMap both ons))
               `Set.difference` Set.fromList (concatMap both offs)
  put now'
  pure $ if null changes
    then ""
    else "(?" <> T.concat (map letter ons)
           <> (if null offs then "" else "-" <> T.concat (map letter offs))
           <> ")"
  where
    letter f = case f of
      I  -> "i"
      M  -> "m"
      S  -> "s"
      AD -> "aD"
      AS -> "aS"
      AW -> "aW"
      AP -> "aP"
      AT -> "aT"
      _  -> ""
-- ANCHOR_END: need

-- 🔁 The quantifier as written: "", *, +, ?, {n}, {n,}, {n,m}, and
--    then ? or + for Lazy and Possessive. Once, but not greedy, has to
--    be written as {1}? (a? would mean "zero or one time").
-- 量指定子の書き方：""・*・+・?・{n}・{n,}・{n,m}、そのあとに控えめなら ?、
-- 所有的なら +。1回で欲ばりでないものは {1}? と書く（a? と書くと
-- 「0回か1回」になってしまう）
renderQuantifier :: Quantifier -> Text
renderQuantifier (Quantifier c g) = base <> suffix
  where
    base = case c of
      Count 1 (Just 0) | g == Greedy -> ""
      Count 0 Nothing                -> "*"
      Count 1 Nothing                -> "+"
      Count 0 (Just 1)               -> "?"
      Count n (Just 0)               -> "{" <> num n <> "}"
      Count n Nothing                -> "{" <> num n <> ",}"
      Count n (Just e)               -> "{" <> num n <> ","
                                          <> num (n + e) <> "}"
    suffix = case g of
      Greedy     -> ""
      Lazy       -> "?"
      Possessive -> "+"
    num = T.pack . show

-- 🔤 [ ... ]. In a class, only \ ] ^ - and [ need a \ (and : . = at
--    the head). A class of just \d (or \p{Han} ...) is written alone.
-- [ ... ]。[ ] の中で \ が要るのは、\ ] ^ - [ だけ（頭なら : . = も）。
-- \d（や \p{Han} など）だけの集まりは、それだけで書く
renderClass :: CharClass -> Text
renderClass (CharClass False (Builtin o t :| []))
  | not (isPosix t) = builtin o t
  where
    isPosix (Posix _ _) = True
    isPosix _           = False
renderClass (CharClass neg (i :| is)) = "[" <> hat <> body <> "]"
  where
    hat  = if neg then "^" else ""
    body = item firstOne i <> T.concat (map (item inside) is)
    item f (Single c)    = f c
    item f (Range a b)   = f a <> "-" <> inside b
    item _ (Builtin o t) = builtin o t
    inside c
      | c `elem` ("\\]^-[" :: String) = T.pack ['\\', c]
      | otherwise                    = T.singleton c
    -- [:x:] at the head would read as a POSIX name; \: never does
    -- 頭の [:x:] は POSIX の名前と読まれる。\: なら、そうならない
    firstOne c
      | c `elem` (":.=" :: String) && not neg = T.pack ['\\', c]
      | otherwise                              = inside c

-- 🧺 \d \D \w ... [:alpha:] [:^alpha:] \p{Han} \P{Han}.
-- \d \D \w など、[:alpha:] [:^alpha:] \p{Han} \P{Han}
builtin :: Bool -> CharType -> Text
builtin o t = case t of
  Digit _   -> letter 'd'
  WhiteSpace _ -> letter 's'
  Word _    -> letter 'w'
  HSpace    -> letter 'h'
  VSpace    -> letter 'v'
  Posix _ n -> "[:" <> (if o then "^" else "") <> n <> ":]"
  Property p -> T.pack ['\\', if o then 'P' else 'p'] <> "{" <> prop p <> "}"
  where
    letter c = T.pack ['\\', if o then toUpperAscii c else c]
    toUpperAscii c = toEnum (fromEnum c - 32)
    prop (Category n)  = n
    prop (Script s)    = "sc:" <> T.pack (show s)
    prop (ScriptExt s) = T.pack (show s)
    prop AnyCode       = "Any"
    prop (Special n)   = n

-- 🛡️ A character outside [ ], with a \ when it has a meaning.
-- [ ] の外の文字。意味のある記号なら \ を付ける
escapeOutside :: Char -> Text
escapeOutside c
  | c `elem` ("\\^$.|?*+()[{" :: String) = T.pack ['\\', c]
  | otherwise                           = T.singleton c
-- ANCHOR_END: render

-- ANCHOR: draw
-- 🌳 Draw a tree with lines, one piece on a line.
-- 木を線で描く。1行に部品1つ
drawPattern :: Pattern -> Text
drawPattern = T.unlines . draw . patternTree

-- 🌱 The tree as a Data.Tree of labels.
-- 木を、ラベルの Data.Tree にする
patternTree :: Pattern -> Tree Text
patternTree (Pattern (b :| [])) = branchTree b
patternTree (Pattern bs)        = Node "or" (map branchTree (NE.toList bs))

branchTree :: Branch -> Tree Text
branchTree (Branch ps) = Node "branch" (map pieceTree ps)

pieceTree :: Piece -> Tree Text
pieceTree KeepOut          = Node "\\K the match starts here" []
pieceTree (Anchor a)       = Node (anchorLabel a) []
  where
    anchorLabel LineStart   = "^ line start (m)"
    anchorLabel LineEnd     = "$ line end (m)"
    anchorLabel TextStart   = "^ text start"
    anchorLabel TextEnd     = "\\z text end"
    anchorLabel LastLineEnd = "$ text end (or before a last \\n)"
    anchorLabel (WordBoundary _)    = "\\b word boundary"
    anchorLabel (NotWordBoundary _) = "\\B not a word boundary"
pieceTree (Quantified a q) = case a of
  Group kind inner -> Node ("group " <> groupName kind <> times)
                           [patternTree inner]
  Atomic inner     -> Node ("atomic (?>)" <> times) [patternTree inner]
  Look d s inner   -> Node (lookName d s <> times) [patternTree inner]
  _                -> Node (label a <> times) []
  where
    times = case renderQuantifier q of
      "" -> ""
      t  -> "  " <> t
    label (Lit c)
      | isPrint c     = T.pack ['\'', c, '\'']
      | otherwise     = T.pack (show c)  -- '\t', '\n' ...
    label AnyChar     = ". any but \\n"
    label Anything    = ". any (s)"
    label (Class cls) = renderClass cls
    label (Group _ _) = "group"
    label (Atomic _)  = "atomic"
    label (Look _ _ _) = "look"
    label (Backref n) = "\\" <> T.pack (show n) <> " same as group "
                          <> T.pack (show n)
    label (NoCase x)  = "(?i) " <> label x
    groupName Numbered     = "(numbered)"
    groupName (Named n)    = "<" <> n <> ">"
    groupName NonCapturing = "(?:)"
    groupName BranchReset  = "(?|)"
    lookName Ahead  Positive = "lookahead (?=)"
    lookName Ahead  Negative = "negative lookahead (?!)"
    lookName Behind Positive = "lookbehind (?<=)"
    lookName Behind Negative = "negative lookbehind (?<!)"

-- ✏️ The lines of a tree: ├─ for a child with more after it, └─ for
--    the last one.
-- 木の行。あとに続く子には ├─、最後の子には └─
draw :: Tree Text -> [Text]
draw (Node top kids) = top : concat (zipWith child [1 :: Int ..] kids)
  where
    n = length kids
    child i kid =
      let (mark, more) = if i == n then ("└─ ", "   ") else ("├─ ", "│  ")
      in  case draw kid of
            first : rest -> (mark <> first) : map (more <>) rest
            []           -> []
-- ANCHOR_END: draw
-- ANCHOR_END: all
