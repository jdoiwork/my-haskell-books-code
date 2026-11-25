-- ANCHOR: all
-- | The compiler: from the tree of a pattern to a program for the VM.
--   It walks the tree the way the continuation matcher does, but
--   instead of running each part, it writes down instructions. Labels
--   are names at first; `assemble` turns them into addresses.
-- コンパイラー：パターンの木から、VM のプログラムへ。継続のマッチャーと
-- 同じように木を歩くが、部分を動かすかわりに、命令を書き出す。ラベルは
-- はじめは名前で、assemble が番地に変える
module MiniRegex.Compile
  ( compileTree
  , programSize
  , programLimit
  , compilePlain
  , possess
  , apart
  , nextAtoms
  , writeOut
  , Line (..)
  , assemble
  , listing
  , showInst
  ) where

-- ANCHOR: imports
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Control.Monad.State.Strict (State, evalState, gets, state)
import Data.Array ((!))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Continuation (farthestFirst, numbered)
import MiniRegex.Groups (branchGroups, calls, groupBody, pieceGroups)
import MiniRegex.Pretty (render)
import MiniRegex.Rules (fitsChar)
import MiniRegex.Unicode (caseVariants)
import MiniRegex.Program
import MiniRegex.Rules (lookCount)
import MiniRegex.Syntax
import MiniRegex.Width (Width (..), atomWidth, branchWidth)
-- ANCHOR_END: imports

-- ANCHOR: line
-- 📝 A line of the program, before it has addresses: a place with a
--    name, or an instruction whose labels are names.
-- 番地を振る前の、プログラムの1行：名前の付いた場所か、ラベルが名前の
-- 命令
data Line
  = Here Name
  | Do (Inst Name)
  deriving (Show, Eq)

-- 🏷️ The name of a place.
-- 場所の名前
type Name = Int

-- 🧮 While writing: the next name and the next mark to hand out, and
--    the names of the places where the called groups' code starts.
-- 書いているあいだ：次に渡す名前と、次に渡す印。呼ばれるグループの
-- コードが始まる場所の名前
data Counters = Counters
  { nextName :: !Name
  , nextMark :: !Mark
  , callees  :: !(IntMap Name)
  }

type Asm = State Counters

newName :: Asm Name
newName = state (\s -> (nextName s, s { nextName = nextName s + 1 }))

newMark :: Asm Mark
newMark = state (\s -> (nextMark s, s { nextMark = nextMark s + 1 }))
-- ANCHOR_END: line

-- ANCHOR: assemble
-- 📍 Give each line an address, then turn each name into the address
--    of its place: fmap goes through every label of an instruction.
-- 行ごとに番地を振り、名前を、その場所の番地に置き換える。fmap が、
-- 命令の中のラベルを全部たどる
assemble :: [Line] -> Program
assemble ls = programOf [fmap addressOf i | Do i <- ls]
  where
    places = IntMap.fromList (go 0 ls)
    go _ []              = []
    go n (Here x : rest) = (x, n) : go n rest
    go n (Do _ : rest)   = go (n + 1) rest
    addressOf x = places IntMap.! x
-- ANCHOR_END: assemble

-- ANCHOR: compile
-- 🏗️ The whole pattern, and Match at the end. Then, for each group
--    that is called, its code once more, as a subroutine: the inside of
--    the group (its own Open and Close are not there: a call does not
--    set the group), and Return.
-- パターン全体と、最後に Match。そのあと、呼ばれるグループごとに、その
-- コードをもう一度、サブルーチンとして書く：グループの中身（グループ
-- 自身の Open と Close は書かない。呼び出しは、そのグループを書き換え
-- ない）と、Return
compileTree :: Pattern -> Program
compileTree p = assemble (possess (writeOut p))

-- 🐢 The same, without the possessive loops (as the chapters before).
-- 同じもの。所有的な輪にしない（前の章までと同じ）
compilePlain :: Pattern -> Program
compilePlain p = assemble (writeOut p)

-- ✍️ The lines of the program, with names for labels.
-- プログラムの行。ラベルは名前のまま
writeOut :: Pattern -> [Line]
writeOut p = evalState whole start
  where
    called = calls p
    start = Counters (length called) 0
              (IntMap.fromList (zip called [0 ..]))
    whole = do
      body <- pattern 1 p
      subs <- mapM subroutine called
      pure (body ++ [Do Match] ++ concat subs)
    subroutine g = do
      named <- gets ((IntMap.! g) . callees)
      code <- case groupBody g p of
        Just inner -> pattern (g + 1) inner
        Nothing    -> pure []  -- (not reached / 来ない)
      pure ([Here named] ++ code ++ [Do Return])

-- 🌳 The branches, tried from the left. The groups are numbered as in
--    the matcher: `n` is the number of the next group to open.
-- 枝を左から試す。グループの番号は、マッチャーと同じ付け方。n は、
-- 次に開くグループの番号
pattern :: Int -> Pattern -> Asm [Line]
pattern n (Pattern bs) =
  alternatives [branch m b | (m, b) <- numbered n branchGroups (NE.toList bs)]

-- 🪵 The pieces, one after another: just put the code side by side.
-- 部品を順に。コードを並べるだけ
branch :: Int -> Branch -> Asm [Line]
branch n (Branch ps) =
  concat <$> mapM (uncurry piece) (numbered n pieceGroups ps)
-- ANCHOR_END: compile

-- ANCHOR: alternatives
-- 🔀 a|b|c: Split to the first; if it fails, Split again to the next.
--    Each one jumps to the end when it is done.
-- a|b|c：1つ目へ Split する。だめなら、次へまた Split。どれも、
-- 終わったら最後へ飛ぶ
alternatives :: [Asm [Line]] -> Asm [Line]
alternatives parts = do
  end <- newName
  code <- go end parts
  pure (code ++ [Here end])
  where
    go _   []         = pure []
    go _   [one]      = one
    go end (one : others) = do
      this  <- newName
      other <- newName
      a <- one
      b <- go end others
      pure ([Do (Split this other), Here this] ++ a
            ++ [Do (Jmp end), Here other] ++ b)
-- ANCHOR_END: alternatives

-- ANCHOR: piece
-- 🧩 A piece. a*+ is (?>a*): the greedy repeat inside a fence. `eats`:
--    does the atom always take at least one character?
-- 部品。a*+ は (?>a*)：欲ばりのくり返しを、区切りの中で。eats は、
-- アトムがいつも1文字以上食べるか
piece :: Int -> Piece -> Asm [Line]
piece _ (Anchor a) = pure [Do (Assert a)]
piece _ KeepOut    = pure [Do Keep]
piece n (Quantified a written)
  | greed q == Possessive =
      atomic (repeatAtom eats q { greed = Greedy } (atom n a))
  | otherwise = repeatAtom eats q (atom n a)
  where
    eats = shortest (atomWidth a) >= 1
    -- (?=…)+ goes round once more at most: lookCount in Rules
    -- (?=…)+ は、多くても、もう1回だけ回る（Rules の lookCount）
    q = lookCount a written
-- ANCHOR_END: piece

-- ANCHOR: atom
-- ⚛️ An atom. `atom n a` is an action, not code: a repeat runs it once
--    for each copy, so that each copy has names and marks of its own.
-- アトム。atom n a はコードではなく、コードを書く動作。くり返しは、
-- 写しごとに動かすので、写しごとに別の名前と印が付く
atom :: Int -> Atom -> Asm [Line]
atom n (Group NonCapturing inner) = pattern n inner
atom n (Group BranchReset (Pattern bs)) =
  alternatives [branch n b | b <- NE.toList bs]
atom n (Group _ inner) = do
  body <- pattern (n + 1) inner
  pure ([Do (Open n)] ++ body ++ [Do (Close n)])
atom _ (Backref r)          = pure [Do (Ref r)]
atom _ (NoCase (Backref r)) = pure [Do (RefNoCase r)]
atom n (Atomic inner)       = atomic (pattern n inner)
atom n (Look Ahead sense inner) = looking sense (pattern n inner)
atom n (Look Behind sense inner) = do
  m <- newMark
  body <- looking sense (lookBehind n m inner)
  pure (Do (SetMark m) : body)
-- ANCHOR: atomcall
-- (?1): Gosub to where group 1's subroutine starts
-- (?1)：1番のグループのサブルーチンが始まる場所へ Gosub
atom _ (Call g) = do
  named <- gets ((IntMap.! g) . callees)
  pure [Do (Gosub g named)]
-- ANCHOR_END: atomcall
atom _ a = pure [Do (Take a)]
-- ANCHOR_END: atom

-- ANCHOR: fences
-- 🔒 (?>...): a fence, the inside, and Cut.
-- (?>...)：区切り、中身、Cut
atomic :: Asm [Line] -> Asm [Line]
atomic inside = do
  body <- inside
  pure ([Do Fence] ++ body ++ [Do Cut])

-- 👀 A look: a positive one goes back to where it started; a negative
--    one fails when the inside works, and goes on after it when the
--    inside fails.
-- 「見る」もの：肯定は、始めた場所へ戻って先へ。否定は、中がうまく
-- いったら失敗、だめなら、そのうしろから先へ
looking :: Sense -> Asm [Line] -> Asm [Line]
looking Positive inside = do
  body <- inside
  pure ([Do Fence] ++ body ++ [Do CutBack])
looking Negative inside = do
  after <- newName
  body <- inside
  pure ([Do (FenceNot after)] ++ body ++ [Do CutFail, Here after])
-- ANCHOR_END: fences

-- ANCHOR: behind
-- 🔙 The inside of a lookbehind: for each branch, for each way back
--    (the farthest first), step back, run the branch, and check that
--    it ended at mark m, where the lookbehind started.
-- 後読みの中身：枝ごとに、下がり方ごとに（遠いほうから）、下がって、
-- 枝を動かし、後読みを始めた場所（印 m）で終わったかを確かめる
lookBehind :: Int -> Mark -> Pattern -> Asm [Line]
lookBehind n m (Pattern bs) = alternatives
  [ do code <- branch k b
       pure ([Do (Back d)] ++ code ++ [Do (AtMark m)])
  | (k, b) <- numbered n branchGroups (NE.toList bs)
  , d <- farthestFirst (branchWidth b) ]
-- ANCHOR_END: behind

-- ANCHOR: size
-- 📐 About how many instructions the program will have, counted from
--    the tree before it is made. A repeat writes its atom out once for
--    each time (a{100} is 100 copies), and a lookbehind once for each
--    distance, so nested counts multiply: (?:a{65535}){65535} would be
--    about 4.3 billion. The count is a little more than the real one,
--    never less.
-- プログラムの命令が、だいたいいくつになるか。作る前に、木から数える。
-- くり返しはアトムを回数ぶん書き出し（a{100} は 100 個の写し）、後読みは
-- 下がる距離ごとに書き出すので、入れ子の回数はかけ算になる：
-- (?:a{65535}){65535} は約 43 億。本物より少し多めに数え、少なくはしない
programSize :: Pattern -> Integer
programSize whole =
  -- the pattern, and each called group's body once more, with Return
  -- (compileTree writes them after Match)
  -- パターンと、呼ばれるグループの中身をもう1度ずつ、Return と一緒に
  -- （compileTree が Match のうしろに書く）
  patternSize whole
    + sum [ patternSize body + 2
          | g <- calls whole, Just body <- [groupBody g whole] ]

patternSize :: Pattern -> Integer
patternSize (Pattern bs) =
  sum (map branchSize (NE.toList bs)) + 3 * fromIntegral (length bs)
  where
    branchSize (Branch ps) = sum (map pieceSize ps)
    pieceSize (Quantified a written) =
      let Quantifier (Count least more) g = lookCount a written
          copies = fromIntegral least + maybe 1 fromIntegral more
          -- a*+ is fenced: two more (the fence and the cut)
          -- a*+ は区切りの中：2つ多い（区切りと、切り捨て）
          fence = if g == Possessive then 2 else 0
      in  (atomSize a + 2) * max 1 copies + 2 + fence
    pieceSize _ = 1
    atomSize a = case a of
      Group _ p          -> patternSize p + 2
      Atomic p           -> patternSize p + 3
      NoCase x           -> atomSize x
      Look Ahead _ p     -> patternSize p + 4
      Look Behind _ p    ->
        sum [ (patternSize (Pattern (b NE.:| [])) + 3) * distances b
            | b <- let Pattern xs = p in NE.toList xs ] + 4
      _                  -> 1
    distances b = fromIntegral (length (farthestFirst (branchWidth b)))

-- 🧱 The most instructions MiniRegex makes a program of. A pattern
--    that needs more is "not yet" (PCRE2 says error 120, "regular
--    expression is too large", to the biggest of them; its program is
--    made another way, so the line is not in the same place).
-- MiniRegex がプログラムにする命令の数の上限。これより多く要るパターンは
-- 「まだ」（いちばん大きなものには、PCRE2 もエラー 120「regular
-- expression is too large」を言う。プログラムの作り方がちがうので、
-- 線の位置は同じではない）
programLimit :: Integer
programLimit = 1000000
-- ANCHOR_END: size

-- ANCHOR: repeat
-- 🔁 A repeat, the way the continuation matcher does it. The copies
--    that must be are put one after another; then either a loop (no
--    upper end) or the copies that may be (an upper end). `eats` says
--    the body always takes a character.
-- くり返し。継続のマッチャーと同じやり方。「必ず」の写しを並べてから、
-- 上の端がなければループ、あれば「あってもよい」写しを並べる。eats は、
-- 中身がいつも1文字以上食べること
repeatAtom :: Bool -> Quantifier -> Asm [Line] -> Asm [Line]
repeatAtom eats (Quantifier (Count least more) greed) one = case more of
  -- a*, a+, a{2,}: the last copy that must be is the loop's first time
  -- a*・a+・a{2,}：「必ず」の最後の写しが、ループの1回目
  Nothing
    | least == 0 -> do
        top <- newName
        out <- newName
        body <- loop top out
        pure ([Do (choose top out)] ++ body ++ [Here out])
    | otherwise -> do
        musts <- replicateM (fromIntegral least - 1) one
        top <- newName
        out <- newName
        body <- loop top out
        pure (concat musts ++ body ++ [Here out])
  -- a?, a{2,5}: the ones that must be, and then up to k more
  -- a?・a{2,5}：必ずのものと、あと k 回まで
  Just k -> do
    musts <- replicateM (fromIntegral least) one
    out <- newName
    mays <- maybes out (fromIntegral k)
    pure (concat musts ++ mays ++ [Here out])
  where
    -- Greedy tries one more time first; lazy tries the rest first.
    -- 欲ばりは「もう1回」を先に、控えめは「残り」を先に
    choose again rest = case greed of
      Lazy -> Split rest again
      _    -> Split again rest
    -- One time round. If the body always takes a character, it is
    -- just the body: a round never takes nothing. If not, mark where it
    -- starts, and do not go round again when it took nothing.
    -- 1回分。中身がいつも1文字以上食べるなら、中身だけ：何も食べない回は
    -- ない。そうでなければ、始めた場所に印を付けて、何も食べなかったら、
    -- もう回らない
    loop top out
      | eats = do
          body <- one
          pure ([Here top] ++ body ++ [Do (choose top out)])
      | otherwise = do
          m <- newMark
          body <- one
          pure ([Here top, Do (SetMark m)] ++ body
                ++ [Do (IfStill m out), Do (choose top out)])
    -- k more, each one may be: each says "this one, or out".
    -- あと k 回。どれも「これか、外へ」
    maybes :: Name -> Int -> Asm [Line]
    maybes _   0 = pure []
    maybes out j = do
      this <- newName
      body <- one
      rest <- maybes out (j - 1)
      pure ([Do (choose this out), Here this] ++ body ++ rest)
-- ANCHOR_END: repeat

-- ANCHOR: possess
-- 🔒 Make the loops of one character possessive where giving back
--    could not help: a* (and the loop of a+) whose next thing never
--    fits a character that a fits. Then the loop is one instruction,
--    takeall, and leaves no ways on the stack. PCRE2 does the same
--    (auto-possessification).
-- 返しても役に立たない所で、1文字の輪を所有的にする：a*（と a+ の輪）の
-- 次に来るものが、a に合う文字に決して合わないとき。輪は1つの命令
-- takeall になり、スタックに道を残さない。PCRE2 も同じことをする
-- （自動の所有化）
possess :: [Line] -> [Line]
possess ls = go ls
  where
    prog = assemble ls
    -- the address of each name
    -- 名前ごとの番地
    places = IntMap.fromList (addresses 0 ls)
    addresses _ []              = []
    addresses n (Here x : more) = (x, n) : addresses n more
    addresses n (Do _ : more)   = addresses (n + 1) more

    -- a*: split top, out / top: take a / split top, out / out:
    go (Do (Split t o) : Here t' : Do (Take a) : Do (Split t2 o2)
        : Here o' : more)
      | all (== t) [t', t2], all (== o) [o2, o'], safe a o =
          Here t : Do (TakeAll a) : Here o : go more
    -- the loop of a+: top: take a / split top, out / out:
    go (Here t : Do (Take a) : Do (Split t' o) : Here o' : more)
      | t == t', o == o', safe a o =
          Here t : Do (Take a) : Do (TakeAll a) : Here o : go more
    go (l : more) = l : go more
    go []         = []

    -- every first character after `out` is apart from the atom
    -- out のうしろの、どの最初の文字も、アトムと重ならない
    safe a out = case nextAtoms prog (places IntMap.! out) of
      Just bs -> all (apart a) bs
      Nothing -> False

-- 👣 What can come next from an address: the atoms of the first takes,
--    through the instructions that take nothing and change nothing that
--    giving back would change. A match is all right (giving back would
--    only make the match shorter, and the longest is tried first). Any
--    other instruction (assert, ref, mark, fence, back, call): we cannot
--    tell (Nothing).
-- ある番地から、次に来うるもの：最初の take のアトム。文字を食べず、
-- 返すことで変わるものも変えない命令は、通りぬける。match はよい（返すと、
-- 一致が短くなるだけで、いちばん長いものが先に試される）。それ以外の命令
-- （assert・ref・印・区切り・back・呼び出し）は、わからない（Nothing）
nextAtoms :: Program -> Int -> Maybe [Atom]
nextAtoms prog start = walk [start] IntSet.empty []
  where
    walk [] _ found = Just found
    walk (pc : todo) seen found
      | IntSet.member pc seen = walk todo seen found
      | otherwise = case prog ! pc of
          Take b    -> walk todo seen' (b : found)
          Match     -> walk todo seen' found
          Split a b -> walk (a : b : todo) seen' found
          Jmp a     -> walk (a : todo) seen' found
          Open _    -> walk (pc + 1 : todo) seen' found
          Close _   -> walk (pc + 1 : todo) seen' found
          _         -> Nothing
      where
        seen' = IntSet.insert pc seen

-- ↔️ Can no character fit both atoms? Only told for sure when one of
--    them is one character (or one character and its other cases).
-- 2つのアトムの両方に合う文字は、ないか。確かに言えるのは、片方が1文字
-- （か、1文字と、その大文字・小文字）のときだけ
apart :: Atom -> Atom -> Bool
apart a b = case (charsOf a, charsOf b) of
  (Just xs, _) -> not (any (fitsChar b) xs)
  (_, Just ys) -> not (any (fitsChar a) ys)
  _            -> False
  where
    charsOf (Lit x)          = Just [x]
    charsOf (NoCase (Lit x)) = Just (caseVariants x)
    charsOf _                = Nothing
-- ANCHOR_END: possess

-- ANCHOR: listing
-- 📋 The program, one instruction to a line, with its address.
-- プログラムを、1行に1命令、番地つきで
listing :: Program -> Text
listing prog = T.unlines (zipWith line [0 :: Int ..] (instructions prog))
  where
    line n i = T.justifyRight 3 ' ' (T.pack (show n)) <> "  " <> showInst i

-- 🔤 One instruction, the way people read it.
-- 命令1つを、人が読む形で
showInst :: Inst Int -> Text
showInst i = case i of
  Take a      -> "take " <> quoted (render (alone (Quantified a once)))
  Assert a    -> "assert " <> render (alone (Anchor a))
  Split a b   -> "split " <> num a <> ", " <> num b
  Jmp a       -> "jmp " <> num a
  Open n      -> "open " <> num n
  Close n     -> "close " <> num n
  Ref n       -> "ref " <> num n
  RefNoCase n -> "ref (?i)" <> num n
  Keep        -> "keep"
  Match       -> "match"
  SetMark m   -> "mark m" <> num m
  IfStill m a -> "ifstill m" <> num m <> ", " <> num a
  Fence       -> "fence"
  FenceNot a  -> "fencenot " <> num a
  Cut         -> "cut"
  CutBack     -> "cutback"
  CutFail     -> "cutfail"
  Back d      -> "back " <> num d
  AtMark m    -> "atmark m" <> num m
  Gosub g a   -> "gosub " <> group g <> ", " <> num a
  Return      -> "return"
  TakeAll a   -> "takeall " <> quoted (render (alone (Quantified a once)))
  where
    num = T.pack . show
    group 0 = "(?R)"
    group g = "(?" <> num g <> ")"
    alone p = Pattern (Branch [p] NE.:| [])
    -- a space would not be seen: write it in quotes
    -- 空白は見えないので、引用符で囲む
    quoted r
      | T.all isSpace r = T.pack (show r)
      | otherwise       = r
-- ANCHOR_END: listing
-- ANCHOR_END: all
