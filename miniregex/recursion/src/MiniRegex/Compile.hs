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
  , Line (..)
  , assemble
  , listing
  ) where

-- ANCHOR: imports
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Control.Monad.State.Strict (State, evalState, gets, state)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Continuation (farthestFirst, numbered)
import MiniRegex.Groups (branchGroups, calls, groupBody, pieceGroups)
import MiniRegex.Pretty (render)
import MiniRegex.Program
import MiniRegex.Syntax
import MiniRegex.Width (branchWidth)
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
compileTree p = assemble (evalState whole start)
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
-- 🧩 A piece. a*+ is (?>a*): the greedy repeat inside a fence.
-- 部品。a*+ は (?>a*)：欲ばりのくり返しを、区切りの中で
piece :: Int -> Piece -> Asm [Line]
piece _ (Anchor a) = pure [Do (Assert a)]
piece _ KeepOut    = pure [Do Keep]
piece n (Quantified a q)
  | greed q == Possessive =
      atomic (repeatAtom q { greed = Greedy } (atom n a))
  | otherwise = repeatAtom q (atom n a)
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

-- ANCHOR: repeat
-- 🔁 A repeat, the way the continuation matcher does it. The copies
--    that must be are put one after another; then either a loop (no
--    upper end) or the copies that may be (an upper end).
-- くり返し。継続のマッチャーと同じやり方。「必ず」の写しを並べてから、
-- 上の端がなければループ、あれば「あってもよい」写しを並べる
repeatAtom :: Quantifier -> Asm [Line] -> Asm [Line]
repeatAtom (Quantifier (Count least more) greed) one = case more of
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
    -- One time round: mark where it starts, and do not go round again
    -- when it took nothing.
    -- 1回分：始めた場所に印を付ける。何も食べなかったら、もう回らない
    loop top out = do
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
  -- ANCHOR: showcall
  Gosub g a   -> "gosub " <> group g <> ", " <> num a
  Return      -> "return"
  where
    num = T.pack . show
    group 0 = "(?R)"
    group g = "(?" <> num g <> ")"
    -- ANCHOR_END: showcall
    alone p = Pattern (Branch [p] NE.:| [])
    -- a space would not be seen: write it in quotes
    -- 空白は見えないので、引用符で囲む
    quoted r
      | T.all isSpace r = T.pack (show r)
      | otherwise       = r
-- ANCHOR_END: listing
-- ANCHOR_END: all
