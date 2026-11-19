-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Sugar: shorter ways of writing, opened into the old shapes of the
--   tree. The evaluator and the type checker never see them.
-- 糖衣構文：短い書き方を、これまでの木の形に開く。評価器と型検査器は、
-- 糖衣構文を知らない。
module Lam.Sugar
  ( lambdaWith
  , letPattern
  , Qualifier (..)
  , comprehension
  , Operator (..)
  , operatorFunction
  , leftSection
  , rightSection
  , pipe
  , pipeLeft
  , composeRight
  , composeLeft
  , Statement (..)
  , doBind
  , doBlock
  ) where

import Lam.Syntax
-- ANCHOR_END: head

-- ANCHOR: lambda
-- 🧩 \p1 p2 -> body, where each pi may be a pattern.
--    A plain name stays as it is; any other pattern becomes a fresh
--    argument $arg, taken apart by a case with one branch:
--      \(a, b) -> a + b   is   \$arg -> case $arg of | (a, b) -> a + b
--    $arg cannot be written in Lam, so it hits no name of the user.
-- 引数がパターンでもよいラムダ。名前はそのまま。ほかのパターンは、
-- 新しい引数 $arg にして、枝が1つの case で分ける。$arg は Lam では
-- 書けない名前なので、使う人の名前とぶつからない
lambdaWith :: [Pattern] -> Expr -> Expr
lambdaWith params body = foldr one body params
  where
    one (PVar x) rest = Lam x rest
    one PWild    rest = Lam "$_" rest
    one p        rest = Lam "$arg" (Case (Var "$arg") [(p, rest)])
-- ANCHOR_END: lambda

-- ANCHOR: letPattern
-- 🧩 let (q, r) = e in body   is   case e of | (q, r) -> body
--    Unlike a plain let, it is not recursive: e cannot use q or r.
-- パターンの let は、枝が1つの case。ふつうの let とちがって、
-- 再帰しない（e の中で q や r は使えない）
letPattern :: Pattern -> Expr -> Expr -> Expr
letPattern p e body = Case e [(p, body)]
-- ANCHOR_END: letPattern

-- ANCHOR: comprehension
-- 🧺 One part after the | of a list comprehension.
-- リスト内包表記の | のあとの部品
data Qualifier
  = Generator Pattern Expr   -- x <- xs
  | Condition Expr           -- x > 1
  | LetName Name Expr        -- let y = x * 2
  | LetPattern Pattern Expr  -- let (a, b) = pair
  deriving (Eq, Show)

-- 🧺 [e | q1, q2, ...], opened from the left:
--      [e | ]                  is  [e]
--      [e | c, rest]           is  if c then [e | rest] else []
--      [e | let y = d, rest]   is  let y = d in [e | rest]
--      [e | p <- xs, rest]     is  a loop $go over xs:
--        let $go = \$xs -> case $xs of
--          | [] -> []
--          | p : $rest -> [e | rest] ++ $go $rest
--          | _ : $rest -> $go $rest       (an item that does not fit p)
--        in $go xs
--    The inner [e | rest] may have its own $go, but it is inside the
--    branch, so the $go $rest beside it is still the outer one.
-- 内包表記を、左の部品から開く。<- はループ $go になる。内側の
-- [e | rest] にも $go があってよい。枝の中で決まるので、となりの
-- $go $rest は、外の $go のまま
comprehension :: Expr -> [Qualifier] -> Expr
comprehension e qualifiers = case qualifiers of
  [] -> List [e]
  q : rest ->
    let inner = comprehension e rest
    in case q of
         Condition c    -> If c inner (List [])
         LetName y d    -> Let y d inner
         LetPattern p d -> letPattern p d inner
         Generator p xs ->
           let go    = Var "$go"
               more  = App go (Var "$rest")
               skip  = [ (PCons PWild (PVar "$rest"), more) | refutable p ]
               loop  = Lam "$xs" $ Case (Var "$xs") $
                         [ (PList [], List [])
                         , (PCons p (PVar "$rest"), BinOp Concat inner more) ]
                         <> skip
           in Let "$go" loop (App go xs)
  where
    -- A name or _ fits every item; then the skipping branch is not needed.
    -- 名前と _ は、どの要素にも合うので、飛ばす枝は要らない
    refutable p = case p of
      PVar _ -> False
      PWild  -> False
      -- A record pattern fits every record of its type.
      -- レコードのパターンは、その型のどのレコードにも合う
      PRecord _ -> False
      _      -> True
-- ANCHOR_END: comprehension

-- ANCHOR: operators
-- ➕ An operator, as a way to build the tree from its two sides.
--    Most are BinOp; |> and >> are sugar too, so they are built
--    from App and Lam.
-- 演算子を、左右の式から木を作る方法として持つ。たいていは BinOp。
-- |> や >> も糖衣構文なので、App と Lam で作る
newtype Operator = Operator (Expr -> Expr -> Expr)

-- 🔧 (+)  is  \$l $r -> $l + $r
-- 演算子を関数に
operatorFunction :: Operator -> Expr
operatorFunction (Operator build) =
  Lam "$l" (Lam "$r" (build (Var "$l") (Var "$r")))

-- 🔧 (10 -)  is  \$r -> 10 - $r
-- 左の値を決めたセクション
leftSection :: Expr -> Operator -> Expr
leftSection l (Operator build) = Lam "$r" (build l (Var "$r"))

-- 🔧 (- 1)  is  \$l -> $l - 1
-- 右の値を決めたセクション
rightSection :: Operator -> Expr -> Expr
rightSection (Operator build) r = Lam "$l" (build (Var "$l") r)
-- ANCHOR_END: operators

-- ANCHOR: pipe
-- 🚰 x |> f  is  f x,  and  f <| x  is  f x  too.
-- パイプライン。どちらも f x のこと
pipe :: Expr -> Expr -> Expr
pipe x f = App f x

pipeLeft :: Expr -> Expr -> Expr
pipeLeft f x = App f x
-- ANCHOR_END: pipe

-- ANCHOR: compose
-- 🔗 f >> g  is  \$x -> g (f $x)  (f first, then g),
--    and  g << f  is the same function.
-- 関数の合成。f >> g は「f してから g」。g << f も同じ関数
composeRight :: Expr -> Expr -> Expr
composeRight f g = Lam "$x" (App g (App f (Var "$x")))

composeLeft :: Expr -> Expr -> Expr
composeLeft g f = composeRight f g
-- ANCHOR_END: compose

-- ANCHOR: statement
-- 📝 One line of a do.
-- do の1行
data Statement
  = DoBind Pattern Expr       -- name <- getLine
  | DoRun Expr                -- putStrLn "hi"
  | DoLet Name Expr           -- let n = length name
  | DoLetPattern Pattern Expr -- let (a, b) = pair
  deriving (Eq, Show)
-- ANCHOR_END: statement

-- ANCHOR: doBlock
-- 📝 do, opened from the top line down:
--      do p <- e; rest       is  $do.bind e (\p -> do rest)
--      do e; rest            is  $do.bind e (\_ -> do rest)
--      do let x = e; rest    is  let x = e in do rest
--      do e                  is  e        (the last line)
--    $do.bind is the bind of the class Monad, under a name a program
--    cannot write: a bind of your own does not take its place.
-- do を、上の行から開く。$do.bind は、クラス Monad の bind を、
-- プログラムには書けない名前で持ったもの。自分で bind を定義しても、
-- 入れ替わらない
doBlock :: [Statement] -> Expr
doBlock statements = case statements of
  [] -> Tuple []                      -- the parser never makes this
  [DoRun e] -> e
  s : rest ->
    let inner = doBlock rest
    in case s of
         DoBind p e       -> bindThen e (lambdaWith [p] inner)
         DoRun e          -> bindThen e (Lam "$_" inner)
         DoLet x e        -> Let x e inner
         DoLetPattern p e -> letPattern p e inner
  where
    bindThen e f = App (App (Var doBind) e) f

-- 🙈 The hidden name of bind.
-- bind の隠した名前
doBind :: Name
doBind = "$do.bind"
-- ANCHOR_END: doBlock
-- ANCHOR_END: all
