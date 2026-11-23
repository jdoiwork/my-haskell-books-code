{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | A small tree to turn into other languages: the checked program,
--   with every name told apart (a local, a definition, a builtin, a
--   constructor), and only the definitions main needs.
-- ほかの言語に変換するための小さな木：確かめ終えたプログラムの、名前を
-- 見分け（ローカル・定義・組み込み・構成子）、main が使う定義だけを集めた
-- もの。
module Lam.Core
  ( Core (..)
  , Global (..)
  , Lowered (..)
  , Target (..)
  , lower
  , lowerExpr
  , params
  , initNeeds
  ) where

import Control.Monad.State.Strict (State, modify, runState)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub, partition, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lam.Program (Code (..))
import Lam.Syntax

-- ANCHOR: core
-- 🌳 The small tree. The same shapes as Expr, without the marks (At,
--    Annot), and with every name told apart.
-- 小さな木。Expr と同じ形だが、印（At・Annot）がなく、名前がどれも
-- 見分けてある
data Core
  = CLit Literal
  | CLocal Name                 -- x of \x -> ..., let x, case ... | x
  | CGlobal Name Name           -- a definition: its home and its name
  | CPrim Name                  -- a builtin: Map.insert
  | CCon Name Int Int           -- a constructor: its name, number, arity
  | CLam Name Core
  | CApp Core Core
  | CLet Name Core Core
  | CIf Core Core Core
  | COp Op Core Core
  | CTuple [Core]
  | CList [Core]
  | CCase Core [(Pattern, Core)]
  | CRecord [(Name, Core)]
  | CField Core Name
  | CUpdate Core [(Name, Core)]
  deriving (Eq, Show)
-- ANCHOR_END: core

-- ANCHOR: lowered
-- 📦 One definition to write out: List.foldl is home "List", name
--    "foldl".
-- 書き出す定義1つ。List.foldl なら、home は "List"、name は "foldl"
data Global = Global
  { globalHome :: Name
  , globalName :: Name
  , globalBody :: Core
  }
  deriving (Eq, Show)

-- 📦 The whole program, lowered: the definitions (each after the ones
--    it needs), the builtins they call, and the expression of main.
-- 小さな木にしたプログラム全体：定義（それぞれ、使う定義のあと）、呼ぶ
-- 組み込み、main の式
data Lowered = Lowered
  { loweredGlobals :: [Global]
  , loweredPrims   :: [Name]
  , loweredMain    :: Core
  }
  deriving (Eq, Show)

-- 🎯 What the other language has: the builtins its runtime has, and
--    the definitions of the standard modules it has its own version of
--    (List.foldr, written as a loop, so that a long list does not use
--    up the stack).
-- 変換先が持っているもの：ランタイムにある組み込みと、標準のモジュールの
-- 定義のうち、自分の版を持っているもの（List.foldr。ループで書けば、長い
-- リストでスタックを使い切らない）
data Target = Target
  { targetPrims   :: Set Name
  , targetNatives :: Set Name
  }
-- ANCHOR_END: lowered

-- ANCHOR: lower
-- ⬇️ Lower main (the expression that starts it, from checkMain) and
--    every definition it reaches. The names it reaches that the target
--    does not have are the Left: then nothing is written.
-- main（checkMain がくれる、main を始める式）と、そこから届く定義を
-- 全部、小さな木にする。届いた名前のうち、変換先にないものが Left。
-- そのときは、何も書かない
lower :: Target -> Map Name Code -> Expr -> Either [Name] Lowered
lower target scope start =
  case missing of
    [] -> Right Lowered
      { loweredGlobals = ordered
      , loweredPrims   = sort (Set.toList prims)
      , loweredMain    = mainCore }
    names -> Left names
  where
    (mainCore, firstRefs) = lowerExpr target scope start
    (globals, prims) = reach target firstRefs Map.empty Set.empty
    missing = sort [ p | p <- Set.toList prims
                       , not (p `Set.member` targetPrims target)
                       , not (p `Set.member` targetNatives target) ]
    ordered = orderGlobals (Map.elems globals)

-- 🧭 Follow the definitions: lower each one once (by its home and
--    name), and collect the builtins on the way.
-- 定義をたどる：それぞれ（home と name で）1度だけ小さな木にし、途中で
-- 組み込みを集める
reach
  :: Target -> [Ref] -> Map (Name, Name) (Global, [(Name, Name)])
  -> Set Name -> (Map (Name, Name) (Global, [(Name, Name)]), Set Name)
reach target refs done prims = case refs of
  [] -> (done, prims)
  RefPrim p : rest -> reach target rest done (Set.insert p prims)
  RefGlobal home name body seen : rest
    | Map.member (home, name) done -> reach target rest done prims
    | otherwise ->
        let (core, more) = lowerExpr target seen body
            needs = nub [ (h, n) | RefGlobal h n _ _ <- more ]
            done' = Map.insert (home, name)
                      (Global home name core, needs) done
        in reach target (more <> rest) done' prims
-- ANCHOR_END: lower

-- ANCHOR: order
-- 🔢 Put the definitions in an order the target can define them:
--    each group of definitions that use each other (a strongly
--    connected component) after the groups it uses. Inside a group,
--    the functions come first: a function only looks at the others
--    when it is called. A value that is not a function is computed
--    right away, so it must come after what it uses at once.
-- 変換先が定義できる順に並べる：使い合う定義のグループ（強連結成分）を、
-- 使うグループのあとに。グループの中では関数が先。関数がほかを見るのは
-- 呼ばれたときだけ。関数でない値はすぐに計算するので、すぐ使うものの
-- あとに置く
orderGlobals :: [(Global, [(Name, Name)])] -> [Global]
orderGlobals defs = concatMap inGroup (stronglyConnComp
  [ (g, (globalHome g, globalName g), needs) | (g, needs) <- defs ])
  where
    inGroup scc = case scc of
      AcyclicSCC g -> [g]
      CyclicSCC gs ->
        let (functions, values) = partition isFunction gs
        in functions <> valuesInOrder values
    isFunction g = case globalBody g of
      CLam _ _ -> True
      _        -> False
    -- The values of one group: each after the values it needs at once.
    -- グループの値：それぞれ、すぐ要る値のあとに
    valuesInOrder values = concatMap flatten (stronglyConnComp
      [ (g, key g, Set.toList (initNeeds (globalBody g))) | g <- values ])
    flatten scc = case scc of
      AcyclicSCC g -> [g]
      CyclicSCC gs -> gs
    key g = (globalHome g, globalName g)

-- 🔎 The definitions a tree needs right away, when it is computed: not
--    the ones inside a function, which wait until it is called.
-- 木を計算するとき、すぐ要る定義。関数の中のものは、呼ばれるまで待つ
initNeeds :: Core -> Set (Name, Name)
initNeeds core = case core of
  CGlobal h n   -> Set.singleton (h, n)
  CLam _ _      -> Set.empty
  CApp f a      -> initNeeds f <> initNeeds a
  CLet _ e b    -> initNeeds e <> initNeeds b
  CIf c a b     -> initNeeds c <> initNeeds a <> initNeeds b
  COp _ l r     -> initNeeds l <> initNeeds r
  CTuple es     -> foldMap initNeeds es
  CList es      -> foldMap initNeeds es
  CCase e alts  -> initNeeds e <> foldMap (initNeeds . snd) alts
  CRecord fs    -> foldMap (initNeeds . snd) fs
  CField e _    -> initNeeds e
  CUpdate e fs  -> initNeeds e <> foldMap (initNeeds . snd) fs
  _             -> Set.empty
-- ANCHOR_END: order

-- ANCHOR: lowerExpr
-- 🔗 A name found on the way: a definition to lower too (with the
--    names its tree sees), or a builtin.
-- 途中で見つけた名前：これも小さな木にする定義（その木が見る名前と）か、
-- 組み込み
data Ref
  = RefGlobal Name Name Expr (Map Name Code)
  | RefPrim Name

-- ⬇️ One tree, with the names it sees: the small tree, and the names
--    found in it.
-- 木を1つ、それが見る名前と一緒に：小さな木と、その中で見つけた名前
lowerExpr :: Target -> Map Name Code -> Expr -> (Core, [Ref])
lowerExpr target scope e =
  let (core, refs) = runState (go Set.empty e) []
  in (core, reverse refs)
  where
    found :: Ref -> State [Ref] ()
    found r = modify (r :)
    go :: Set Name -> Expr -> State [Ref] Core
    go locals expr = case expr of
      At _ x        -> go locals x
      Annot _ x     -> go locals x
      Lit l         -> pure (CLit l)
      Var x
        | x `Set.member` locals -> pure (CLocal x)
        | otherwise -> named x
      Lam x b       -> CLam x <$> go (Set.insert x locals) b
      App f a       -> CApp <$> go locals f <*> go locals a
      -- let is recursive: e sees x, too.
      -- let は再帰する：e からも x が見える
      Let x a b     -> let inner = Set.insert x locals
                       in CLet x <$> go inner a <*> go inner b
      If c a b      -> CIf <$> go locals c <*> go locals a <*> go locals b
      BinOp o l r   -> COp o <$> go locals l <*> go locals r
      Tuple es      -> CTuple <$> traverse (go locals) es
      List es       -> CList <$> traverse (go locals) es
      Case x alts   -> CCase <$> go locals x <*> traverse (branch locals) alts
      Record fs     -> CRecord <$> traverse (field locals) fs
      Field x n     -> (`CField` n) <$> go locals x
      Update x fs   -> CUpdate <$> go locals x <*> traverse (field locals) fs
    branch locals (p, b) =
      (,) p <$> go (locals <> patternVars p) b
    field locals (n, x) = (,) n <$> go locals x
    -- A name that is not local: what the scope says it was made from.
    -- A definition the target has its own version of is a builtin.
    -- ローカルでない名前：スコープが言う、作ったもの。変換先が自分の版を
    -- 持っている定義は、組み込みとして扱う
    named x = case Map.lookup x scope of
      Just (Defined home name body seen)
        | (home <> "." <> name) `Set.member` targetNatives target -> do
            found (RefPrim (home <> "." <> name))
            pure (CPrim (home <> "." <> name))
        | otherwise -> do
            found (RefGlobal home name body seen)
            pure (CGlobal home name)
      Just (Primitive p) -> found (RefPrim p) >> pure (CPrim p)
      Just (Made c i n)  -> pure (CCon c i n)
      -- Not there: the type checker would have said so already. It is
      -- told as a name the target lacks.
      -- ない：型検査器が先に言ったはず。変換先にない名前として知らせる
      Nothing -> found (RefPrim x) >> pure (CPrim x)
-- ANCHOR_END: lowerExpr

-- ANCHOR: params
-- 🧮 The parameters of a function and its body:
--    \f -> \z -> \xs -> body  is  ([f, z, xs], body).
-- 関数の引数と本体
params :: Core -> ([Name], Core)
params core = case core of
  CLam x b -> let (xs, body) = params b in (x : xs, body)
  _        -> ([], core)
-- ANCHOR_END: params
-- ANCHOR_END: all
