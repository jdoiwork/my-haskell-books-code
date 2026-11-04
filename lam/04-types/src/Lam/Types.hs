{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Type inference for Lam (Hindley-Milner, algorithm W):
--   find the type of every expression, without any type written.
-- Lam の型推論（Hindley-Milner、W アルゴリズム）：
-- 型を1つも書かなくても、すべての式の型を見つける。
module Lam.Types
  ( Type (..)
  , Scheme (..)
  , TypeEnv
  , TypeError (..)
  , inferType
  , inferDefinition
  , renderType
  , renderScheme
  , renderTypeError
  ) where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Syntax

-- ANCHOR: types
-- 🏷️ A type: Int, Bool, a type variable (not known yet), or a function.
-- 型：Int、Bool、型変数（まだわからない型）、または関数
data Type
  = TInt
  | TBool
  | TVar Int          -- t0, t1, ... shown as a, b, ...
  | TFun Type Type    -- a -> b
  deriving (Eq, Ord, Show)

-- 🧬 A type that works for ANY a, b, ...: forall a. a -> a
--    (the type of a let-defined name, like id).
-- どんな a、b でも使える型（let で決めた id のような名前の型）
data Scheme = Forall [Int] Type
  deriving (Eq, Show)

-- 📒 The type of each name.
-- それぞれの名前の型
type TypeEnv = Map Name Scheme

-- 🚨 Why a program has no type.
-- プログラムに型が付かない理由
data TypeError
  = Mismatch Type Type      -- found / needed
  | InfiniteType Int Type   -- a = a -> b
  | UnknownName Name
  deriving (Eq, Show)
-- ANCHOR_END: types

-- ANCHOR: subst
-- 📝 A substitution: what we have learned about type variables.
--    {t0 := Int} means "t0 turned out to be Int".
-- 置き換え：型変数について、わかったこと
type Subst = Map Int Type

-- 🔁 Apply what we learned to a type.
-- わかったことを、型に当てはめる
applyT :: Subst -> Type -> Type
applyT s t = case t of
  TVar n   -> Map.findWithDefault t n s
  TFun a b -> TFun (applyT s a) (applyT s b)
  _        -> t

applyScheme :: Subst -> Scheme -> Scheme
applyScheme s (Forall vs t) = Forall vs (applyT (foldr Map.delete s vs) t)

applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s = Map.map (applyScheme s)

-- 🔗 First s1, then s2.
-- まず s1、それから s2
compose :: Subst -> Subst -> Subst
compose s2 s1 = Map.map (applyT s2) s1 `Map.union` s2
-- ANCHOR_END: subst

-- ANCHOR: unify
-- 🤝 Make two types the same, if we can, and say how.
--    Int -> t0  and  t1 -> Bool  give {t1 := Int, t0 := Bool}.
-- 2つの型を同じにする（できれば）。その方法を返す
unify :: Type -> Type -> Either TypeError Subst
unify a b = case (a, b) of
  (TInt, TInt)   -> Right Map.empty
  (TBool, TBool) -> Right Map.empty
  (TVar n, t)    -> bind n t
  (t, TVar n)    -> bind n t
  (TFun a1 r1, TFun a2 r2) -> do
    s1 <- unify a1 a2
    s2 <- unify (applyT s1 r1) (applyT s1 r2)
    pure (compose s2 s1)
  _ -> Left (Mismatch a b)

-- 📌 t_n := t. But not t_n := t_n -> ... (that type would be infinite).
-- t_n := t。ただし t_n := t_n -> ... はだめ（無限に大きな型になる）
bind :: Int -> Type -> Either TypeError Subst
bind n t
  | t == TVar n          = Right Map.empty
  | n `Set.member` ftv t = Left (InfiniteType n t)
  | otherwise            = Right (Map.singleton n t)

-- 🔓 The type variables inside a type.
-- 型の中の型変数
ftv :: Type -> Set Int
ftv t = case t of
  TVar n   -> Set.singleton n
  TFun a b -> ftv a <> ftv b
  _        -> Set.empty
-- ANCHOR_END: unify

-- ANCHOR: monad
-- 🎟️ Inference needs fresh type variables (t0, t1, ...) and may fail.
--    Infer carries the next number along, like a ticket machine.
-- 推論には、新しい型変数（t0、t1、...）が要り、失敗もする。
-- Infer は、次の番号を、番号札の機械のように持ち回る
newtype Infer a = Infer (Int -> Either TypeError (a, Int))

runInfer :: Infer a -> Either TypeError a
runInfer (Infer f) = fst <$> f 0

instance Functor Infer where
  fmap g (Infer f) = Infer (\n -> fmap (\(a, n') -> (g a, n')) (f n))

instance Applicative Infer where
  pure a = Infer (\n -> Right (a, n))
  mf <*> ma = do
    g <- mf
    g <$> ma

instance Monad Infer where
  Infer f >>= k = Infer $ \n -> case f n of
    Left err      -> Left err
    Right (a, n') -> let Infer g = k a in g n'

-- 🆕 A new type variable we have never used.
-- まだ使っていない、新しい型変数
fresh :: Infer Type
fresh = Infer (\n -> Right (TVar n, n + 1))

-- ❌ Stop with an error.
-- エラーで止まる
failWith :: TypeError -> Infer a
failWith err = Infer (\_ -> Left err)

-- 🤝 unify, inside Infer.
liftUnify :: Type -> Type -> Infer Subst
liftUnify a b = either failWith pure (unify a b)
-- ANCHOR_END: monad

-- ANCHOR: scheme
-- 🧬 forall a. a -> a  becomes  t7 -> t7  (new variables each use).
-- 使うたびに、新しい型変数にする
instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
  news <- mapM (const fresh) vs
  pure (applyT (Map.fromList (zip vs news)) t)

-- 🧬 t3 -> t3, where t3 is not used by the environment,
--    becomes  forall a. a -> a.
-- 環境が使っていない型変数を、「どんな型でもよい」にする
generalize :: TypeEnv -> Type -> Scheme
generalize env t =
  -- The variables the environment really uses: in a forall type,
  -- the variables under forall do not count.
  -- 環境が本当に使っている型変数。forall の型では、
  -- forall で縛られた型変数は数えない
  let envVars = Set.unions
        [ ftv ty `Set.difference` Set.fromList vs
        | Forall vs ty <- Map.elems env ]
  in Forall (Set.toList (ftv t `Set.difference` envVars)) t
-- ANCHOR_END: scheme

-- ANCHOR: infer
-- 🔍 Algorithm W: the type of an expression, and what we learned.
-- W アルゴリズム：式の型と、わかったこと
infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env expr = case expr of
  Lit (LInt _)  -> pure (Map.empty, TInt)
  Lit (LBool _) -> pure (Map.empty, TBool)

  Var x -> case Map.lookup x env of
    Nothing -> failWith (UnknownName x)
    Just sc -> do
      t <- instantiate sc
      pure (Map.empty, t)

  -- \x -> body : we do not know x's type yet, so give it a new t.
  -- x の型はまだわからないので、新しい t にしておく
  Lam x body -> do
    tx <- fresh
    (s, tb) <- infer (Map.insert x (Forall [] tx) env) body
    pure (s, TFun (applyT s tx) tb)

  -- f a : f must be a function from a's type to something new.
  -- f は、a の型から、何か新しい型への関数のはず
  App f a -> do
    tr <- fresh
    (s1, tf) <- infer env f
    (s2, ta) <- infer (applyEnv s1 env) a
    let tf'    = applyT s2 tf
        needed = TFun ta tr
    s3 <- case tf' of
      -- A number or true/false is not a function at all.
      -- 数や真偽値は、そもそも関数ではない
      TInt  -> failWith (Mismatch tf' needed)
      TBool -> failWith (Mismatch tf' needed)
      -- Otherwise the argument must fit the function: found first.
      -- そうでなければ、引数が関数に合うはず（来たものを先に）
      _     -> liftUnify needed tf'
    pure (s3 `compose` s2 `compose` s1, applyT s3 tr)

  -- let is recursive: x has a new t while we look at e.
  -- Then x gets a forall type, so that the body can use it in many ways.
  -- let は再帰する。e を見るあいだ、x は新しい t。
  -- そのあと x を forall の型にして、本体でいろいろな型で使えるようにする
  Let x e body -> do
    tx <- fresh
    (s1, te) <- infer (Map.insert x (Forall [] tx) env) e
    s2 <- liftUnify (applyT s1 tx) te
    let s    = s2 `compose` s1
        env' = applyEnv s env
        sc   = generalize env' (applyT s te)
    (s3, tb) <- infer (Map.insert x sc env') body
    pure (s3 `compose` s, tb)

  If c t e -> do
    (s1, tc) <- infer env c
    s2 <- liftUnify tc TBool
    let env1 = applyEnv (s2 `compose` s1) env
    (s3, tt) <- infer env1 t
    (s4, te) <- infer (applyEnv s3 env1) e
    -- else must have the same type as then.
    -- else は、then と同じ型でなければならない
    s5 <- liftUnify te (applyT s4 tt)
    let s = s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1
    pure (s, applyT s5 te)

  BinOp op l r -> do
    (s1, tl) <- infer env l
    (s2, tr) <- infer (applyEnv s1 env) r
    s3 <- liftUnify (applyT s2 tl) TInt
    s4 <- liftUnify (applyT s3 tr) TInt
    let s = s4 `compose` s3 `compose` s2 `compose` s1
    pure (s, if op `elem` [Eq, Lt] then TBool else TInt)
-- ANCHOR_END: infer

-- ANCHOR: run
-- 🔍 The type of a whole expression.
-- 式全体の型
inferType :: TypeEnv -> Expr -> Either TypeError Type
inferType env e = runInfer $ do
  (s, t) <- infer env e
  pure (applyT s t)

-- 🔍 The type of a REPL definition  let x = e  (recursive, like let).
-- REPL の定義 let x = e の型（let と同じく再帰する）
inferDefinition :: TypeEnv -> Name -> Expr -> Either TypeError Scheme
inferDefinition env x e = runInfer $ do
  tx <- fresh
  (s1, te) <- infer (Map.insert x (Forall [] tx) env) e
  s2 <- liftUnify (applyT s1 tx) te
  let s = s2 `compose` s1
  pure (generalize (applyEnv s env) (applyT s te))
-- ANCHOR_END: run

-- ANCHOR: render
-- 🖨️ Show a type, naming the variables a, b, c, ... in order.
-- 型を見せる。型変数は、出てきた順に a、b、c、... と呼ぶ
renderType :: Type -> Text
renderType t =
  let names = Map.fromList (zip (nub (order t)) letters)
  in go names False t
  where
    order ty = case ty of
      TVar n   -> [n]
      TFun a b -> order a <> order b
      _        -> []
    letters = [ T.singleton c | c <- ['a' .. 'z'] ]
              <> [ "t" <> T.pack (show i) | i <- [1 :: Int ..] ]
    go names left ty = case ty of
      TInt     -> "Int"
      TBool    -> "Bool"
      TVar n   -> Map.findWithDefault "?" n names
      -- -> leans to the right: a -> b -> c is a -> (b -> c).
      -- -> は右に寄る
      TFun a b ->
        let inner = go names True a <> " -> " <> go names False b
        in if left then "(" <> inner <> ")" else inner

renderScheme :: Scheme -> Text
renderScheme (Forall _ t) = renderType t

-- 📣 A type error, as people read it.
-- 人が読む型のエラー
renderTypeError :: TypeError -> Text
renderTypeError err = case err of
  -- 「Int が要るところに、Bool が来ました」
  Mismatch a b     -> "the types do not fit: " <> renderType a
                        <> " where " <> renderType b <> " is needed"
  -- 「無限に大きな型になります」
  InfiniteType n t -> "an infinite type: " <> renderType (TVar n)
                        <> " = " <> renderType t
                        <> " (is a function applied to itself?)"
  -- 「知らない名前です」
  UnknownName x    -> "unknown name: " <> x
-- ANCHOR_END: render
-- ANCHOR_END: all
