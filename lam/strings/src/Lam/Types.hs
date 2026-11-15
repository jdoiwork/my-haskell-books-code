{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Type inference for Lam (Hindley-Milner, algorithm W):
--   find the type of every expression, without any type written.
-- Lam の型推論（Hindley-Milner、W アルゴリズム）：
-- 型を1つも書かなくても、すべての式の型を見つける。
module Lam.Types
  ( Type (..)
  , Scheme (..)
  , Class (..)
  , Needs
  , TypeEnv
  , TypeError (..)
  , tString
  , tList
  , tTuple
  , closeOver
  , closeOverWith
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
-- 🏷️ A type: Int, Bool, a type variable (not known yet), a function,
--    or a type with a name and some types inside.
-- 型：Int、Bool、型変数（まだわからない型）、関数、
-- または、名前と、中の型を持つ型
data Type
  = TInt
  | TBool
  | TVar Int            -- t0, t1, ... shown as a, b, ...
  | TFun Type Type      -- a -> b
  | TCon Name [Type]    -- String, [a], (a, b)
  deriving (Eq, Ord, Show)

-- ANCHOR: tcon
-- 🧱 The types made with TCon.
-- TCon で作る型
tString :: Type
tString = TCon "String" []          -- String

tList :: Type -> Type
tList a = TCon "List" [a]           -- [a]

tTuple :: [Type] -> Type
tTuple ts = TCon "Tuple" ts         -- (a, b)
-- ANCHOR_END: tcon

-- ANCHOR: class
-- 🎓 The classes: what a type variable may be. The language decides
--    them and their members; programs cannot add more.
-- クラス：型変数がなれるもの。クラスもその仲間も言語が決めていて、
-- プログラムからは増やせない
data Class
  = Comparable    -- == < ... : Int, Bool, String, lists and tuples of them
  | Semigroup     -- ++       : String, [a]
  | HasLength     -- length   : String, [a]
  deriving (Eq, Ord, Show, Enum, Bounded)

-- 📋 For each type variable, the classes it must be in.
--    {3 := {Comparable}} means "t3 must be comparable".
-- 型変数ごとの、入っていなければならないクラス
type Needs = Map Int (Set Class)
-- ANCHOR_END: class

-- ANCHOR: scheme-type
-- 🧬 A type that works for ANY a, b, ...: forall a. a -> a
--    (the type of a let-defined name, like id). Some of the variables
--    may need classes: forall a. Comparable a => a -> a -> a.
-- どんな a、b でも使える型（let で決めた id のような名前の型）。
-- 型変数に、クラスの条件が付くこともある
data Scheme = Forall [Int] Needs Type
  deriving (Eq, Show)
-- ANCHOR_END: scheme-type

-- 📒 The type of each name.
-- それぞれの名前の型
type TypeEnv = Map Name Scheme

-- 🚨 Why a program has no type.
-- プログラムに型が付かない理由
data TypeError
  = Mismatch Type Type      -- found / needed
  | InfiniteType Int Type   -- a = a -> b
  | UnknownName Name
  | NotInClass Class Type   -- a -> a is not Comparable
  deriving (Eq, Show)
-- ANCHOR_END: types

-- ANCHOR: subst
-- 📝 A substitution: what we have learned about type variables.
--    {t0 := Int} means "t0 turned out to be Int".
-- 置き換え：型変数について、わかったこと
type Subst = Map Int Type

-- 🔁 Apply what we learned to a type.
-- わかったことを、型に当てはめる
-- ANCHOR: applyT
applyT :: Subst -> Type -> Type
applyT s t = case t of
  TVar n    -> Map.findWithDefault t n s
  TFun a b  -> TFun (applyT s a) (applyT s b)
  TCon c ts -> TCon c (map (applyT s) ts)
  _         -> t
-- ANCHOR_END: applyT

applyScheme :: Subst -> Scheme -> Scheme
applyScheme s (Forall vs needs t) =
  Forall vs needs (applyT (foldr Map.delete s vs) t)

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
  -- ANCHOR: unifyCon
  -- The same name and the same number of types inside: make the
  -- insides the same, one by one.  [t0] and [Int] give {t0 := Int}.
  -- 名前も中の型の数も同じなら、中の型を1つずつ同じにする
  (TCon c1 ts1, TCon c2 ts2)
    | c1 == c2 && length ts1 == length ts2 -> unifyMany ts1 ts2
  -- ANCHOR_END: unifyCon
  _ -> Left (Mismatch a b)

-- ANCHOR: unifyMany
-- 🤝 unify, for two lists of types side by side.
-- 並んだ2つの型のリストを、順に unify する
unifyMany :: [Type] -> [Type] -> Either TypeError Subst
unifyMany as bs = case (as, bs) of
  (a : as', b : bs') -> do
    s1 <- unify a b
    s2 <- unifyMany (map (applyT s1) as') (map (applyT s1) bs')
    pure (compose s2 s1)
  _ -> Right Map.empty
-- ANCHOR_END: unifyMany

-- 📌 t_n := t. But not t_n := t_n -> ... (that type would be infinite).
-- t_n := t。ただし t_n := t_n -> ... はだめ（無限に大きな型になる）
bind :: Int -> Type -> Either TypeError Subst
bind n t
  | t == TVar n          = Right Map.empty
  | n `Set.member` ftv t = Left (InfiniteType n t)
  | otherwise            = Right (Map.singleton n t)

-- 🔓 The type variables inside a type.
-- 型の中の型変数
-- ANCHOR: ftv
ftv :: Type -> Set Int
ftv t = case t of
  TVar n    -> Set.singleton n
  TFun a b  -> ftv a <> ftv b
  TCon _ ts -> foldMap ftv ts
  _         -> Set.empty
-- ANCHOR_END: ftv
-- ANCHOR_END: unify

-- ANCHOR: monad
-- 🎟️ Inference needs fresh type variables (t0, t1, ...) and may fail.
--    Infer carries the next number along, like a ticket machine, and
--    the classes each type variable must be in.
-- 推論には、新しい型変数（t0、t1、...）が要り、失敗もする。
-- Infer は、次の番号を、番号札の機械のように持ち回る。
-- 型変数ごとのクラスの条件も、一緒に持ち回る
data InferState = InferState
  { nextVar :: Int
  , needs   :: Needs
  }

newtype Infer a = Infer (InferState -> Either TypeError (a, InferState))

runInfer :: Infer a -> Either TypeError a
runInfer (Infer f) = fst <$> f (InferState 0 Map.empty)

instance Functor Infer where
  fmap g (Infer f) = Infer (\st -> fmap (\(a, st') -> (g a, st')) (f st))

instance Applicative Infer where
  pure a = Infer (\st -> Right (a, st))
  mf <*> ma = do
    g <- mf
    g <$> ma

instance Monad Infer where
  Infer f >>= k = Infer $ \st -> case f st of
    Left err       -> Left err
    Right (a, st') -> let Infer g = k a in g st'

-- 🆕 A new type variable we have never used.
-- まだ使っていない、新しい型変数
fresh :: Infer Type
fresh = Infer $ \st ->
  Right (TVar (nextVar st), st { nextVar = nextVar st + 1 })

-- ❌ Stop with an error.
-- エラーで止まる
failWith :: TypeError -> Infer a
failWith err = Infer (\_ -> Left err)

-- ANCHOR: liftUnify
-- 🤝 unify, inside Infer. Then check the classes: when t3 must be
--    Comparable and t3 turned out to be Int -> Int, that is an error.
-- Infer の中の unify。そのあと、クラスの条件を確かめる
liftUnify :: Type -> Type -> Infer Subst
liftUnify a b = do
  s <- either failWith pure (unify a b)
  current <- currentNeeds
  sequence_
    [ require c t
    | (n, t) <- Map.toList s
    , c <- maybe [] Set.toList (Map.lookup n current) ]
  pure s
-- ANCHOR_END: liftUnify
-- ANCHOR_END: monad

-- ANCHOR: require
-- 📋 The classes each type variable must be in, so far.
-- ここまでにわかった、型変数ごとのクラスの条件
currentNeeds :: Infer Needs
currentNeeds = Infer (\st -> Right (needs st, st))

-- 🎓 Type t must be in class c. A type variable just remembers it;
--    [a] is Comparable when a is, so the condition moves inside.
-- 型 t は、クラス c に入っていなければならない。型変数なら覚えておくだけ。
-- [a] は a が Comparable なら Comparable なので、条件が中へ移る
require :: Class -> Type -> Infer ()
require c t = case t of
  TVar n -> Infer $ \st ->
    let more = Map.insertWith (<>) n (Set.singleton c) (needs st)
    in Right ((), st { needs = more })
  _ -> case members c t of
    Just inside -> mapM_ (require c) inside
    Nothing     -> failWith (NotInClass c t)

-- 📖 The members of each class: Nothing if t is not one; otherwise
--    the types inside that must be in the class too.
-- クラスの仲間の表。仲間でなければ Nothing。仲間なら、
-- 同じクラスに入っていなければならない、中の型
members :: Class -> Type -> Maybe [Type]
members c t = case (c, t) of
  (Comparable, TInt)              -> Just []
  (Comparable, TBool)             -> Just []
  (Comparable, TCon "String" [])  -> Just []
  (Comparable, TCon "List" [a])   -> Just [a]
  (Comparable, TCon "Tuple" ts)   -> Just ts
  (Semigroup,  TCon "String" [])  -> Just []
  (Semigroup,  TCon "List" [_])   -> Just []
  (HasLength,  TCon "String" [])  -> Just []
  (HasLength,  TCon "List" [_])   -> Just []
  _                               -> Nothing
-- ANCHOR_END: require

-- ANCHOR: scheme
-- 🧬 forall a. a -> a  becomes  t7 -> t7  (new variables each use).
--    The new variables get the same classes as the old ones.
-- 使うたびに、新しい型変数にする。クラスの条件も、新しい型変数に写す
instantiate :: Scheme -> Infer Type
instantiate (Forall vs ns t) = do
  news <- mapM (const fresh) vs
  sequence_
    [ require c new
    | (v, new) <- zip vs news
    , c <- maybe [] Set.toList (Map.lookup v ns) ]
  pure (applyT (Map.fromList (zip vs news)) t)

-- 🧬 t3 -> t3, where t3 is not used by the environment,
--    becomes  forall a. a -> a.
-- 環境が使っていない型変数を、「どんな型でもよい」にする
generalize :: TypeEnv -> Needs -> Type -> Scheme
generalize env ns t =
  -- The variables the environment really uses: in a forall type,
  -- the variables under forall do not count.
  -- 環境が本当に使っている型変数。forall の型では、
  -- forall で縛られた型変数は数えない
  let envVars = Set.unions
        [ ftv ty `Set.difference` Set.fromList vs
        | Forall vs _ ty <- Map.elems env ]
      vs' = ftv t `Set.difference` envVars
  -- The generalized variables keep their classes.
  -- 「どんな型でもよい」にした型変数は、クラスの条件を持っていく
  in Forall (Set.toList vs') (Map.restrictKeys ns vs') t

-- ANCHOR: closeOver
-- 🧬 Every type variable may be any type: for the builtins.
-- どの型変数も「どんな型でもよい」にする（組み込みの関数の型に使う）
closeOver :: Type -> Scheme
closeOver = closeOverWith []

-- 🧬 The same, with classes: closeOverWith [(HasLength, 0)] (t0 -> Int)
--    is  HasLength a => a -> Int.
-- 同じだが、クラスの条件を付ける
closeOverWith :: [(Class, Int)] -> Type -> Scheme
closeOverWith cs t = Forall (Set.toList (ftv t))
  (Map.fromListWith (<>) [ (n, Set.singleton c) | (c, n) <- cs ]) t
-- ANCHOR_END: closeOver
-- ANCHOR_END: scheme

-- ANCHOR: infer
-- 🔍 Algorithm W: the type of an expression, and what we learned.
-- W アルゴリズム：式の型と、わかったこと
infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env expr = case expr of
  -- ANCHOR: inferLit
  Lit (LInt _)    -> pure (Map.empty, TInt)
  Lit (LBool _)   -> pure (Map.empty, TBool)
  Lit (LString _) -> pure (Map.empty, tString)
  -- ANCHOR_END: inferLit

  Var x -> case Map.lookup x env of
    Nothing -> failWith (UnknownName x)
    Just sc -> do
      t <- instantiate sc
      pure (Map.empty, t)

  -- \x -> body : we do not know x's type yet, so give it a new t.
  -- x の型はまだわからないので、新しい t にしておく
  Lam x body -> do
    tx <- fresh
    (s, tb) <- infer (Map.insert x (Forall [] Map.empty tx) env) body
    pure (s, TFun (applyT s tx) tb)

  -- f a : f must be a function from a's type to something new.
  -- f は、a の型から、何か新しい型への関数のはず
  App f a -> do
    tr <- fresh
    (s1, tf) <- infer env f
    (s2, ta) <- infer (applyEnv s1 env) a
    let tf'    = applyT s2 tf
        needed = TFun ta tr
    -- ANCHOR: notAFunction
    s3 <- case tf' of
      -- A number, true/false, a string, a tuple or a list is not
      -- a function at all.
      -- 数、真偽値、文字列、タプル、リストは、そもそも関数ではない
      TInt    -> failWith (Mismatch tf' needed)
      TBool   -> failWith (Mismatch tf' needed)
      TCon {} -> failWith (Mismatch tf' needed)
      -- Otherwise the argument must fit the function: found first.
      -- そうでなければ、引数が関数に合うはず（来たものを先に）
      _       -> liftUnify needed tf'
    -- ANCHOR_END: notAFunction
    pure (s3 `compose` s2 `compose` s1, applyT s3 tr)

  -- let is recursive: x has a new t while we look at e.
  -- Then x gets a forall type, so that the body can use it in many ways.
  -- let は再帰する。e を見るあいだ、x は新しい t。
  -- そのあと x を forall の型にして、本体でいろいろな型で使えるようにする
  Let x e body -> do
    tx <- fresh
    (s1, te) <- infer (Map.insert x (Forall [] Map.empty tx) env) e
    s2 <- liftUnify (applyT s1 tx) te
    let s    = s2 `compose` s1
        env' = applyEnv s env
    ns <- currentNeeds
    let sc = generalize env' ns (applyT s te)
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

  -- ANCHOR: inferBinOp
  -- Each operator says what its two sides must be, and what it gives.
  -- 演算子ごとに、両側の型と、答えの型が決まっている
  BinOp op l r -> do
    (left, right, result) <- opType op
    (s1, tl) <- infer env l
    (s2, tr) <- infer (applyEnv s1 env) r
    s3 <- liftUnify (applyT s2 tl) (applyT (s2 `compose` s1) left)
    let s123 = s3 `compose` s2 `compose` s1
    s4 <- liftUnify (applyT s3 tr) (applyT s123 right)
    let s = s4 `compose` s123
    pure (s, applyT s result)
  -- ANCHOR_END: inferBinOp

  -- ANCHOR: inferCollections
  -- (a, b): the type of each part, side by side.
  -- (a, b)：部品の型を、順に並べる
  Tuple es -> do
    (s, ts) <- inferAll env es
    pure (s, tTuple ts)

  -- [a, b, c]: every item must have the same type.
  -- [a, b, c]：どの要素も、同じ型でなければならない
  List es -> do
    item <- fresh
    (s1, ts) <- inferAll env es
    s2 <- unifyAll (applyT s1 item) ts
    let s = s2 `compose` s1
    pure (s, tList (applyT s item))
  -- ANCHOR_END: inferCollections
-- ANCHOR_END: infer

-- ANCHOR: opType
-- ➕ The types of the two sides, and of the answer. Both sides of
--    == and < are the same Comparable type; ++ joins two of the same
--    Semigroup type.
-- 両側の型と、答えの型。== や < の両側は、同じ Comparable の型。
-- ++ は、同じ Semigroup の型を2つつなぐ
opType :: Op -> Infer (Type, Type, Type)
opType op = case op of
  _ | op `elem` [Add, Sub, Mul, Div] -> pure (TInt, TInt, TInt)
    | op `elem` [Eq, Ne, Lt, Le, Gt, Ge] -> do
        a <- freshIn Comparable
        pure (a, a, TBool)
  Concat -> do
    a <- freshIn Semigroup
    pure (a, a, a)
  Cons   -> do
    a <- fresh
    pure (a, tList a, tList a)
  _      -> pure (TBool, TBool, TBool)          -- && ||

-- 🆕 A new type variable that must be in class c.
-- クラス c に入っていなければならない、新しい型変数
freshIn :: Class -> Infer Type
freshIn c = do
  a <- fresh
  require c a
  pure a
-- ANCHOR_END: opType

-- ANCHOR: inferAll
-- 🔍 The types of many expressions, from left to right.
-- いくつもの式の型を、左から順に
inferAll :: TypeEnv -> [Expr] -> Infer (Subst, [Type])
inferAll env es = case es of
  [] -> pure (Map.empty, [])
  e : rest -> do
    (s1, t)  <- infer env e
    (s2, ts) <- inferAll (applyEnv s1 env) rest
    pure (s2 `compose` s1, applyT s2 t : ts)

-- 🤝 Make every type in the list the same as t.
-- リストのどの型も、t と同じにする
unifyAll :: Type -> [Type] -> Infer Subst
unifyAll t ts = case ts of
  [] -> pure Map.empty
  u : rest -> do
    s1 <- liftUnify u t
    s2 <- unifyAll (applyT s1 t) (map (applyT s1) rest)
    pure (s2 `compose` s1)
-- ANCHOR_END: inferAll

-- ANCHOR: run
-- 🔍 The type of a whole expression.
-- 式全体の型
-- The answer is a Scheme, to show the classes: Comparable a => a -> Bool
-- クラスの条件を見せるため、答えは Scheme
inferType :: TypeEnv -> Expr -> Either TypeError Scheme
inferType env e = runInfer $ do
  (s, t) <- infer env e
  ns <- currentNeeds
  pure (generalize (applyEnv s env) ns (applyT s t))

-- 🔍 The type of a REPL definition  let x = e  (recursive, like let).
-- REPL の定義 let x = e の型（let と同じく再帰する）
inferDefinition :: TypeEnv -> Name -> Expr -> Either TypeError Scheme
inferDefinition env x e = runInfer $ do
  tx <- fresh
  (s1, te) <- infer (Map.insert x (Forall [] Map.empty tx) env) e
  s2 <- liftUnify (applyT s1 tx) te
  let s = s2 `compose` s1
  ns <- currentNeeds
  pure (generalize (applyEnv s env) ns (applyT s te))
-- ANCHOR_END: run

-- ANCHOR: render
-- 🖨️ Show a type, naming the variables a, b, c, ... in order.
-- 型を見せる。型変数は、出てきた順に a、b、c、... と呼ぶ
renderType :: Type -> Text
renderType t = renderWith (typeNames t) t

-- 🏷️ The names a, b, c, ... of the type variables, in order.
-- 型変数の名前 a、b、c、...（出てきた順）
typeNames :: Type -> Map Int Text
typeNames t = Map.fromList (zip (nub (order t)) letters)
  where
    order ty = case ty of
      TVar n    -> [n]
      TFun a b  -> order a <> order b
      TCon _ ts -> concatMap order ts
      _         -> []
    letters = [ T.singleton c | c <- ['a' .. 'z'] ]
              <> [ "t" <> T.pack (show i) | i <- [1 :: Int ..] ]

renderWith :: Map Int Text -> Type -> Text
renderWith names0 = go names0 False
  where
    go names left ty = case ty of
      TInt     -> "Int"
      TBool    -> "Bool"
      TVar n   -> Map.findWithDefault "?" n names
      -- -> leans to the right: a -> b -> c is a -> (b -> c).
      -- -> は右に寄る
      TFun a b ->
        let inner = go names True a <> " -> " <> go names False b
        in if left then "(" <> inner <> ")" else inner
      -- ANCHOR: renderCon
      -- [a] and (a, b) have their own brackets, so the inside
      -- needs no more.
      -- [a] と (a, b) は自前の括弧があるので、中に括弧は要らない
      TCon "List" [a] -> "[" <> go names False a <> "]"
      TCon "Tuple" ts ->
        "(" <> T.intercalate ", " (map (go names False) ts) <> ")"
      TCon c ts -> T.unwords (c : map (argument names) ts)
      -- ANCHOR_END: renderCon
    -- An argument of a type name, like a in  Box a.  Wrap it when it
    -- is a function or has arguments itself:  Box (Box a).
    -- 型の名前の引数。関数か、それ自身に引数があれば、括弧で包む
    argument names ty = case ty of
      TFun {} -> "(" <> go names False ty <> ")"
      TCon c (_ : _)
        | c `notElem` ["List", "Tuple"] -> "(" <> go names False ty <> ")"
      _ -> go names False ty

-- ANCHOR: renderScheme
-- 🖨️ A type with its classes first, like Haskell:
--    Comparable a => a -> a -> a
-- クラスの条件を前に付けた型（Haskell と同じ見せ方）
renderScheme :: Scheme -> Text
renderScheme (Forall _ ns t) =
  let names = typeNames t
      conds = [ T.pack (show c) <> " " <> name
              | (n, name) <- Map.toList names
              , c <- maybe [] Set.toList (Map.lookup n ns) ]
      prefix = case conds of
        []  -> ""
        [c] -> c <> " => "
        _   -> "(" <> T.intercalate ", " conds <> ") => "
  in prefix <> renderWith names t
-- ANCHOR_END: renderScheme

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
  -- 「a -> a は、Comparable のクラスに入っていません」
  NotInClass c t   -> renderType t <> " is not in the class "
                        <> T.pack (show c)
-- ANCHOR_END: render
-- ANCHOR_END: all
