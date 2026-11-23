{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Type inference for Lam (Hindley-Milner, algorithm W):
--   find the type of every expression, without any type written.
-- Lam の型推論（Hindley-Milner、W アルゴリズム）：
-- 型を1つも書かなくても、すべての式の型を見つける。
module Lam.Types
  ( Type (..)
  , inPlace
  , Scheme (..)
  , Class (..)
  , builtinClasses
  , className
  , lookupClass
  , ClassInfo (..)
  , Instance (..)
  , Given
  , dictName
  , dictFor
  , dictParam
  , dictNeeds
  , superClosure
  , typeHead
  , Needs
  , TypeEnv
  , TypeError (..)
  , DataTable (..)
  , Alias (..)
  , DataType (..)
  , Constructor (..)
  , noData
  , members
  , derive
  , derivable
  , fieldsOf
  , tString
  , tFloat
  , defaultNumbers
  , renderUsual
  , tList
  , tTuple
  , tMap
  , tSet
  , tIO
  , tUnit
  , appT
  , closeOver
  , closeOverWith
  , annotationScheme
  , toType
  , varsIn
  , fitsDefault
  , inferType
  , typesAt
  , inferIO
  , inferDefinition
  , inferGroup
  , inferAnnotated
  , checkAnnotated
  , checkInstanceSupers
  , substitute
  , substituteVars
  , renderType
  , renderScheme
  , renderTypeError
  ) where

import Control.Monad (foldM, forM, forM_, unless, when)
import Data.List (nub, partition, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Syntax
import Lam.Sugar (doBind, droppedLine, tryBranch)

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
  -- ANCHOR: tapp
  -- A type in a hole whose type is not known yet:  f a.  When f
  -- turns out to be List, f a is [a] (see appT).
  -- まだわからない穴に入った型：f a。f が List だとわかれば、
  -- f a は [a] になる（appT を見る）
  | TApp Type Type      -- f a
  -- ANCHOR_END: tapp
  -- ANCHOR: trecord
  -- A record: the fields we know, and maybe a type variable for the
  -- other fields (the "rest of the row").
  --   { name : String, age : Int }   is  TRecord {age, name} Nothing
  --   { r | name : String }          is  TRecord {name} (Just r)
  -- レコード：わかっているフィールドと、ほかのフィールドを表す型変数
  -- （行の残り）。Nothing なら、ほかのフィールドはない
  | TRecord (Map Name Type) (Maybe Int)
  -- ANCHOR_END: trecord
  deriving (Eq, Ord, Show)

-- ANCHOR: tcon
-- 🧱 The types made with TCon.
-- TCon で作る型
tString :: Type
tString = TCon "String" []          -- String

-- ANCHOR: tfloat
tFloat :: Type
tFloat = TCon "Float" []            -- Float: 1.5, 1e3
-- ANCHOR_END: tfloat

tList :: Type -> Type
tList a = TCon "List" [a]           -- [a]

tTuple :: [Type] -> Type
tTuple ts = TCon "Tuple" ts         -- (a, b)

-- ANCHOR: tmap
-- A map from keys of type k to values of type v, and a set of a.
-- k 型のキーから v 型の値への Map と、a の Set
tMap :: Type -> Type -> Type
tMap k v = TCon "Map" [k, v]        -- Map k v

tSet :: Type -> Type
tSet a = TCon "Set" [a]             -- Set a
-- ANCHOR_END: tmap

-- ANCHOR: tio
-- An action that, once done, gives an a; and (), the tuple of nothing.
-- やり終えると a をくれる頼みごと。() は、何も入っていないタプル
tIO :: Type -> Type
tIO a = TCon "IO" [a]               -- IO a

tUnit :: Type
tUnit = tTuple []                   -- ()
-- ANCHOR_END: tio
-- ANCHOR_END: tcon

-- ANCHOR: class
-- 🎓 The classes: what a type variable may be. The first four are
--    builtin: the language decides their members. A program can
--    declare more (class Describe a where ...), with instances.
-- クラス：型変数がなれるもの。はじめの4つは組み込みで、仲間は言語が
-- 決める。プログラムは、クラスを宣言して増やせる（インスタンスも）
data Class
  = Comparable    -- == < ... : Int, Bool, String, lists and tuples of them
  | Semigroup     -- ++       : String, [a]
  | HasLength     -- length   : String, [a]
  -- ANCHOR: classFloat
  | Number        -- + - *  and 1, 2  : Int, Float
  -- ANCHOR_END: classFloat
  -- ANCHOR: classKey
  | KeyComparable -- a key of a Map or a Set: the Comparable types
  -- ANCHOR_END: classKey
  -- ANCHOR: classShowable
  | Showable      -- show     : what can be text (not a function)
  -- ANCHOR_END: classShowable
  -- ANCHOR: classDeclared
  | Declared Name -- class Describe a where ...  (its members: instances)
  -- ANCHOR_END: classDeclared
  deriving (Eq, Ord, Show)

-- 🧱 The builtin classes.
-- 組み込みのクラス
builtinClasses :: [Class]
builtinClasses =
  [Comparable, Semigroup, HasLength, Number, KeyComparable, Showable]

-- 🏷️ The name of a class, as written.  Declared "Describe" is Describe.
-- 書いたとおりのクラスの名前
className :: Class -> Text
className c = case c of
  Declared n -> n
  _          -> T.pack (show c)

-- 🔎 A class by its name: a builtin one, or one declared so far.
-- 名前でクラスを引く：組み込みか、ここまでに宣言したもの
lookupClass :: DataTable -> Name -> Maybe Class
lookupClass table n =
  case [ c | c <- builtinClasses, className c == n ] of
    c : _ -> Just c
    [] | Map.member n (classes table) -> Just (Declared n)
       | otherwise                    -> Nothing

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
  | UnknownNameNear Name Name   -- String.length, and length is there
  | NotInClass Class Type   -- a -> a is not Comparable
  -- ANCHOR: orElseNeedsType
  -- 3 ?: 0: the left of ?: is not a Maybe or a Result.
  -- ?: の左が Maybe でも Result でもない
  | OrElseNeeds Type
  -- ANCHOR_END: orElseNeedsType
  -- ANCHOR: dataErrors
  -- Handler is not Comparable: its constructor Button holds Int -> Int.
  -- Handler は比べられない：構成子 Button が Int -> Int を持っている
  | HoldsNotIn Class Type Name Type
  | UnknownConstructor Name           -- case s of | Circel r -> ...
  | WrongPatternArgs Name Int Int     -- Rect w   (needs 2, got 1)
  | UnknownType Name                  -- data T = C Strng
  | UnknownTypeVar Name               -- data T = C a   (a not after T)
  | WrongTypeArgs Name Int Int        -- data T = C Maybe
  | BuiltinType Name                  -- data List a = ...
  -- ANCHOR_END: dataErrors
  -- ANCHOR: recordErrors
  -- { name : String } has no field age.
  -- { name : String } には、フィールド age がない
  | MissingField Name Type
  -- The annotation of next says a can be any type, but the definition
  -- makes it Int.
  -- next の注釈は「a はどんな型でもよい」と言うが、定義は Int にする
  | TooGeneral Name Name Type
  -- The annotation of swap says a and b may differ, but they are the same.
  -- swap の注釈は「a と b はちがってよい」と言うが、同じになる
  | SameVariables Name Name Name
  -- The annotation of largest must say Comparable a.
  -- largest の注釈には、Comparable a が要る
  | AnnotationNeeds Name Class Name
  | UnknownClass Name                 -- Comprable a => ...
  -- r is the rest of a record, so it cannot be a type as well.
  -- r はレコードの残りなので、型としては使えない
  | RowAndType Name
  | DuplicateField Name               -- { age : Int, age : Int }
  -- ANCHOR: defaultError
  -- The default does not fit the type:  count : Int = "one"
  -- 既定値が型に合わない
  | BadDefault Name Type
  -- ANCHOR_END: defaultError
  -- ANCHOR_END: recordErrors
  -- ANCHOR: classErrors
  -- describe [] : the type inside is not known, so which instance?
  -- 中の型がわからないので、どのインスタンスか決められない
  | AmbiguousClass Class Type
  | DuplicateClass Name               -- class Describe a, twice
  | BuiltinClassInstance Class        -- instance Comparable Shape
  | DuplicateInstance Class Name      -- instance Describe Shape, twice
  | InstanceType Text                 -- instance Describe (Int -> Int)
  -- ANCHOR: instanceKindType
  -- instance Foldable Int: Foldable needs a type with a hole (1 more
  -- type to take), like List or Maybe.
  -- Foldable には、穴のある型（あと1つ型を受け取る型）が要る
  | InstanceKind Name Text Int
  -- ANCHOR_END: instanceKindType
  | ClassVarMissing Name Name         -- describe : Int -> String  (no a)
  | UnknownMethod Name Name           -- describ s = ...
  | MissingMethod Name Name           -- describe is not written
  | DuplicateMethod Name              -- describe s = ..., twice
  -- ANCHOR_END: classErrors
  -- ANCHOR: moduleErrors
  -- JDec is a constructor that the module Json does not show.
  -- JDec は、モジュール Json が見せていない構成子
  | Hidden Name Name
  -- data Maybe ... again: the type Maybe belongs to the module Maybe.
  -- The text says what it is: a type, a type alias, or a class.
  -- もう一度 data Maybe：型 Maybe は、モジュール Maybe のもの。文字は
  -- それが何か（型、型の別名、クラス）
  | ForeignType Text Name Name
  -- ANCHOR_END: moduleErrors
  -- ANCHOR: scriptErrors
  -- main has none of the five shapes (see runMain).
  -- main が、5つの形のどれでもない（runMain を見る）
  | BadMain Type
  -- The same error, and where it happened (the innermost place).
  -- 同じエラーと、それが起きた場所（いちばん内側の場所）
  | TypeAt Loc TypeError
  -- ANCHOR_END: scriptErrors
  -- ANCHOR: hardeningErrors
  -- x <- readFile path  gives a Result, but the lines below use x as a
  -- String: the line gives the first type, the lines use the second.
  -- x <- readFile path の答えは Result なのに、下の行は x を String と
  -- して使う：行が出すのが1つ目の型、下の行が使うのが2つ目
  | LineGives Type Type
  -- A ? where the do gives no Result or Maybe: the type of the lines
  -- below it (IO ()).
  -- do が Result も Maybe も出さない所の ?。下の行の型（IO ()）
  | TryNeeds Type
  -- Functions that use each other need a dictionary: write their
  -- annotations.
  -- おたがいを使う関数に、辞書が要る：注釈を書く
  | GroupNeedsAnnotation [Name]
  -- Which definition of a group an error happened in (only inside
  -- inferGroup).
  -- グループのどの定義でエラーが起きたか（inferGroup の中だけ）
  | GroupIn Name TypeError
  -- ANCHOR_END: hardeningErrors
  deriving (Eq, Show)
-- ANCHOR_END: types

-- ANCHOR: dataTable
-- 📚 What the type checker knows about the data types.
-- 型検査器が知っている、data の型
data DataTable = DataTable
  { dataTypes    :: Map Name DataType
  , constructors :: Map Name Constructor
  -- ANCHOR: aliasesField
  -- The type aliases: type alias Member = { name : String, days : Int }
  -- 型の別名
  , aliases      :: Map Name Alias
  -- ANCHOR_END: aliasesField
  -- ANCHOR: classesField
  -- The declared classes, and their instances by the class and the
  -- name of the type:  (Describe, Maybe).
  -- 宣言したクラスと、そのインスタンス（クラスと型の名前で引く）
  , classes      :: Map Name ClassInfo
  , instances    :: Map (Name, Name) Instance
  -- ANCHOR_END: classesField
  }

-- ANCHOR: classInfo
-- 🎓 A declared class: the classes above it, and its methods with
--    their types.  class Describe a where describe : a -> String
--    has  describe : forall a. Describe a => a -> String.
-- 宣言したクラス：上位のクラスと、メソッドとその型
data ClassInfo = ClassInfo
  { classSuper      :: [Class]
    -- ANCHOR: classKindField
    -- How many types the class's type variable takes: 0 for
    -- class Describe a (a alone), 1 for class Foldable f (f a).
    -- クラスの型変数が受け取る型の数。Describe a は 0、Foldable f は 1
  , classKind       :: Int
    -- ANCHOR_END: classKindField
    -- Each method's type, and the names of its type variables as
    -- written (for the errors).
    -- メソッドの型と、書いたとおりの型変数の名前（エラーのため）
  , classMethodsOf  :: [(Name, Scheme, Map Int Name)]
  , classDefaultsOf :: [Name]          -- the methods with a default
  }

-- 🧩 One instance:  instance Describe a => Describe (Maybe a)  has 1
--    type variable, and needs Describe of it (number 0).
-- インスタンス1つ：型変数の数と、型変数に要るクラス
data Instance = Instance
  { instArity :: Int
  , instNeeds :: [(Class, Int)]
  }
-- ANCHOR_END: classInfo

-- ANCHOR: alias
-- 🏷️ A type alias: how many type arguments, and the type it stands for
--    (the arguments are t0, t1, ...).  type alias Pair a = (a, a)  is
--    Alias 1 (t0, t0).
-- 型の別名：型引数の数と、表す型（型引数は t0、t1、...）
--    The last is the type as written, for the command line: its
--    records keep the lines of help, the defaults and the short names.
-- 最後は書いたとおりの型。コマンドラインのため：そのレコードは、
-- ヘルプの行・既定値・短い名前を持っている
data Alias = Alias Int Type TypeExpr
-- ANCHOR_END: alias

-- ANCHOR: dataType
-- 📐 One data type.  data Maybe a = Nothing | Just a  has 1 type
--    variable, and is Comparable when a (t0) is, and Showable when a
--    is: {Comparable := [0], Showable := [0]}.
-- data の型1つ。型変数の数と、クラスごとの、そのクラスに入る条件
data DataType = DataType
  { typeArity :: Int
    -- For each class it can be in: which type variables must be in
    -- the class too (by number). A class not here: never.
    -- 入れるクラスごとに、同じクラスに入っていなければならない型変数
    -- （番号）。ここにないクラスには、決して入らない
  , derived   :: Map Class [Int]
  }
-- ANCHOR_END: dataType

-- 🧱 One constructor: Just is the 2nd (number 1) of Maybe, with 1
--    argument, and has the type  forall a. a -> Maybe a.
-- 構成子1つ：どの型の何番目か、引数の数、型
data Constructor = Constructor
  { conTypeName :: Name
  , conIndex    :: Int
  , conArity    :: Int
  , conScheme   :: Scheme
  }

-- 📚 No data types yet.
-- data の型が、まだない
noData :: DataTable
noData = DataTable Map.empty Map.empty Map.empty Map.empty Map.empty
-- ANCHOR_END: dataTable

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
  -- ANCHOR: applyApp
  TApp f a  -> appT (applyT s f) (applyT s a)
  -- ANCHOR_END: applyApp
  -- ANCHOR: applyRecord
  -- When the rest r turned out to be more fields (and another rest),
  -- put them together:  { r | name : a }  with  r := { s | age : Int }
  -- is  { s | age : Int, name : a }.
  -- 残りの r が、もっとフィールド（と別の残り）だとわかったら、合わせる
  TRecord fs rest ->
    let fs' = Map.map (applyT s) fs
    in case rest >>= (`Map.lookup` s) of
         Just (TVar m)             -> TRecord fs' (Just m)
         Just (TRecord more rest') -> TRecord (Map.union fs' more) rest'
         _                         -> TRecord fs' rest
  -- ANCHOR_END: applyRecord
  _         -> t
-- ANCHOR_END: applyT

-- ANCHOR: appT
-- 🕳️ Put a type into the hole. When f turned out to be a named type
--    with a hole (List, Map String, Result e), the hole is filled:
--    f Int  with  f := List  is  [Int],  with  f := Map String  is
--    Map String Int.
-- 穴に型を入れる。f が、穴のあいた名前の型（List、Map String、
-- Result e）だとわかったら、穴を埋める
appT :: Type -> Type -> Type
appT f a = case f of
  TCon c ts -> TCon c (ts <> [a])
  _         -> TApp f a
-- ANCHOR_END: appT

-- 🔁 Put the types into the type variables t0, t1, ... (for aliases).
-- 型変数 t0、t1、... に、型を入れる（別名のため）
substitute :: [Type] -> Type -> Type
substitute ts = applyT (Map.fromList (zip [0 ..] ts))

-- 🔁 Put types into some type variables, by their numbers.
-- いくつかの型変数に、番号で型を入れる
substituteVars :: Map Int Type -> Type -> Type
substituteVars = applyT

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
-- ANCHOR: unifyHead
-- 🤝 Make two types the same, if we can, and say how.
--    Int -> t0  and  t1 -> Bool  give {t1 := Int, t0 := Bool}.
--    It runs in Infer, because two records may need a new type
--    variable for the rest of their fields.
-- 2つの型を同じにする（できれば）。その方法を返す。
-- 2つのレコードは、残りのフィールドに新しい型変数が要ることがあるので、
-- Infer の中で動かす
unify :: Type -> Type -> Infer Subst
unify a b = case (a, b) of
  (TInt, TInt)   -> pure Map.empty
  (TBool, TBool) -> pure Map.empty
  (TVar n, t)    -> bind n t
  (t, TVar n)    -> bind n t
  (TFun a1 r1, TFun a2 r2) -> do
    s1 <- unify a1 a2
    s2 <- unify (applyT s1 r1) (applyT s1 r2)
    pure (compose s2 s1)
  -- ANCHOR_END: unifyHead
  -- ANCHOR: unifyCon
  -- The same name and the same number of types inside: make the
  -- insides the same, one by one.  [t0] and [Int] give {t0 := Int}.
  -- 名前も中の型の数も同じなら、中の型を1つずつ同じにする
  (TCon c1 ts1, TCon c2 ts2)
    | c1 == c2 && length ts1 == length ts2 -> unifyMany ts1 ts2
  -- ANCHOR_END: unifyCon
  -- ANCHOR: unifyApp
  -- f a  and  Map String Int: f is Map String (a hole is left), and
  -- a is Int. The last type inside is the one in the hole.
  -- f a と Map String Int：f は Map String（穴が1つ残る）、a は Int。
  -- 中の最後の型が、穴の中身
  (TApp f x, TApp g y) -> unifyMany [f, x] [g, y]
  (TApp f x, TCon c ts@(_ : _)) ->
    unifyMany [f, x] [TCon c (init ts), last ts]
  (TCon c ts@(_ : _), TApp f x) ->
    unifyMany [TCon c (init ts), last ts] [f, x]
  -- ANCHOR_END: unifyApp
  -- ANCHOR: unifyRecord
  -- Two records: the fields both have must be the same; the fields
  -- only one has go into the rest of the other (see unifyRests).
  -- 2つのレコード：両方にあるフィールドは同じ型に。片方にしかない
  -- フィールドは、もう片方の残りに入れる（unifyRests を見る）
  (TRecord f1 r1, TRecord f2 r2) -> do
    let both = Map.elems (Map.intersectionWith (,) f1 f2)
    s1 <- unifyMany (map fst both) (map snd both)
    let left  = applyT s1 (TRecord (Map.difference f1 f2) r1)
        right = applyT s1 (TRecord (Map.difference f2 f1) r2)
    s2 <- unifyRests a b left right
    pure (compose s2 s1)
  -- ANCHOR_END: unifyRecord
  _ -> failWith (Mismatch a b)

-- ANCHOR: unifyRests
-- 🧩 Two records with no field in common (what is left of them).
--    Each one's rest must hold the other's fields:
--      { r | name : a }  and  { s | age : Int }
--    give  r := { t | age : Int },  s := { t | name : a }  (t is new).
--    A record with no rest (closed) cannot take more fields.
-- 共通のフィールドがない2つのレコード（残ったもの）。それぞれの残りが、
-- 相手のフィールドを持たなければならない。残りのない（閉じた）
-- レコードは、フィールドを増やせない
unifyRests :: Type -> Type -> Type -> Type -> Infer Subst
unifyRests whole1 whole2 left right = case (left, right) of
  (TRecord f1 r1, TRecord f2 r2) -> case (r1, r2) of
    (Nothing, _) | Just x <- firstKey f2 -> failWith (MissingField x whole1)
    (_, Nothing) | Just x <- firstKey f1 -> failWith (MissingField x whole2)
    (Nothing, Nothing) -> pure Map.empty
    (Just n, Nothing)  -> bind n (TRecord f2 Nothing)
    (Nothing, Just m)  -> bind m (TRecord f1 Nothing)
    (Just n, Just m)
      | n == m -> if Map.null f1 && Map.null f2
                    then pure Map.empty
                    else failWith (Mismatch whole1 whole2)
      | Map.null f2 -> bind m (row f1 n)
      | Map.null f1 -> bind n (row f2 m)
      | otherwise -> do
          t  <- freshRow
          s1 <- bind n (TRecord f2 (Just t))
          s2 <- bind m (TRecord f1 (Just t))
          pure (compose s2 s1)
  _ -> unify left right
  where
    firstKey fs = fst <$> Map.lookupMin fs
    -- No more fields: the rest is just the other rest.
    -- フィールドが増えないなら、残りは相手の残りそのもの
    row fs r = if Map.null fs then TVar r else TRecord fs (Just r)
-- ANCHOR_END: unifyRests

-- ANCHOR: unifyMany
-- 🤝 unify, for two lists of types side by side.
-- 並んだ2つの型のリストを、順に unify する
unifyMany :: [Type] -> [Type] -> Infer Subst
unifyMany as bs = case (as, bs) of
  (a : as', b : bs') -> do
    s1 <- unify a b
    s2 <- unifyMany (map (applyT s1) as') (map (applyT s1) bs')
    pure (compose s2 s1)
  _ -> pure Map.empty
-- ANCHOR_END: unifyMany

-- 📌 t_n := t. But not t_n := t_n -> ... (that type would be infinite).
-- t_n := t。ただし t_n := t_n -> ... はだめ（無限に大きな型になる）
bind :: Int -> Type -> Infer Subst
bind n t
  | t == TVar n          = pure Map.empty
  | n `Set.member` ftv t = failWith (InfiniteType n t)
  | otherwise            = pure (Map.singleton n t)

-- 🔓 The type variables inside a type.
-- 型の中の型変数
-- ANCHOR: ftv
ftv :: Type -> Set Int
ftv t = case t of
  TVar n    -> Set.singleton n
  TFun a b  -> ftv a <> ftv b
  TCon _ ts -> foldMap ftv ts
  TApp f a  -> ftv f <> ftv a
  TRecord fs rest -> foldMap ftv fs <> maybe Set.empty Set.singleton rest
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
-- ANCHOR: inferState
data InferState = InferState
  { nextVar :: Int
  , needs   :: Needs
  , known   :: DataTable     -- read only / 読むだけ
  -- ANCHOR: holesField
  -- The holes left for dictionaries: a name, the class, and the type.
  -- 辞書のために空けておいた穴：名前、クラス、型
  , holes   :: [(Name, Class, Type)]
  -- ANCHOR_END: holesField
  -- ANCHOR: pendingField
  -- The classes that wait for an f: HasLength (f Int) can be
  -- checked only when f is known.
  -- f を待っている条件。HasLength (f Int) は、f がわかるまで確かめられない
  , pending :: [(Class, Type)]
  -- ANCHOR_END: pendingField
  -- ANCHOR: localsField
  -- The local names here: the arguments of the lambdas around, the
  -- names of the patterns, and the lets that were not made general.
  -- A let that uses none of them is closed (see inferLet).
  -- ここで見えるローカルな名前：外側のラムダの引数、パターンの名前、
  -- 一般にしなかった let。どれも使わない let は「閉じている」
  , locals  :: Set Name
  -- ANCHOR_END: localsField
  -- ANCHOR: marksField
  -- The type found at each mark of a place (At), for the hover of an
  -- editor. The types are made whole at the end (see typesAt).
  -- 場所の印（At）ごとに見つけた型。エディターのホバーのため。型は最後に
  -- 仕上げる（typesAt を見る）
  , marks   :: [(Loc, Type)]
  -- ANCHOR_END: marksField
  }

newtype Infer a = Infer (InferState -> Either TypeError (a, InferState))

runInfer :: DataTable -> Infer a -> Either TypeError a
runInfer table (Infer f) =
  fst <$> f (InferState 0 Map.empty table [] [] Set.empty [])

-- 📚 The data types we know.
-- 知っている data の型
knownData :: Infer DataTable
knownData = Infer (\st -> Right (known st, st))
-- ANCHOR_END: inferState

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

-- ANCHOR: newHole
-- 🕳️ A hole for a dictionary: describe x  becomes  describe $hole7,
--    and $hole7 is filled when the type is known (see fillHoles).
-- 辞書のための穴。describe x は describe $hole7 になり、型がわかったら
-- $hole7 を埋める（fillHoles を見る）
newHole :: Class -> Type -> Infer Expr
newHole c t = Infer $ \st ->
  let h = "$hole" <> T.pack (show (nextVar st))
  in Right (Var h, st { nextVar = nextVar st + 1
                      , holes = (h, c, t) : holes st })
-- ANCHOR_END: newHole

-- ANCHOR: freshRow
-- 🆕 A new type variable, by its number: for the rest of a record.
-- 新しい型変数を、番号で。レコードの残りのため
freshRow :: Infer Int
freshRow = Infer $ \st -> Right (nextVar st, st { nextVar = nextVar st + 1 })
-- ANCHOR_END: freshRow

-- ❌ Stop with an error.
-- エラーで止まる
failWith :: TypeError -> Infer a
failWith err = Infer (\_ -> Left err)

-- ANCHOR: scoped
-- 🏠 Run m with the local names changed, then put them back.
-- ローカルな名前を変えて m を動かし、元に戻す
scoped :: (Set Name -> Set Name) -> Infer a -> Infer a
scoped change m = do
  old <- Infer (\st -> Right (locals st, st))
  Infer (\st -> Right ((), st { locals = change old }))
  a <- m
  Infer (\st -> Right ((), st { locals = old }))
  pure a

-- 🏠 The local names here.
-- ここで見えるローカルな名前
localNames :: Infer (Set Name)
localNames = Infer (\st -> Right (locals st, st))
-- ANCHOR_END: scoped

-- ANCHOR: attempt
-- 🧪 Try m: its error, or what it gave. On an error, nothing of m is
--    kept (the state goes back to before).
-- m を試す。エラーか、くれたもの。エラーなら、m がしたことは何も残らない
attempt :: Infer a -> Infer (Either TypeError a)
attempt (Infer f) = Infer $ \st -> case f st of
  Left err         -> Right (Left err, st)
  Right (a, after) -> Right (Right a, after)

-- 🧪 Would m go through? Nothing of m is kept, either way.
-- m が通るか。どちらでも、m がしたことは残らない
fitsAlone :: Infer a -> Infer Bool
fitsAlone (Infer f) = Infer $ \st ->
  Right (either (const False) (const True) (f st), st)
-- ANCHOR_END: attempt

-- ANCHOR: inPlace
-- 📍 Put the place on an error inside. An error that already has a
--    place keeps it, as in the evaluator.
-- 中で起きたエラーに場所を付ける。もう場所のあるエラーは、評価器と
-- 同じく、そのまま
inPlace :: Loc -> Infer a -> Infer a
inPlace loc (Infer f) = Infer $ \st -> case f st of
  Left err@(TypeAt _ _) -> Left err
  Left err              -> Left (TypeAt loc err)
  Right done            -> Right done
-- ANCHOR_END: inPlace

-- ANCHOR: atArgument
-- 📍 A call whose argument does not fit: the place is the argument's
--    (its mark), not the call's. An argument with no mark (made by
--    the sugar, not written) leaves the place to the call.
-- 引数が合わない呼び出し：場所は呼び出しではなく、引数のもの（その印）。
-- 印のない引数（書いたものではなく、糖衣構文が作ったもの）なら、
-- 場所は呼び出しに任せる
atArgument :: Expr -> Infer a -> Infer a
atArgument arg m = case arg of
  At loc _ -> inPlace loc m
  _        -> m
-- ANCHOR_END: atArgument

-- ANCHOR: liftUnify
-- 🤝 unify, inside Infer. Then check the classes: when t3 must be
--    Comparable and t3 turned out to be Int -> Int, that is an error.
-- Infer の中の unify。そのあと、クラスの条件を確かめる
liftUnify :: Type -> Type -> Infer Subst
liftUnify a b = do
  s <- unify a b
  current <- currentNeeds
  sequence_
    [ require c t
    | (n, t) <- Map.toList s
    , c <- maybe [] Set.toList (Map.lookup n current) ]
  recheck s
  pure s
-- ANCHOR_END: liftUnify

-- ANCHOR: pending
-- ⏳ Apply what we learned to the waiting classes. Those whose f is
--    known now (f a became [Int]) are checked as usual.
-- 待っている条件に、わかったことを当てはめる。f がわかったもの
-- （f a が [Int] になった）は、ふつうに確かめる
recheck :: Subst -> Infer ()
recheck s = do
  waiting <- Infer $ \st -> Right (pending st, st { pending = [] })
  mapM_ (\(c, t) -> require c (applyT s t)) waiting

-- 🧺 Before a let gets its forall type: an f that is still not known
--    but has a class waiting on it becomes a list (like the defaulting
--    of Haskell). So  let average xs = sum xs / toFloat (length xs)
--    is for lists:  [Float] -> Float.
-- let の型を forall にする前に：まだわからず、条件が待っている f は、
-- リストにする（Haskell の defaulting のように）
settle :: Infer Subst
settle = do
  waiting <- Infer $ \st -> Right (pending st, st)
  case [ n | (_, t) <- waiting, n <- holeHeads t ] of
    []    -> pure Map.empty
    n : _ -> do
      s1 <- liftUnify (TVar n) (TCon "List" [])
      s2 <- settle
      pure (s2 `compose` s1)
  where
    holeHeads t = case t of
      TApp (TVar n) _ -> [n]
      TApp f _        -> holeHeads f
      _               -> []
-- ANCHOR_END: pending
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
  -- ANCHOR: requireApp
  -- f a, with f not known yet: wait (see recheck). But a number is
  -- never one:  sum 5  stops here, with 5 (an Int) where f a is needed.
  -- f a の f がまだわからない：待つ（recheck を見る）。ただし、数は
  -- 決して f a にならない。sum 5 は、ここで止まる
  TApp _ _
    | c == Number -> failWith (Mismatch TInt t)
    | otherwise -> Infer $ \st ->
    Right ((), st { pending = (c, t) : pending st })
  -- ANCHOR_END: requireApp
  TVar n -> do
    Infer $ \st ->
      let more = Map.insertWith (<>) n (Set.singleton c) (needs st)
      in Right ((), st { needs = more })
    -- ANCHOR: numberClash
    -- A number (Int or Float) can never be in some classes: length 5
    -- is an error now, as Int is not in HasLength.
    -- 数（Int か Float）が決して入れないクラスもある。length 5 は、
    -- いまエラーにする。Int は HasLength に入らないので
    cs    <- Map.findWithDefault Set.empty n <$> currentNeeds
    table <- knownData
    let neither k = all (\ty -> inClass table k ty == Nothing)
                        [TInt, tFloat]
    case [ k | Number `Set.member` cs, k <- Set.toList cs, neither k ] of
      k : _ -> failWith (NotInClass k TInt)
      []    -> pure ()
    -- ANCHOR_END: numberClash
  _ -> do
    table <- knownData
    case inClass table c t of
      Just inside -> mapM_ (uncurry require) inside
      Nothing     -> failWith (notIn table)
  where
    -- For a data type, look at its constructors again, to say which
    -- one keeps it out.
    -- data の型なら、構成子を見直して、どれのせいかを言う
    -- ANCHOR: notIn
    notIn table = case t of
      TCon name ts
        | Just k <- derivedAs c
        , Left (con, inside) <- derive table k name (fieldsOf table name) ->
            HoldsNotIn k t con
              (applyT (Map.fromList (zip [0 ..] ts)) inside)
      _ -> NotInClass c t
    -- A key is Comparable: say Comparable for it.
    -- キーは Comparable のこと。Comparable と言う
    derivedAs k = case k of
      KeyComparable -> Just Comparable
      _ | k `elem` derivable -> Just k
        | otherwise          -> Nothing
    -- ANCHOR_END: notIn

-- 🧱 The constructors of a data type, with the types of their arguments.
-- data の型の構成子と、その引数の型
fieldsOf :: DataTable -> Name -> [(Name, [Type])]
fieldsOf table name =
  [ (c, fst (splitArgs (conArity con) t))
  | (c, con) <- sortOn (conIndex . snd) (Map.toList (constructors table))
  , conTypeName con == name
  , let Forall _ _ t = conScheme con ]

-- ANCHOR: inClass
-- 🎓 Is t in class c? Nothing if not; otherwise the classes the types
--    inside must be in too. A declared class asks its instances.
-- t はクラス c に入るか。入らなければ Nothing。入るなら、中の型が
-- 入らなければならないクラス。宣言したクラスは、インスタンスに聞く
inClass :: DataTable -> Class -> Type -> Maybe [(Class, Type)]
inClass table c t = case c of
  Declared n -> instanceFor table n t
  _          -> map ((,) c) <$> members table c t

-- 🧩 The instance of class n for t:  Describe (Maybe Shape)  finds
--    instance Describe a => Describe (Maybe a), and needs Describe Shape.
-- t のための、クラス n のインスタンス
instanceFor :: DataTable -> Name -> Type -> Maybe [(Class, Type)]
instanceFor table n t = do
  (h, args) <- typeHead t
  inst <- Map.lookup (n, h) (instances table)
  if length args /= instArity inst
    then Nothing
    else Just [ (k, args !! i) | (k, i) <- instNeeds inst ]

-- 🏷️ The name of a type, and the types inside:  Maybe Int  is
--    (Maybe, [Int]), and [a] is (List, [a]).  A function, a record or
--    a type variable has none.
-- 型の名前と中の型。関数・レコード・型変数には、ない
typeHead :: Type -> Maybe (Name, [Type])
typeHead t = case t of
  TInt      -> Just ("Int", [])
  TBool     -> Just ("Bool", [])
  TCon c ts -> Just (c, ts)
  _         -> Nothing
-- ANCHOR_END: inClass

-- ANCHOR: superClosure
-- 🪜 A class and every class above it, all the way up.
--    class Describe a => Ranked a  makes Ranked give Ranked and Describe.
-- クラスと、その上にあるクラスぜんぶ
superClosure :: DataTable -> Class -> Set Class
superClosure table c0 = go Set.empty [c0]
  where
    go seen cs = case cs of
      [] -> seen
      c : rest
        | c `Set.member` seen -> go seen rest
        | otherwise           -> go (Set.insert c seen) (above c <> rest)
    above c = case c of
      Declared n -> maybe [] classSuper (Map.lookup n (classes table))
      -- ANCHOR: keyAbove
      -- A Comparable type is a KeyComparable type too.
      -- Comparable の型は、KeyComparable の型でもある
      Comparable -> [KeyComparable]
      -- ANCHOR_END: keyAbove
      _          -> []

-- 🧹 Leave out a class that another class of the same type variable
--    brings with it:  (Describe a, Ranked a)  is just  Ranked a.
-- 同じ型変数の、ほかのクラスが連れてくるクラスは外す
simplifyNeeds :: DataTable -> Needs -> Needs
simplifyNeeds table = Map.map keep
  where
    keep cs = Set.filter (\c -> not (any (brings c) (Set.toList cs))) cs
    brings c k = k /= c && c `Set.member` superClosure table k
-- ANCHOR_END: superClosure

-- 📖 The members of each class: Nothing if t is not one; otherwise
--    the types inside that must be in the class too.
-- クラスの仲間の表。仲間でなければ Nothing。仲間なら、
-- 同じクラスに入っていなければならない、中の型
members :: DataTable -> Class -> Type -> Maybe [Type]
members table c t = case (c, t) of
  -- ANCHOR: membersKey
  -- The keys: the same members as Comparable.
  -- キー：仲間は Comparable と同じ
  (KeyComparable, _)              -> members table Comparable t
  -- ANCHOR_END: membersKey
  (Comparable, TInt)              -> Just []
  -- ANCHOR: membersFloat
  (Comparable, TCon "Float" [])   -> Just []
  -- The numbers: + - * and the literals 1, 2 work on these two.
  -- 数。+ - * と、1 や 2 のリテラルは、この2つに使える
  (Number,     TInt)              -> Just []
  (Number,     TCon "Float" [])   -> Just []
  -- ANCHOR_END: membersFloat
  (Comparable, TBool)             -> Just []
  (Comparable, TCon "String" [])  -> Just []
  (Comparable, TCon "List" [a])   -> Just [a]
  (Comparable, TCon "Tuple" ts)   -> Just ts
  -- ANCHOR: membersShowable
  -- Showable: the same as Comparable, and Float, records and data
  -- types the same way (a function is in neither).
  -- Showable：Comparable と同じ。関数は、どちらにも入らない
  (Showable, TInt)                -> Just []
  (Showable, TCon "Float" [])     -> Just []
  (Showable, TBool)               -> Just []
  (Showable, TCon "String" [])    -> Just []
  (Showable, TCon "List" [a])     -> Just [a]
  (Showable, TCon "Tuple" ts)     -> Just ts
  (Showable, TCon "Map" [k, v])   -> Just [k, v]
  (Showable, TCon "Set" [a])      -> Just [a]
  (Showable, TRecord fs rest)     ->
    Just (Map.elems fs <> maybe [] (\r -> [TVar r]) rest)
  -- ANCHOR_END: membersShowable
  -- ANCHOR: membersMap
  -- Map k v is Comparable when k and v are, like the pairs it holds.
  -- Set a is when a is, like [a].
  -- Map k v は、持っている組と同じく、k と v が Comparable なら
  -- Comparable。Set a は、[a] と同じく、a が Comparable なら
  (Comparable, TCon "Map" [k, v]) -> Just [k, v]
  (Comparable, TCon "Set" [a])    -> Just [a]
  -- ANCHOR_END: membersMap
  (Semigroup,  TCon "String" [])  -> Just []
  (Semigroup,  TCon "List" [_])   -> Just []
  (HasLength,  TCon "String" [])  -> Just []
  (HasLength,  TCon "List" [_])   -> Just []
  -- ANCHOR: membersBytes
  -- Bytes can be compared (byte by byte, as a key of a Map too), shown,
  -- and measured (length is the number of bytes).
  -- Bytes は、比べられ（1バイトずつ。Map のキーにも）、見せられ、
  -- 長さを測れる（length はバイトの数）
  (Comparable, TCon "Bytes" [])   -> Just []
  (Showable,   TCon "Bytes" [])   -> Just []
  (HasLength,  TCon "Bytes" [])   -> Just []
  -- ANCHOR_END: membersBytes
  -- ANCHOR: membersRecord
  -- A record is Comparable when every field is. The rest r, if any,
  -- must be too: every field it will hold.
  -- レコードは、どのフィールドも Comparable なら Comparable。
  -- 残りの r があれば、r も（r が持つことになる、どのフィールドも）
  (Comparable, TRecord fs rest)   ->
    Just (Map.elems fs <> maybe [] (\r -> [TVar r]) rest)
  -- ANCHOR_END: membersRecord
  -- ANCHOR: membersData
  -- A data type: the rows its declaration added, for any class.
  -- Maybe Int needs Int (its argument number 0) to be Comparable.
  -- data の型：宣言が足した行（どのクラスでも）。Maybe Int は、
  -- 0番目の引数 Int が Comparable であればよい
  (cls, TCon name ts)
    | Just dt <- Map.lookup name (dataTypes table)
    , length ts == typeArity dt
    , Just needed <- Map.lookup cls (derived dt) ->
        Just [ ts !! i | i <- needed ]
  -- ANCHOR_END: membersData
  _                               -> Nothing
-- ANCHOR_END: require

-- ANCHOR: derive
-- 🧬 The classes a data type joins by what it holds.
-- 中身から自動で入るクラス
derivable :: [Class]
derivable = [Comparable, Showable]

-- ⚖️ Is the new type in class cls, and when?  Look at every argument
--    of every constructor:
--      a type variable    -> it must be in cls
--      the type itself    -> skip it (we are deciding that now)
--      anything else      -> ask the table of members, as require does
--    for Comparable:
--    data Maybe a = Nothing | Just a      gives Right [0]   (a must be)
--    data Tagged a = Tagged Int           gives Right []    (always)
--    data Handler = Button (Int -> Int)   gives Left (Button, Int -> Int)
-- 新しい型はクラス cls に入るか、どんなときか。どの構成子のどの引数も見る
derive
  :: DataTable -> Class -> Name -> [(Name, [Type])]
  -> Either (Name, Type) [Int]
derive table cls self fields =
  Set.toList <$> foldM add Set.empty
    [ (c, t) | (c, ts) <- fields, t <- ts ]
  where
    add acc (c, t) = case needs t of
      Just vs -> Right (acc <> vs)
      Nothing -> Left (c, t)

    -- The type variables t needs to be in cls, or Nothing if it can
    -- never be.
    -- t が cls に入るために、cls に入っていなければならない型変数
    needs :: Type -> Maybe (Set Int)
    needs t = case t of
      TVar i -> Just (Set.singleton i)
      TCon name _ | name == self -> Just Set.empty
      _ -> case members table cls t of
        Just inside -> Set.unions <$> traverse needs inside
        Nothing     -> Nothing
-- ANCHOR_END: derive


-- ANCHOR: scheme
-- 🧬 forall a. a -> a  becomes  t7 -> t7  (new variables each use).
--    The new variables get the same classes as the old ones.
-- 使うたびに、新しい型変数にする。クラスの条件も、新しい型変数に写す
--    A declared class leaves a hole for its dictionary.
--    宣言したクラスは、辞書のための穴を空ける
instantiate :: Scheme -> Infer (Type, [Expr])
instantiate (Forall vs ns t) = do
  news <- mapM (const fresh) vs
  let sub = Map.fromList (zip vs news)
  sequence_
    [ require c new
    | (v, new) <- zip vs news
    , c <- maybe [] Set.toList (Map.lookup v ns) ]
  dicts <- forM (dictNeeds ns) $ \(v, c) ->
    newHole c (applyT sub (TVar v))
  pure (applyT sub t, dicts)

-- ANCHOR: dictNeeds
-- 📋 The dictionaries a type needs, in a fixed order (by the type
--    variable, then the class): the builtin classes need none.
-- 型に要る辞書。決まった順（型変数、クラスの順）。
-- 組み込みのクラスには要らない
dictNeeds :: Needs -> [(Int, Class)]
dictNeeds ns =
  [ (v, c) | (v, cs) <- Map.toList ns, c@(Declared _) <- Set.toList cs ]
-- ANCHOR_END: dictNeeds

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

-- ANCHOR: generalizeIn
-- 🧬 generalize, in Infer: the classes are made as small as they can
--    be. A local let does not generalize a type variable of a declared
--    class: its hole is filled outside, at the definition.
-- Infer の中の generalize。クラスはできるだけ少なくする。中の let は、
-- 宣言したクラスの型変数を「どんな型でもよい」にしない。その穴は、
-- 外の定義で埋める
generalizeIn :: Bool -> TypeEnv -> Type -> Infer Scheme
generalizeIn inner env t = do
  ns    <- currentNeeds
  table <- knownData
  let Forall vs ns' t' = generalize env ns t
      declared v = any isDeclared (Map.findWithDefault Set.empty v ns')
      kept = if inner then filter (not . declared) vs else vs
  pure (Forall kept
          (simplifyNeeds table (Map.restrictKeys ns' (Set.fromList kept)))
          t')
  where
    isDeclared c = case c of
      Declared _ -> True
      _          -> False
-- ANCHOR_END: generalizeIn
-- ANCHOR_END: scheme

-- ANCHOR: annotationScheme
-- ✍️ An annotation, as a type that works for any a, b, ...:
--      largest : Comparable a => [a] -> Maybe a
--    gives  forall t0. Comparable t0 => [t0] -> Maybe t0,  and the
--    names {t0 := a} for the errors. The type variables are numbered
--    in the order they appear.
-- 注釈を、どんな a、b でも使える型にする。型変数は、出てきた順に番号を
-- 付ける。名前の表は、エラーのため
annotationScheme
  :: DataTable -> Signature -> Either TypeError (Scheme, Map Int Name)
annotationScheme table (Signature conds te) = do
  let rows  = nub (restsIn te)
      plain = nub (varsIn te)
      names = nub (rows <> plain)
      vars  = Map.fromList (zip names [0 ..])
  case filter (`elem` plain) rows of
    r : _ -> Left (RowAndType r)
    []    -> Right ()
  t <- toType table vars te
  needs <- traverse (condition vars) conds
  pure ( Forall (Map.elems vars) (Map.fromListWith (<>) needs) t
       , Map.fromList [ (i, x) | (x, i) <- Map.toList vars ] )
  where
    -- Comparable a: the class must be known (builtin, or declared
    -- above), and a must be in the type.
    -- クラスは知っているもの（組み込みか、上で宣言したもの）、
    -- a は型の中にあるもの
    condition vars (c, a) = do
      cls <- maybe (Left (UnknownClass c)) Right (lookupClass table c)
      i <- maybe (Left (UnknownTypeVar a)) Right (Map.lookup a vars)
      pure (i, Set.singleton cls)

-- 🔎 The type variables written in a type, and the rests of records.
-- 型に書かれた型変数と、レコードの残り
varsIn :: TypeExpr -> [Name]
varsIn te = case te of
  TyVar a       -> [a]
  TyApp f ts    -> f : concatMap varsIn ts
  TyCon _ ts    -> concatMap varsIn ts
  TyFun a b     -> varsIn a <> varsIn b
  TyList a      -> varsIn a
  TyTuple ts    -> concatMap varsIn ts
  TyRecord fs _ -> concatMap (varsIn . fieldType) fs

restsIn :: TypeExpr -> [Name]
restsIn te = case te of
  TyVar _       -> []
  TyApp _ ts    -> concatMap restsIn ts
  TyCon _ ts    -> concatMap restsIn ts
  TyFun a b     -> restsIn a <> restsIn b
  TyList a      -> restsIn a
  TyTuple ts    -> concatMap restsIn ts
  TyRecord fs r -> maybe [] pure r <> concatMap (restsIn . fieldType) fs
-- ANCHOR_END: annotationScheme

-- ANCHOR: toType
-- 🏷️ A written type, as a Type.  Maybe a  is  TCon "Maybe" [TVar 0].
--    It takes the whole table, to look up the aliases too.
-- 書かれた型を、Type にする。別名も引けるように、表をまるごと受け取る
toType :: DataTable -> Map Name Int -> TypeExpr -> Either TypeError Type
toType table vars te = case te of
  TyVar a ->
    maybe (Left (UnknownTypeVar a)) (Right . TVar) (Map.lookup a vars)
  -- ANCHOR: toTypeApp
  -- f a: the type in f's hole. When f is known to be List later,
  -- f Int becomes [Int] (see appT).
  -- f a：f の穴に入った型。あとで f が List だとわかれば、f Int は
  -- [Int] になる（appT を見る）
  TyApp f args -> do
    i <- maybe (Left (UnknownTypeVar f)) Right (Map.lookup f vars)
    foldl appT (TVar i) <$> traverse go args
  -- ANCHOR_END: toTypeApp
  TyFun a b -> TFun <$> go a <*> go b
  TyList a -> tList <$> go a
  TyTuple ts -> tTuple <$> traverse go ts
  TyCon "Int" [] -> Right TInt
  TyCon "Bool" [] -> Right TBool
  TyCon "String" [] -> Right tString
  -- ANCHOR: floatType
  TyCon "Float" [] -> Right tFloat
  TyCon "Float" args -> Left (WrongTypeArgs "Float" 0 (length args))
  -- ANCHOR_END: floatType
  -- ANCHOR: toTypeMap
  -- Map String Int, and Set Int
  TyCon "Map" [k, v] -> tMap <$> go k <*> go v
  TyCon "Map" args -> Left (WrongTypeArgs "Map" 2 (length args))
  TyCon "Set" [a] -> tSet <$> go a
  TyCon "Set" args -> Left (WrongTypeArgs "Set" 1 (length args))
  -- ANCHOR_END: toTypeMap
  -- ANCHOR: toTypeIO
  -- IO String
  TyCon "IO" [a] -> tIO <$> go a
  TyCon "IO" args -> Left (WrongTypeArgs "IO" 1 (length args))
  -- ANCHOR_END: toTypeIO
  -- ANCHOR: toTypeBytes
  TyCon "Bytes" [] -> Right (TCon "Bytes" [])
  TyCon "Bytes" args -> Left (WrongTypeArgs "Bytes" 0 (length args))
  -- ANCHOR_END: toTypeBytes
  -- ANCHOR: toTypeRegex
  TyCon "Regex" [] -> Right (TCon "Regex" [])
  TyCon "Regex" args -> Left (WrongTypeArgs "Regex" 0 (length args))
  -- ANCHOR_END: toTypeRegex
  -- ANCHOR: toTypeParser
  -- Parser Int: a parser that gives an Int.
  -- Int をくれるパーサー
  TyCon "Parser" [a] -> TCon "Parser" <$> traverse go [a]
  TyCon "Parser" args -> Left (WrongTypeArgs "Parser" 1 (length args))
  -- ANCHOR: toTypeTask
  -- Task Int: a job started with spawn, that gives an Int.
  -- Task Int：spawn で始めた、Int をくれる仕事
  TyCon "Task" [a] -> TCon "Task" <$> traverse go [a]
  TyCon "Task" args -> Left (WrongTypeArgs "Task" 1 (length args))
  -- ANCHOR_END: toTypeTask
  -- ANCHOR: toTypeChan
  -- Chan Int: a channel of Ints. Pid Msg: a mailbox that takes Msgs.
  -- Chan Int：Int を流すチャネル。Pid Msg：Msg を受け取る受け箱
  TyCon "Chan" [a] -> TCon "Chan" <$> traverse go [a]
  TyCon "Chan" args -> Left (WrongTypeArgs "Chan" 1 (length args))
  TyCon "Pid" [a] -> TCon "Pid" <$> traverse go [a]
  TyCon "Pid" args -> Left (WrongTypeArgs "Pid" 1 (length args))
  -- ANCHOR_END: toTypeChan
  -- ANCHOR_END: toTypeParser
  -- ANCHOR: toTypeRecord
  -- { name : String, age : Int }, and { r | name : String }
  TyRecord fs rest -> do
    let names = map fieldName fs
    case [ f | (i, f) <- zip [0 :: Int ..] names
             , f `elem` drop (i + 1) names ] of
      f : _ -> Left (DuplicateField f)
      []    -> Right ()
    fields <- traverse (\f -> (,) (fieldName f) <$> go (fieldType f)) fs
    -- ANCHOR: checkDefaults
    -- A default must fit the type of its field (count : Int = 1).
    -- 既定値は、そのフィールドの型に合わなければならない
    case [ (f, t) | (FieldType f _ (Just d) _ _, (_, t)) <- zip fs fields
                  , not (fitsDefault table d t) ] of
      (f, t) : _ -> Left (BadDefault f t)
      []         -> Right ()
    -- ANCHOR_END: checkDefaults
    r <- traverse (\a -> maybe (Left (UnknownTypeVar a)) Right
                           (Map.lookup a vars)) rest
    pure (TRecord (Map.fromList fields) r)
  -- ANCHOR_END: toTypeRecord
  -- ANCHOR: toTypeAlias
  -- An alias: the type it stands for, with the arguments put in.
  -- 別名：表す型に、引数を入れたもの
  TyCon c args
    | Just (Alias n body _) <- Map.lookup c (aliases table) -> do
        unless (n == length args) $
          Left (WrongTypeArgs c n (length args))
        ts <- traverse go args
        pure (substitute ts body)
  -- ANCHOR_END: toTypeAlias
  TyCon c args -> case Map.lookup c (dataTypes table) of
    Nothing -> Left (UnknownType c)
    Just dt
      | typeArity dt /= length args ->
          Left (WrongTypeArgs c (typeArity dt) (length args))
      | otherwise -> TCon c <$> traverse go args
  where
    go = toType table vars

-- ANCHOR: fits
-- 🎚️ Does the default fit the type? A whole number also fits Float,
--    and a constructor with no fields fits its own type (Normal fits
--    Level).
-- 既定値は型に合うか。整数は Float にも合う。フィールドのない構成子は、
-- 自分の型に合う（Normal は Level に合う）
fitsDefault :: DataTable -> Default -> Type -> Bool
fitsDefault table d t = case (d, t) of
  (DefaultOne (LInt _),    TInt)              -> True
  (DefaultOne (LInt _),    TCon "Float" [])   -> True
  (DefaultOne (LFloat _),  TCon "Float" [])   -> True
  (DefaultOne (LString _), TCon "String" [])  -> True
  (DefaultOne (LBool _),   TBool)             -> True
  (DefaultCon c,           TCon h _)          -> constructorOf c h
  (DefaultList ds,         TCon "List" [a])   ->
    all (\x -> fitsDefault table x a) ds
  _                                           -> False
  where
    constructorOf c h = case Map.lookup c (constructors table) of
      Just con -> conTypeName con == h && conArity con == 0
      Nothing  -> False
-- ANCHOR_END: fits
-- ANCHOR_END: toType

-- ANCHOR: infer
-- 🔍 Algorithm W: the type of an expression, what we learned, and the
--    expression again, with holes for the dictionaries of the declared
--    classes (describe x  becomes  describe $hole7 x).
-- W アルゴリズム：式の型と、わかったこと。それに、宣言したクラスの
-- 辞書のための穴を空けた式（describe x は describe $hole7 x になる）
infer :: TypeEnv -> Expr -> Infer (Subst, Type, Expr)
infer env expr = case expr of
  -- ANCHOR: inferAt
  -- The mark stays in the expression to compute: the evaluator needs
  -- it for its errors too.
  -- 印は、計算する式にも残す。評価器もエラーのために使う
  At loc e -> do
    (s, t, e') <- inPlace loc (infer env e)
    -- Write down the type found here (the editor shows it).
    -- ここで見つけた型を書き留める（エディターが見せる）
    Infer (\st -> Right ((), st { marks = (loc, t) : marks st }))
    pure (s, t, At loc e')
  -- ANCHOR_END: inferAt

  -- ANCHOR: inferLit
  -- ANCHOR: inferNumber
  -- 3 can be an Int or a Float: a new a, in the class Number.
  -- 1.5 is always a Float.
  -- 3 は Int にも Float にもなれる：Number に入る新しい a。1.5 は Float
  Lit (LInt _)    -> do
    a <- freshIn Number
    pure (Map.empty, a, expr)
  Lit (LFloat _)  -> pure (Map.empty, tFloat, expr)
  -- ANCHOR_END: inferNumber
  Lit (LBool _)   -> pure (Map.empty, TBool, expr)
  Lit (LString _) -> pure (Map.empty, tString, expr)
  -- ANCHOR_END: inferLit

  -- ANCHOR: inferVar
  -- A name of a declared class's type gets its dictionaries (holes for
  -- now) as its first arguments.
  -- 宣言したクラスの条件のある名前は、辞書（いまは穴）を最初の引数にもらう
  Var x -> case Map.lookup x env of
    Nothing -> failWith (maybe (UnknownName x) (UnknownNameNear x)
                           (nearName env x))
    Just sc -> do
      (t, dicts) <- instantiate sc
      pure (Map.empty, t, foldl App (Var x) dicts)
  -- ANCHOR_END: inferVar

  -- \x -> body : we do not know x's type yet, so give it a new t.
  -- x の型はまだわからないので、新しい t にしておく
  Lam x body -> do
    tx <- fresh
    (s, tb, body') <- scoped (Set.insert x)
      (infer (Map.insert x (Forall [] Map.empty tx) env) body)
    pure (s, TFun (applyT s tx) tb, Lam x body')

  -- An annotation outside a let: the expression must fit it.
  -- let の外の注釈：式が注釈に合わなければならない
  Annot sig e -> do
    table <- knownData
    (sc, _) <- either failWith pure (annotationScheme table sig)
    (t, _) <- instantiate sc
    (s1, e') <- check env e t
    pure (s1, applyT s1 t, Annot sig e')

  -- ANCHOR: inferApp
  -- f a : f must be a function from a's type to something new.
  -- f は、a の型から、何か新しい型への関数のはず
  App f a -> do
    tr <- fresh
    (s1, tf, f') <- infer env f
    (s2, ta, a') <- infer (applyEnv s1 env) a
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
      TRecord {} -> failWith (Mismatch tf' needed)
      -- Otherwise the argument must fit the function: found first.
      -- If not, the mistake is the argument's: told at its place.
      -- そうでなければ、引数が関数に合うはず（来たものを先に）。
      -- 合わなければ、まちがいは引数のもの。引数の場所で知らせる
      _       -> atArgument a (doLine f ta tf' (liftUnify needed tf'))
    -- ANCHOR_END: notAFunction
    pure (s3 `compose` s2 `compose` s1, applyT s3 tr,
          App f' (dropped f (applyT s3 tf') a'))
  -- ANCHOR_END: inferApp

  -- let is recursive: x has a new t while we look at e.
  -- Then x gets a forall type, so that the body can use it in many ways.
  -- let は再帰する。e を見るあいだ、x は新しい t。
  -- そのあと x を forall の型にして、本体でいろいろな型で使えるようにする
  Let x e body -> case annotated e of
    Just (sig, inner) -> inferLetAnnotated env x sig inner body
    Nothing           -> inferLet env x e body

  If c t e -> do
    (s1, tc, c') <- infer env c
    s2 <- liftUnify tc TBool
    let env1 = applyEnv (s2 `compose` s1) env
    (s3, tt, t') <- infer env1 t
    (s4, te, e') <- infer (applyEnv s3 env1) e
    -- else must have the same type as then.
    -- else は、then と同じ型でなければならない
    s5 <- liftUnify te (applyT s4 tt)
    let s = s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1
    pure (s, applyT s5 te, If c' t' e')

  -- ANCHOR: inferOrElse
  -- m ?: d  uses the method toMaybe of the class Fallible:
  --   case toMaybe m of | Just $x -> $x | Nothing -> d
  -- so d is computed only when it is needed. The left is looked at
  -- first, to say clearly when it can never be one (a number, a
  -- string, a list, a function, ...).
  -- m ?: d は、クラス Fallible のメソッド toMaybe を使って開く。
  -- d は、要るときだけ計算する。左を先に見て、決してそうならないもの
  -- （数、文字列、リスト、関数など）なら、はっきりそう言う
  BinOp OrElse l r -> do
    (s1, tl, l') <- infer env l
    fallible tl
    (s2, tm, m') <- infer (applyEnv s1 env) (Var "toMaybe")
    a  <- fresh
    s3 <- liftUnify (applyT s2 tm) (TFun (applyT s2 tl) (TCon "Maybe" [a]))
    let s123 = s3 `compose` s2 `compose` s1
    (s4, tr, r') <- infer (applyEnv s123 env) r
    s5 <- liftUnify tr (applyT (s4 `compose` s123) a)
    let s = s5 `compose` s4 `compose` s123
    pure ( s, applyT s a
         , Case (App m' l') [ (PCon "Just" [PVar "$x"], Var "$x")
                            , (PCon "Nothing" [], r') ] )
  -- ANCHOR_END: inferOrElse

  -- ANCHOR: inferBinOp
  -- Each operator says what its two sides must be, and what it gives.
  -- 演算子ごとに、両側の型と、答えの型が決まっている
  BinOp op l r -> do
    (left, right, result) <- opType op
    (s1, tl, l') <- infer env l
    (s2, tr, r') <- infer (applyEnv s1 env) r
    s3 <- liftUnify (applyT s2 tl) (applyT (s2 `compose` s1) left)
    let s123 = s3 `compose` s2 `compose` s1
    s4 <- liftUnify (applyT s3 tr) (applyT s123 right)
    let s = s4 `compose` s123
    pure (s, applyT s result, BinOp op l' r')
  -- ANCHOR_END: inferBinOp

  -- ANCHOR: inferCollections
  -- (a, b): the type of each part, side by side.
  -- (a, b)：部品の型を、順に並べる
  Tuple es -> do
    (s, ts, es') <- inferAll env es
    pure (s, tTuple ts, Tuple es')

  -- [a, b, c]: every item must have the same type.
  -- [a, b, c]：どの要素も、同じ型でなければならない
  List es -> do
    item <- fresh
    (s1, ts, es') <- inferAll env es
    s2 <- unifyAll (applyT s1 item) ts
    let s = s2 `compose` s1
    pure (s, tList (applyT s item), List es')
  -- ANCHOR_END: inferCollections

  -- ANCHOR: inferCase
  -- case e of | p -> a | q -> b : every pattern has e's type, and
  -- every branch has the same type (the answer's).
  -- どのパターンも e の型、どの枝も同じ型（答えの型）
  Case scrutinee branches -> do
    (s1, te, scrutinee') <- infer env scrutinee
    answer   <- fresh
    let oneBranch (s, te', ta, done) (p, body) = do
          (s2, tp, names) <- inferPattern p
          s3 <- liftUnify tp (applyT s2 te')
          let sp   = s3 `compose` s2
              env' = Map.union
                (Map.fromList [ (x, Forall [] Map.empty (applyT sp t))
                              | (x, t) <- names ])
                (applyEnv (sp `compose` s) env)
          (s4, tb, body') <- scoped (<> Set.fromList (map fst names))
                               (infer env' body)
          s5 <- tryLine scrutinee p (applyT (s4 `compose` sp) ta)
                  (liftUnify tb (applyT (s4 `compose` sp) ta))
          let sAll = s5 `compose` s4 `compose` sp `compose` s
          pure (sAll, applyT sAll te', applyT sAll ta, done <> [(p, body')])
    (s, _, ta, branches') <- foldM oneBranch (s1, te, answer, []) branches
    pure (s, ta, Case scrutinee' branches')
  -- ANCHOR_END: inferCase

  -- ANCHOR: inferRecord
  -- { name = "Lam", age = 3 }: the type of each field, and no rest.
  -- フィールドごとの型。残りはない
  Record fields -> do
    (s, ts, es') <- inferAll env (map snd fields)
    pure ( s, TRecord (Map.fromList (zip (map fst fields) ts)) Nothing
         , Record (zip (map fst fields) es') )

  -- r.name: r must be a record with a field name (and maybe others).
  -- r は、フィールド name を持つレコード（ほかにあってもよい）
  Field e x -> do
    (s1, te, e') <- infer env e
    a    <- fresh
    rest <- freshRow
    s2 <- liftUnify te (TRecord (Map.singleton x a) (Just rest))
    pure (s2 `compose` s1, applyT s2 a, Field e' x)

  -- { r | age = 4 }: r must have the field age, with the same type.
  -- The answer has the type of r: no field is added or taken away.
  -- r は、同じ型のフィールド age を持つ。答えは r と同じ型
  Update e fields -> do
    (s1, te, e') <- infer env e
    (s2, ts, es') <- inferAll (applyEnv s1 env) (map snd fields)
    rest <- freshRow
    -- The new values are what came, the record is what is needed.
    -- 新しい値が「来たもの」、レコードが「要るもの」
    s3 <- liftUnify
            (TRecord (Map.fromList (zip (map fst fields) ts)) (Just rest))
            (applyT s2 te)
    let s = s3 `compose` s2 `compose` s1
    pure (s, applyT s te, Update e' (zip (map fst fields) es'))
  -- ANCHOR_END: inferRecord
-- ANCHOR_END: infer

-- ANCHOR: inferLet
-- 🏷️ let x = e in body, with no annotation. let is recursive: x has a
--    new t while we look at e. Then x gets a forall type, so that the
--    body can use it in many ways.
--    A closed let (it uses no local name: no argument of a lambda
--    around, no name of a pattern) is made general with its declared
--    classes too, as GHC does with MonoLocalBinds: it takes their
--    dictionaries, like a definition at the top. Otherwise its
--    declared classes stay with the definition outside, as before.
-- 注釈のない let x = e in body。let は再帰する。e を見るあいだ、x は
-- 新しい t。そのあと x を forall の型にして、本体でいろいろな型で使える
-- ようにする。閉じた let（ローカルな名前を使わない：外側のラムダの引数も、
-- パターンの名前も使わない）は、GHC の MonoLocalBinds と同じく、宣言した
-- クラスも一般にする。いちばん外の定義と同じく、その辞書を受け取る。
-- そうでなければ、宣言したクラスは、これまでどおり外の定義に残す
inferLet :: TypeEnv -> Name -> Expr -> Expr -> Infer (Subst, Type, Expr)
inferLet env x e body = do
  here' <- localNames
  let closed = Set.null (Set.delete x (freeVars e) `Set.intersection` here')
  before <- holeCount
  tx <- fresh
  -- Inside e, x is local: a let inside that uses x is not closed.
  -- e の中では x はローカル。x を使う中の let は、閉じていない
  (s1, te, e') <- scoped (Set.insert x)
    (infer (Map.insert x (Forall [] Map.empty tx) env) e)
  s2 <- liftUnify (applyT s1 tx) te
  -- ANCHOR: settleLet
  -- The f that are still not known become lists (see settle).
  -- まだわからない f は、リストにする（settle を見る）
  s2' <- settle
  let s    = s2' `compose` s2 `compose` s1
  -- ANCHOR_END: settleLet
      env' = applyEnv s env
  sc <- generalizeIn (not closed) env' (applyT s te)
  e'' <- if closed then localDictionaries s sc before x e' else pure e'
  let inBody = if closed then Set.delete x else Set.insert x
  (s3, tb, body') <- scoped inBody (infer (Map.insert x sc env') body)
  pure (s3 `compose` s, tb, Let x e'' body')
-- ANCHOR_END: inferLet

-- ANCHOR: localDictionaries
-- 🎁 A closed let that needs dictionaries takes them in front, like a
--    definition at the top. It calls itself through a let inside, so
--    that it passes on the same dictionaries:
--      let wrap = \$Display_l7 -> let wrap = ... in wrap in ...
--    The holes made inside it, for its own type variables, are filled
--    now; the others wait for the definition outside.
-- 辞書の要る閉じた let は、いちばん外の定義と同じく、辞書を前で受け取る。
-- 中の let を通して自分を呼ぶので、同じ辞書を渡し続ける。中で空けた穴の
-- うち、自分の型変数のものは、いま埋める。ほかは外の定義を待つ
localDictionaries :: Subst -> Scheme -> Int -> Name -> Expr -> Infer Expr
localDictionaries s (Forall vs ns _) before x e
  | null ps   = pure e
  | otherwise = do
      filled <- fillLocal s (Set.fromList vs) given before (Let x e (Var x))
      pure (foldr (\(v, c) -> Lam (param v c)) filled ps)
  where
    ps    = dictNeeds ns
    given = [ (c, v, Var (param v c)) | (v, c) <- ps ]
    -- $Display_l7: a name of its own, so that it never hides the
    -- dictionary of the definition outside ($Display_a).
    -- 自分だけの名前。外の定義の辞書（$Display_a）を隠さない
    param v c = localParam c v
-- ANCHOR_END: localDictionaries

-- 🏷️ The dictionary parameter of a let inside:  $Display_l7
-- 中の let の辞書の引数の名前
localParam :: Class -> Int -> Name
localParam c v = dictParam c ("l" <> T.pack (show v))

-- ANCHOR: inferLetAnnotated
-- ✍️ let x : sig, then x = e, then body. The annotation is the type x
--    gets, even when e uses local names: it is checked as at the top
--    (see checkAnnotated), and it takes the dictionaries of its
--    classes.
-- 注釈のある let。注釈が x の型になる。e がローカルな名前を使っていても
-- よい。いちばん外と同じく確かめ（checkAnnotated を見る）、クラスの辞書を
-- 受け取る
inferLetAnnotated
  :: TypeEnv -> Name -> Signature -> Expr -> Expr
  -> Infer (Subst, Type, Expr)
inferLetAnnotated env x sig e body = do
  table <- knownData
  (declared, names) <- either failWith pure (annotationScheme table sig)
  let Forall vs wanted t0 = declared
      nameOf v = Map.findWithDefault "?" v names
      classesOf v = maybe [] Set.toList (Map.lookup v wanted)
  before <- holeCount
  news <- mapM (const freshRow) vs
  let t = applyT (Map.fromList (zip vs (map TVar news))) t0
  sequence_ [ require c (TVar new) | (v, new) <- zip vs news
                                   , c <- classesOf v ]
  (s0, e') <- check (Map.insert x declared env) e t
  s1 <- settle
  let s      = s1 `compose` s0
      images = [ (v, applyT s (TVar new)) | (v, new) <- zip vs news ]
      outer  = Set.unions [ ftv ty `Set.difference` Set.fromList bound
                          | Forall bound _ ty <- Map.elems (applyEnv s env) ]
  ns <- currentNeeds
  -- Each variable of the annotation stays a variable of its own, not
  -- one of the types outside, with only the classes written.
  -- 注釈の型変数は、それぞれ別の型変数のまま。外の型にもならず、
  -- 書いたクラスだけを持つ
  forM_ images $ \(v, image) -> case image of
    TVar m
      | m `Set.member` outer -> failWith (TooGeneral x (nameOf v) image)
      | (w, _) : _ <- [ (w, ()) | (w, TVar m') <- images, m' == m, w > v ]
          -> failWith (SameVariables x (nameOf v) (nameOf w))
      | c : _ <- Set.toList (Map.findWithDefault Set.empty m ns
                   `Set.difference`
                   Set.unions (map (superClosure table) (classesOf v)))
          -> failWith (AnnotationNeeds x c (nameOf v))
      | otherwise -> pure ()
    _ -> failWith (TooGeneral x (nameOf v) image)
  let ps    = [ (v, m, c) | (v, c) <- dictNeeds wanted
                          , Just (TVar m) <- [lookup v images] ]
      given = [ (c, m, Var (localParam c m)) | (_, m, c) <- ps ]
      mine  = Set.fromList [ m | (_, TVar m) <- images ]
  filled <- fillLocal s mine given before e'
  let e'' = foldr (\(_, m, c) -> Lam (localParam c m)) filled ps
  (s2, tb, body') <- scoped (Set.delete x)
    (infer (Map.insert x declared (applyEnv s env)) body)
  pure (s2 `compose` s, tb, Let x e'' body')
-- ANCHOR_END: inferLetAnnotated

-- ✍️ The annotation of a let, if it has one:  Let x (Annot sig e) ...
-- let の注釈（あれば）
annotated :: Expr -> Maybe (Signature, Expr)
annotated e = case e of
  Annot sig inner -> Just (sig, inner)
  At _ inner      -> annotated inner
  _               -> Nothing

-- ANCHOR: fillLocal
-- 🕳️ How many holes there are now: the holes made after this are at
--    the front of the list.
-- いまの穴の数。このあとで空けた穴は、リストの前にある
holeCount :: Infer Int
holeCount = Infer (\st -> Right (length (holes st), st))

-- 🕳️ Fill the holes made since there were  before  holes, whose types
--    use the variables mine: they are for the dictionaries given. The
--    other holes stay, for the definition outside.
-- before 個だったときから空けた穴のうち、型が mine の型変数を使うものを、
-- 渡された辞書で埋める。ほかの穴は、外の定義のために残す
fillLocal :: Subst -> Set Int -> Given -> Int -> Expr -> Infer Expr
fillLocal s mine given before e = do
  hs    <- Infer (\st -> Right (holes st, st))
  table <- knownData
  let made         = length hs - before
      uses (_, _, t) =
        not (Set.null (ftv (applyT s t) `Set.intersection` mine))
      (now, later) = partition uses (take made hs)
  filled <- forM now $ \(h, c, t) ->
    either failWith (\d -> pure (h, d)) (dictFor table given c (applyT s t))
  Infer (\st -> Right ((), st { holes = later <> drop made hs }))
  pure (fillVars (Map.fromList filled) e)
-- ANCHOR_END: fillLocal

-- ANCHOR: doLine
-- 📝 p <- e in a do is  $do.bind e (\p -> rest). When the value of the
--    line does not fit the way the lines below use it, say so, at the
--    line: a ? left out is the usual reason.
-- do の p <- e は $do.bind e (\p -> rest)。行の値が、下の行の使い方に
-- 合わないなら、行の場所でそう言う。よくあるわけは、? の付け忘れ
doLine :: Expr -> Type -> Type -> Infer Subst -> Infer Subst
doLine f ta tf' unifying = case (f, ta, tf') of
  (App (Var b) (At loc _), TFun used _, TFun (TFun given _) _)
    | b == doBind -> do
        tried <- attempt unifying
        case tried of
          Right s  -> pure s
          Left err -> do
            alone <- fitsAlone (liftUnify given used)
            if alone then failWith err
              else failWith (TypeAt loc (LineGives given used))
  _ -> unifying
-- ANCHOR_END: doLine

-- ANCHOR: dropped
-- 🗑️ A line of a do with no ? and no <- whose value is a Result
--    (File.copy a b gives IO (Result String ())): its Err would be lost
--    without a word. Mark the rest of the do, so a warning tells it
--    (see droppedWarnings). Like Rust's #[must_use], only Result:
--    a Maybe thrown away is fine.
-- ? も <- もない do の行で、値が Result のもの（File.copy a b は
-- IO (Result String ())）：Err が黙って消えてしまう。do の残りに印を
-- 付けて、警告で知らせる（droppedWarnings を見る）。Rust の #[must_use]
-- と同じく Result だけ。Maybe を捨てるのはよい
dropped :: Expr -> Type -> Expr -> Expr
dropped f tf rest = case (f, tf, rest) of
  (App (Var b) (At loc _), TFun (TFun (TCon "Result" _) _) _, Lam "$_" body)
    | b == doBind -> At loc (Lam droppedLine body)
  _ -> rest
-- ANCHOR_END: dropped

-- ANCHOR: nearName
-- 🧭 For an unknown name, the same name in another place, if there is
--    one: String.length → length, startsWith → String.startsWith.
-- 知らない名前に、別の場所の同じ名前（あれば）
nearName :: TypeEnv -> Name -> Maybe Name
nearName env x =
  case filter (`Map.member` env) (filter (/= x) candidates) of
    found : _ -> Just found
    []        -> Nothing
  where
    -- String.length: length. startsWith: String.startsWith (or List.).
    -- Not another module (String.size is not Map.size).
    -- 別のモジュールにはしない（String.size は Map.size ではない）
    candidates
      | T.any (== '.') x = [T.takeWhileEnd (/= '.') x]
      | otherwise        = [ m <> "." <> x | m <- ["String", "List"] ]
-- ANCHOR_END: nearName

-- ANCHOR: tryLine
-- ❓ p <- e ? opens to a case with $try.branch, whose Err branch gives
--    the failure as the answer of the whole do. When that does not fit
--    what the lines below give, say so, at the line of the ?.
-- p <- e ? は $try.branch の case に開く。その Err の枝は、失敗を do 全体の
-- 答えにする。それが下の行の答えに合わないなら、? の行の場所でそう言う
tryLine :: Expr -> Pattern -> Type -> Infer Subst -> Infer Subst
tryLine scrutinee p rest unifying = case (scrutinee, p) of
  (App (Var b) (At loc _), PCon "Err" _)
    | b == tryBranch -> do
        tried <- attempt unifying
        either (const (failWith (TypeAt loc (TryNeeds rest)))) pure tried
  _ -> unifying
-- ANCHOR_END: tryLine

-- ANCHOR: inferGroup
-- 👯 Definitions at the top that use each other (isEven and isOdd):
--    their types are found together. Each has a new t while we look at
--    them all; then each gets a forall type. Dictionaries cannot be
--    passed around in a ring here: then their annotations are needed
--    (like Haskell, where an annotated one is checked alone).
--    The error says which definition went wrong.
-- おたがいを使う、いちばん外の定義（isEven と isOdd）：型を一緒に
-- 見つける。全部を見るあいだ、それぞれ新しい t。そのあとそれぞれを
-- forall の型にする。ここでは辞書を輪にして渡せないので、そのときは注釈が
-- 要る（Haskell でも、注釈のあるものは1つずつ確かめる）。エラーには、
-- どの定義でまちがえたかを書く
inferGroup
  :: DataTable -> TypeEnv -> [(Name, Expr)]
  -> Either (Name, TypeError) [(Name, Scheme, Expr)]
inferGroup table env defs = runInferAt table $ do
  ts <- mapM (const fresh) defs
  let inside = Map.fromList [ (x, Forall [] Map.empty t)
                            | ((x, _), t) <- zip defs ts ] `Map.union` env
      one (s, done) ((x, e), t) = blame x $ do
        (s1, te, e') <- infer (applyEnv s inside) e
        s2 <- liftUnify (applyT (s1 `compose` s) t) te
        pure (s2 `compose` s1 `compose` s, done <> [(x, e')])
  (s0, typed) <- foldM one (Map.empty, []) (zip defs ts)
  s1 <- settle
  let s = s1 `compose` s0
  schemes <- forM (zip typed ts) $ \((x, _), t) ->
    blame x (generalizeIn False (applyEnv s env) (applyT s t))
  case [ x | ((x, _), Forall _ ns _) <- zip typed schemes
           , not (null (dictNeeds ns)) ] of
    x : _ -> blame x (failWith (GroupNeedsAnnotation (map fst defs)))
    []    -> pure ()
  forM (zip typed schemes) $ \((x, e'), sc) ->
    blame x ((,,) x sc <$> withDictionaries s sc e')
  where
    -- Which definition an error happened in.
    -- エラーが、どの定義で起きたか
    blame x (Infer f) = Infer $ \st -> case f st of
      Left err   -> Left (GroupIn x err)
      Right done -> Right done
    runInferAt t m = case (runInfer t m, defs) of
      (Left (GroupIn x err), _)  -> Left (x, err)
      (Left err, (x, _) : _)     -> Left (x, err)
      (Left err, [])             -> Left ("", err)
      (Right done, _)            -> Right done
-- ANCHOR_END: inferGroup

-- ANCHOR: inferPattern
-- 🧩 The type of a pattern, and the names it gives with their types.
--    Just x  has the type Maybe t5, and gives x : t5.
-- パターンの型と、パターンが付ける名前とその型
inferPattern :: Pattern -> Infer (Subst, Type, [(Name, Type)])
inferPattern pat = case pat of
  PVar x -> do
    t <- fresh
    pure (Map.empty, t, [(x, t)])
  PWild -> do
    t <- fresh
    pure (Map.empty, t, [])
  PLit (LInt _)    -> pure (Map.empty, TInt, [])
  PLit (LFloat _)  -> pure (Map.empty, tFloat, [])
  PLit (LBool _)   -> pure (Map.empty, TBool, [])
  PLit (LString _) -> pure (Map.empty, tString, [])

  -- Just x : take a fresh copy of  a -> Maybe a, and give each
  -- argument pattern the type of that argument.
  -- 構成子の型を新しくして、引数のパターンに、その引数の型を付ける
  PCon c ps -> do
    table <- knownData
    con <- maybe (failWith (UnknownConstructor c)) pure
                 (Map.lookup c (constructors table))
    when (length ps /= conArity con) $
      failWith (WrongPatternArgs c (conArity con) (length ps))
    (t, _) <- instantiate (conScheme con)
    let (args, result) = splitArgs (conArity con) t
    (s, names) <- patternsOfTypes ps args
    pure (s, applyT s result, names)

  PTuple ps -> do
    ts <- mapM (const fresh) ps
    (s, names) <- patternsOfTypes ps ts
    pure (s, tTuple (map (applyT s) ts), names)

  -- [p, q]: every item has the same type.  [] : [t]
  -- どの要素も同じ型
  PList ps -> do
    item <- fresh
    (s, names) <- patternsOfTypes ps (map (const item) ps)
    pure (s, tList (applyT s item), names)

  -- x : rest : rest is a list of x's type.
  -- rest は、x の型のリスト
  PCons h rest -> do
    item <- fresh
    (s, names) <- patternsOfTypes [h, rest] [item, tList item]
    pure (s, tList (applyT s item), names)

  -- ANCHOR: patternRecord
  -- { name, age }: a record with these fields (and maybe others).
  -- これらのフィールドを持つレコード（ほかにあってもよい）
  PRecord xs -> do
    ts   <- mapM (const fresh) xs
    rest <- freshRow
    pure (Map.empty, TRecord (Map.fromList (zip xs ts)) (Just rest), zip xs ts)
  -- ANCHOR_END: patternRecord

-- 🧩 Give each pattern its type, from left to right.
-- パターンに、それぞれの型を、左から順に付ける
patternsOfTypes :: [Pattern] -> [Type] -> Infer (Subst, [(Name, Type)])
patternsOfTypes ps ts = case (ps, ts) of
  (p : ps', t : ts') -> do
    (s1, tp, names1) <- inferPattern p
    s2 <- liftUnify tp (applyT s1 t)
    let s = s2 `compose` s1
    (s3, names2) <- patternsOfTypes ps' (map (applyT s) ts')
    let sAll = s3 `compose` s
    pure (sAll, [ (x, applyT sAll ty) | (x, ty) <- names1 <> names2 ])
  _ -> pure (Map.empty, [])

-- ✂️ a -> b -> T  with 2 arguments is  ([a, b], T).
-- 引数の型と、答えの型に分ける
splitArgs :: Int -> Type -> ([Type], Type)
splitArgs n t = case (n, t) of
  (0, _)          -> ([], t)
  (_, TFun a b)   -> let (as, r) = splitArgs (n - 1) b in (a : as, r)
  _               -> ([], t)
-- ANCHOR_END: inferPattern

-- ANCHOR: opType
-- ➕ The types of the two sides, and of the answer. Both sides of
--    == and < are the same Comparable type; ++ joins two of the same
--    Semigroup type.
-- 両側の型と、答えの型。== や < の両側は、同じ Comparable の型。
-- ++ は、同じ Semigroup の型を2つつなぐ
opType :: Op -> Infer (Type, Type, Type)
opType op = case op of
  -- ANCHOR: opTypeNumber
  -- + - * work on any Number, both sides the same: 1 + 2.5 is a Float.
  -- / is for Floats, // for Ints. /? and //? give Nothing for 0.
  -- + - * は、どの Number にも。両側は同じ型。/ は Float、// は Int
  _ | op `elem` [Add, Sub, Mul] -> do
        a <- freshIn Number
        pure (a, a, a)
  Div         -> pure (tFloat, tFloat, tFloat)
  IntDiv      -> pure (TInt, TInt, TInt)
  DivMaybe    -> pure (tFloat, tFloat, TCon "Maybe" [tFloat])
  IntDivMaybe -> pure (TInt, TInt, TCon "Maybe" [TInt])
  -- ANCHOR_END: opTypeNumber
  _ | op `elem` [Eq, Ne, Lt, Le, Gt, Ge] -> do
        a <- freshIn Comparable
        pure (a, a, TBool)
  Concat -> do
    a <- freshIn Semigroup
    pure (a, a, a)
  Cons   -> do
    a <- fresh
    pure (a, tList a, tList a)
  _      -> pure (TBool, TBool, TBool)          -- && ||

-- ANCHOR: fallible
-- 🛟 Can the left of ?: be in the class Fallible? A type variable
--    may (unless it is a number); a named type with a hole left may,
--    if its hole is in Fallible (Maybe Int, Result String Int).
--    A number, a string, a list, a function, ... never is.
-- ?: の左が Fallible に入りうるか。型変数なら入りうる（数でなければ）。
-- 名前の型なら、最後の型を外した穴が Fallible に入るなら（Maybe Int、
-- Result String Int）。数、文字列、リスト、関数などは、決して入らない
fallible :: Type -> Infer ()
fallible t = do
  table <- knownData
  case t of
    TVar n -> do
      cs <- Map.findWithDefault Set.empty n <$> currentNeeds
      when (Number `Set.member` cs) $ failWith (OrElseNeeds t)
    TApp _ _ -> pure ()
    TCon c ts@(_ : _)
      | inClass table (Declared "Fallible") (TCon c (init ts)) /= Nothing
        -> pure ()
    _ -> failWith (OrElseNeeds t)
-- ANCHOR_END: fallible

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
inferAll :: TypeEnv -> [Expr] -> Infer (Subst, [Type], [Expr])
inferAll env es = case es of
  [] -> pure (Map.empty, [], [])
  e : rest -> do
    (s1, t, e')    <- infer env e
    (s2, ts, es')  <- inferAll (applyEnv s1 env) rest
    pure (s2 `compose` s1, applyT s2 t : ts, e' : es')

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
-- 🔍 The type of a whole expression, and the expression with its
--    dictionaries filled in.
-- 式全体の型と、辞書を埋めた式
-- The answer is a Scheme, to show the classes: Comparable a => a -> Bool
-- クラスの条件を見せるため、答えは Scheme
inferType
  :: DataTable -> TypeEnv -> Expr -> Either TypeError (Scheme, Expr)
inferType table env e = runInfer table $ do
  (s0, t, e') <- infer env e
  finish env s0 t e'

-- ANCHOR: typesAt
-- 🔍 The type at each mark of a place in an expression: a call, an
--    argument, an operator, a line of a do. The marks were written
--    down on the way, with the types known then; the substitution at
--    the end makes them whole (t3 found early may turn out to be Int).
--    The type the definition got (its annotation, say) is fitted
--    first, if given. A mistake gives nothing: the editor shows the
--    mistake instead.
-- 式の中の、場所の印ごとの型：呼び出し・引数・演算子・do の行。印は
-- 途中で、そのときわかっていた型と一緒に書き留めた。最後の代入で仕上げる
-- （はじめに見つけた t3 が、あとで Int とわかることがある）。定義が
-- もらった型（注釈など）があれば、先に合わせる。まちがいなら何も返さない
-- （エディターは、かわりにまちがいを見せる）
typesAt :: DataTable -> TypeEnv -> Maybe Scheme -> Expr -> [(Loc, Type)]
typesAt table env given e = either (const []) id $ runInfer table $ do
  (s0, t, _) <- infer env e
  s1 <- case given of
    Nothing -> pure Map.empty
    Just sc -> do
      (whole, _) <- instantiate sc
      fitted <- attempt (liftUnify whole (applyT s0 t))
      pure (either (const Map.empty) id fitted)
  s2 <- settle
  let s = s2 `compose` s1 `compose` s0
  found <- Infer (\st -> Right (marks st, st))
  pure [ (loc, applyT s ty) | (loc, ty) <- reverse found ]
-- ANCHOR_END: typesAt

-- ANCHOR: inferIO
-- 🚪 The same, but the expression must be an IO: IO answer, or IO of
--    anything (Nothing). The REPL does  pure 1  as an IO (GHCi does
--    too), and main must be IO ().
-- 同じだが、式は IO でなければならない（IO answer か、Nothing なら何かの
-- IO）。REPL は pure 1 を IO としてやり（GHCi と同じ）、main は IO ()
inferIO
  :: DataTable -> TypeEnv -> Maybe Type -> Expr
  -> Either TypeError (Scheme, Expr)
inferIO table env answer e = runInfer table $ do
  (s0, t, e') <- infer env e
  a  <- maybe fresh pure answer
  s1 <- liftUnify (applyT s0 t) (tIO a)
  finish env (s1 `compose` s0) t e'
-- ANCHOR_END: inferIO

-- 🏁 Settle the classes, then make the type general (or a number an
--    Int), and fill the dictionaries in.
-- クラスを落ち着かせ、型を一般にし（数は Int にし）、辞書を埋める
finish :: TypeEnv -> Subst -> Type -> Expr -> Infer (Scheme, Expr)
finish env s0 t e' = do
  s1 <- settle
  let s = s1 `compose` s0
  Forall vs ns ty <- generalizeIn False (applyEnv s env) (applyT s t)
  -- A number with a declared class is an Int (or a Float) here, as
  -- the REPL shows it:  total [1, 2, 3]  is 6, not a function.
  -- 宣言したクラスの付いた数は、ここで Int（か Float）にする
  known' <- knownData
  picks <- fmap concat $ forM vs $ \v -> do
    let cs = Map.findWithDefault Set.empty v ns
        fitsAll k = all (\c -> inClass known' c k /= Nothing) (Set.toList cs)
    pure [ (v, k) | Number `Set.member` cs, not (null (dictNeeds
             (Map.singleton v cs)))
         , k : _ <- [filter fitsAll [TInt, tFloat]] ]
  let chosen = Map.fromList picks
      sc     = Forall (filter (`Map.notMember` chosen) vs)
                      (foldr Map.delete ns (Map.keys chosen))
                      (applyT chosen ty)
  e'' <- withDictionaries (chosen `compose` s) sc e'
  pure (sc, e'')

-- ANCHOR: inferDefinition
-- 🔍 The type of a REPL definition  let x = e  (recursive, like let).
--    With dictionaries, it calls itself through a let inside, so that
--    it passes on the same dictionaries:
--      describeAll = \$Describe_a -> let describeAll = ... in describeAll
-- REPL の定義 let x = e の型（let と同じく再帰する）。辞書があるなら、
-- 中の let を通して自分を呼ぶ。そうすれば、同じ辞書を渡し続ける
inferDefinition
  :: DataTable -> TypeEnv -> Name -> Expr -> Either TypeError (Scheme, Expr)
inferDefinition table env x e = runInfer table $ do
  tx <- fresh
  (s1, te, e') <- infer (Map.insert x (Forall [] Map.empty tx) env) e
  s2 <- liftUnify (applyT s1 tx) te
  s3 <- settle
  let s = s3 `compose` s2 `compose` s1
  sc@(Forall _ ns _) <- generalizeIn False (applyEnv s env) (applyT s te)
  let selfCall
        | null (dictNeeds ns) = id
        | otherwise           = \body -> Let x body (Var x)
  e'' <- withDictionaries s sc (selfCall e')
  pure (sc, e'')
-- ANCHOR_END: inferDefinition

-- ANCHOR: withDictionaries
-- 🎁 Fill the holes, and take the dictionaries the type needs as
--    parameters in front:
--      describeTwice x = describe x ++ describe x
--    becomes  \$Describe_a -> \x -> describe $Describe_a x ++ ...
-- 穴を埋め、型に要る辞書を、前の引数として受け取る
withDictionaries :: Subst -> Scheme -> Expr -> Infer Expr
withDictionaries s (Forall _ ns t) e = do
  let ps    = dictNeeds ns
      names = typeNames t
      param v c = dictParam c (Map.findWithDefault "?" v names)
  filled <- fillHoles s [ (c, v, Var (param v c)) | (v, c) <- ps ] e
  pure (foldr (\(v, c) -> Lam (param v c)) filled ps)
-- ANCHOR_END: withDictionaries
-- ANCHOR_END: run

-- ANCHOR: inferAnnotated
-- ✍️ The type of a definition with a type annotation above it.
--    The annotation is the type it gets, but first it is checked:
--      1. the definition must fit it (inside, the name already has it),
--      2. each type variable of the annotation must stay a variable of
--         its own (a may be any type: not Int, and not the same as b),
--      3. the classes it needs must be written (Comparable a =>), or
--         come with a class written (Ranked a brings Describe a).
--    names are the type variables as written (a, r), for the errors.
--    The dictionaries are the parameters, in the order of dictNeeds.
-- 型の注釈が上にある定義の型。注釈がそのまま型になるが、先に確かめる：
--   1. 定義が注釈に合う（中では、名前がもう注釈の型を持つ）
--   2. 注釈の型変数は、それぞれ別の型変数のまま
--      （a はどんな型でもよい：Int にも、b と同じにもならない）
--   3. 要るクラスが書いてある（Comparable a =>）か、書いたクラスが
--      連れてくる（Ranked a は Describe a を連れてくる）
-- names は、書いたとおりの型変数の名前（a、r）。エラーのため。
-- 辞書は引数になる（順番は dictNeeds の順）
inferAnnotated
  :: DataTable -> TypeEnv -> Name -> Map Int Name -> Scheme -> Expr
  -> Either TypeError (Scheme, Expr)
inferAnnotated table env x names declared =
  checkAnnotated table (Map.insert x declared env) x names declared

-- ✍️ The same, but the name is not put into the environment: a method
--    of an instance is checked with the type of the instance, but the
--    method it calls inside is the method of the class.
-- 同じだが、名前を環境に入れない。インスタンスのメソッドはインスタンスの
-- 型で確かめるが、中で呼ぶメソッドは、クラスのメソッド
checkAnnotated
  :: DataTable -> TypeEnv -> Name -> Map Int Name -> Scheme -> Expr
  -> Either TypeError (Scheme, Expr)
checkAnnotated table env x names declared e = runInfer table $ do
  let Forall vs wanted t0 = declared
  -- The variables of the annotation, as new type variables.
  -- 注釈の型変数を、新しい型変数にする
  news <- mapM (const freshRow) vs
  let t = applyT (Map.fromList (zip vs (map TVar news))) t0
  sequence_ [ require c (TVar new)
            | (v, new) <- zip vs news, c <- classesOf wanted v ]
  (s0, e') <- check env e t
  s1 <- settle
  let s = s1 `compose` s0
  let images = [ (v, applyT s (TVar new)) | (v, new) <- zip vs news ]
  ns <- currentNeeds
  mapM_ (checkImage images ns) images
  -- The given dictionaries are for the images of the variables.
  -- 渡される辞書は、型変数の行き先のためのもの
  let ps = dictNeeds wanted
      given = [ (c, m, Var (dictParam c (nameOf v)))
              | (v, c) <- ps, Just (TVar m) <- [lookup v images] ]
  filled <- fillHoles s given e'
  pure (declared, foldr (\(v, c) -> Lam (dictParam c (nameOf v))) filled ps)
  where
    classesOf wanted v = maybe [] Set.toList (Map.lookup v wanted)
    nameOf v = Map.findWithDefault "?" v names
    checkImage images ns (v, image) = case image of
      TVar m -> do
        -- Two variables of the annotation became the same one.
        -- 注釈の2つの型変数が、同じ1つになった
        case [ w | (w, TVar m') <- images, m' == m, w /= v ] of
          w : _ | v < w -> failWith (SameVariables x (nameOf v) (nameOf w))
          _ -> pure ()
        -- A class the definition needs, but the annotation does not
        -- say or bring.
        -- 定義には要るのに、注釈が書いても連れてきてもいないクラス
        let needed = Map.findWithDefault Set.empty m ns
            Forall _ wanted _ = declared
            allowed = Set.unions
              (map (superClosure table) (classesOf wanted v))
        case Set.toList (needed `Set.difference` allowed) of
          c : _ -> failWith (AnnotationNeeds x c (nameOf v))
          []    -> pure ()
      _ -> failWith (TooGeneral x (nameOf v) image)
-- ANCHOR_END: inferAnnotated

-- ANCHOR: given
-- 🔑 The dictionaries we have: for class c of type variable t_v, the
--    expression that gives it (a parameter, like $Describe_a).
-- 手元にある辞書：型変数 t_v のクラス c の辞書を出す式（$Describe_a など）
type Given = [(Class, Int, Expr)]

-- 🏷️ The dictionary of an instance:  $Describe$Maybe
-- インスタンスの辞書の名前
dictName :: Name -> Name -> Name
dictName c h = "$" <> c <> "$" <> h

-- 🏷️ A dictionary parameter:  $Describe_a  (for the type variable a)
-- 辞書の引数の名前
dictParam :: Class -> Name -> Name
dictParam c a = "$" <> className c <> "_" <> a
-- ANCHOR_END: given

-- ANCHOR: dictFor
-- 📗 The dictionary of class c for type t:
--      Describe Shape            $Describe$Shape
--      Describe (Maybe Shape)    $Describe$Maybe $Describe$Shape
--      Describe a                $Describe_a, the one given
--      Describe a, given Ranked a only     $Ranked_a.$Describe
-- 型 t のための、クラス c の辞書
dictFor :: DataTable -> Given -> Class -> Type -> Either TypeError Expr
dictFor table given c t = case t of
  TVar v ->
    case [ e | (k, v', d) <- given, v' == v, Just e <- [upTo k d] ] of
      e : _ -> Right e
      []    -> Left (AmbiguousClass c t)
  _ -> case (c, typeHead t) of
    (Declared n, Just (h, args))
      | Just inst <- Map.lookup (n, h) (instances table)
      , length args == instArity inst -> do
          dicts <- sequence
            [ dictFor table given k (args !! i)
            | (k@(Declared _), i) <- instNeeds inst ]
          Right (foldl App (Var (dictName n h)) dicts)
    _ -> Left (NotInClass c t)
  where
    -- From the dictionary d of class k, the way up to c, if there is
    -- one: a dictionary holds the dictionaries of the classes above.
    -- クラス k の辞書 d から、c までの道（あれば）。辞書は、上位の
    -- クラスの辞書を持っている
    upTo k d
      | k == c    = Just d
      | otherwise = case k of
          Declared n | Just info <- Map.lookup n (classes table) ->
            case [ e | above@(Declared a) <- classSuper info
                     , Just e <- [upTo above (Field d ("$" <> a))] ] of
              e : _ -> Just e
              []    -> Nothing
          _ -> Nothing
-- ANCHOR_END: dictFor

-- ANCHOR: fillHoles
-- 🕳️ Fill every hole with its dictionary, now that the types are
--    known. A number whose type is still open is an Int (or a Float,
--    if only a Float fits), as Haskell's default does; any other open
--    type cannot choose an instance.
-- 型がわかったので、穴を全部、辞書で埋める。まだ決まらない数は Int
-- にする（Float しか合わなければ Float）。Haskell の default と同じ。
-- ほかの決まらない型では、インスタンスを選べない
fillHoles :: Subst -> Given -> Expr -> Infer Expr
fillHoles s given e = do
  hs    <- Infer (\st -> Right (holes st, st))
  table <- knownData
  ns    <- currentNeeds
  let mine  = [ v | (_, v, _) <- given ]
      open  = nub [ (v, c) | (_, c, t) <- hs
                           , v <- Set.toList (ftv (applyT s t))
                           , v `notElem` mine ]
  picks <- forM (nubOn fst open) $ \(v, c) -> do
    let cs   = Map.findWithDefault Set.empty v ns
        fitsAll ty = all (\k -> inClass table k ty /= Nothing) (Set.toList cs)
    if Number `Set.member` cs
      then case filter fitsAll [TInt, tFloat] of
             ty : _ -> pure [(v, ty)]
             []     -> failWith (NotInClass c TInt)
      -- ANCHOR: unusedOpen
      -- A type no dictionary needs, like the e of Result e for
      -- Functor: the instance does not look at it.
      -- どの辞書も要らない型。Functor の Result e の e など：
      -- インスタンスはそれを見ない
      else if not (any isDeclared (Set.toList cs)) then pure []
      -- ANCHOR_END: unusedOpen
      else failWith (AmbiguousClass c (TVar v))
  let s' = Map.fromList (concat picks) `compose` s
  filled <- forM hs $ \(h, c, t) ->
    either failWith (\d -> pure (h, d)) (dictFor table given c (applyT s' t))
  pure (fillVars (Map.fromList filled) e)
  where
    isDeclared k = case k of
      Declared _ -> True
      _          -> False
    nubOn f = foldr (\x acc -> if any ((== f x) . f) acc then acc
                                 else x : acc) []

-- ✏️ Put each hole's dictionary in its place.
-- それぞれの穴に、辞書を入れる
fillVars :: Map Name Expr -> Expr -> Expr
fillVars m = go
  where
    go e = case e of
      Var x | Just d <- Map.lookup x m -> d
      Var _        -> e
      Lit _        -> e
      Lam x b      -> Lam x (go b)
      App f a      -> App (go f) (go a)
      Let x a b    -> Let x (go a) (go b)
      If c a b     -> If (go c) (go a) (go b)
      BinOp o l r  -> BinOp o (go l) (go r)
      Tuple es     -> Tuple (map go es)
      List es      -> List (map go es)
      Case x bs    -> Case (go x) [ (p, go b) | (p, b) <- bs ]
      Record fs    -> Record [ (f, go x) | (f, x) <- fs ]
      Field x f    -> Field (go x) f
      Update x fs  -> Update (go x) [ (f, go y) | (f, y) <- fs ]
      At loc x     -> At loc (go x)
      Annot t x    -> Annot t (go x)
-- ANCHOR_END: fillHoles

-- ANCHOR: checkInstanceSupers
-- 🧩 An instance must fit the classes above its class:
--    instance Ranked Shape  needs  Describe Shape  (and Comparable
--    Shape, if Ranked says so). The type variables of the instance are
--    t0, t1, ... (written as a, b, ...), with the classes of its
--    context. The answer is the
--    dictionaries of the declared classes above, by name.
-- インスタンスは、そのクラスの上位のクラスに合わなければならない。
-- インスタンスの型変数は t0、t1、...（書いた名前は a、b、...）で、
-- 文脈のクラスを持つ。答えは、
-- 上位の宣言したクラスの辞書（名前つき）
checkInstanceSupers
  :: DataTable -> [Class] -> Type -> [Name] -> [(Class, Int)]
  -> Either TypeError [(Name, Expr)]
checkInstanceSupers table supers headType written context =
  runInfer table $ do
    let arity = length written
    vars <- mapM (const fresh) [1 .. arity]          -- t0, t1, ...
    sequence_ [ require c (vars !! i) | (c, i) <- context ]
    mapM_ (\c -> require c headType) supers
    ns <- currentNeeds
    sequence_
      [ case Set.toList (needed `Set.difference` allowed) of
          c : _ -> failWith (NotInClass c (TVar i))
          []    -> pure ()
      | i <- [0 .. arity - 1]
      , let needed  = Map.findWithDefault Set.empty i ns
            allowed = Set.unions [ superClosure table c
                                 | (c, j) <- context, j == i ] ]
    let given = [ (k, i, Var (dictParam k (written !! i)))
                | (k@(Declared _), i) <- context ]
    forM [ n | Declared n <- supers ] $ \n ->
      either failWith (\d -> pure (n, d))
        (dictFor table given (Declared n) headType)
-- ANCHOR_END: checkInstanceSupers

-- ANCHOR: check
-- ☑️ Check an expression against a type we know. A function gives
--    the known types to its arguments first, so that a mistake is
--    found where it is made:  add x y = x ++ y  with  Int -> Int -> Int
--    stops at the ++ (x is Int there), not at the end.
-- 知っている型に対して、式を確かめる。関数なら、先に引数に型を渡す。
-- そうすれば、まちがいは、まちがえた場所で見つかる
check :: TypeEnv -> Expr -> Type -> Infer (Subst, Expr)
check env expr t = case (expr, t) of
  (At loc e, _) -> do
    (s, e') <- inPlace loc (check env e t)
    pure (s, At loc e')
  (Lam y body, TFun a b) -> do
    (s, body') <- scoped (Set.insert y)
      (check (Map.insert y (Forall [] Map.empty a) env) body b)
    pure (s, Lam y body')
  _ -> do
    (s1, te, expr') <- infer env expr
    s2 <- liftUnify te (applyT s1 t)
    pure (s2 `compose` s1, expr')
-- ANCHOR_END: check

-- ANCHOR: renderHelpers
-- 🔢 A type a number can never be: String, a list, a record, Bool, ...
--    (not a function: that has its own words, see numberCalled).
-- 数が決してならない型：String、リスト、レコード、Bool など
--（関数は除く。関数には専用の文がある）
plainType :: Type -> Bool
plainType t = case t of
  TCon _ _     -> True
  TBool        -> True
  TRecord _ _  -> True
  _            -> False

-- 🖨️ How the lines below use a value: "String", or "a number".
-- 下の行が値をどう使うか
asUsed :: Type -> Text
asUsed t = case t of
  TVar _ -> "a number"
  _      -> renderType t

-- ❓ The hint for a ? left out: the line gives a Result (or a Maybe),
--    and the lines below want what is inside.
-- ? の付け忘れのヒント：行は Result（か Maybe）を出し、下の行は中身が欲しい
forgotTry :: Type -> Type -> Text
forgotTry given used = case (given, used) of
  (TCon "Result" _, TCon "Result" _) -> ""
  (TCon "Maybe" _, TCon "Maybe" _)   -> ""
  (TCon "Result" _, _) ->
    ": write ? at the end of the line, to go on with what is in Ok"
      <> " (and stop the do at an Err)"
  (TCon "Maybe" _, _) ->
    ": write ? at the end of the line, to go on with what is in Just"
      <> " (and stop the do at Nothing)"
  _ -> ""
-- ANCHOR_END: renderHelpers

-- ANCHOR: render
-- 🖨️ Show a type, naming the variables a, b, c, ... in order.
-- 型を見せる。型変数は、出てきた順に a、b、c、... と呼ぶ
renderType :: Type -> Text
renderType t = renderWith (typeNames t) t

-- ANCHOR: typeNames
-- 🏷️ The names a, b, c, ... of the type variables, in order.
-- 型変数の名前 a、b、c、...（出てきた順）
typeNames :: Type -> Map Int Text
typeNames t =
  Map.fromList (zip rests restNames
                <> zip heads holed
                <> zip (filter (`notElem` (rests <> heads)) vars) plain)
  where
    vars  = nub (order t)
    rests = nub (rowsOf t)
    order ty = case ty of
      TVar n    -> [n]
      TFun a b  -> order a <> order b
      TCon _ ts -> concatMap order ts
      TApp f a  -> order f <> order a
      TRecord fs r -> maybe [] pure r <> concatMap order (Map.elems fs)
      _         -> []
    -- ANCHOR: rowNames
    -- The rest of a record is called r, r1, r2, ...
    -- レコードの残りは r、r1、r2、... と呼ぶ
    rowsOf ty = case ty of
      TRecord fs r -> maybe [] pure r <> concatMap rowsOf (Map.elems fs)
      TFun a b  -> rowsOf a <> rowsOf b
      TCon _ ts -> concatMap rowsOf ts
      _         -> []
    restNames = "r" : [ "r" <> T.pack (show i) | i <- [1 :: Int ..] ]
    -- ANCHOR_END: rowNames
    -- ANCHOR: holeNames
    -- A variable with a hole (the f of  f a) is called f, g, h, ...
    -- as in Haskell.
    -- 穴のある型変数（f a の f）は、Haskell と同じく f、g、h、... と呼ぶ
    heads = nub (holesOf t)
    holesOf ty = case ty of
      TApp (TVar n) a -> n : holesOf a
      TApp f a  -> holesOf f <> holesOf a
      TFun a b  -> holesOf a <> holesOf b
      TCon _ ts -> concatMap holesOf ts
      TRecord fs _ -> concatMap holesOf (Map.elems fs)
      _         -> []
    holed = [ T.singleton c | c <- "fgh" ]
            <> [ "f" <> T.pack (show i) | i <- [1 :: Int ..] ]
    -- ANCHOR_END: holeNames
    plain = [ T.singleton c | c <- ['a' .. 'z']
                            , null rests || c /= 'r'
                            , null heads || c `notElem` ("fgh" :: String) ]
            <> [ "t" <> T.pack (show i) | i <- [1 :: Int ..] ]
-- ANCHOR_END: typeNames

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
      -- ANCHOR: renderApp
      -- f a: an argument with arguments goes into ( ), as in  f (Maybe a)
      -- f a：引数に引数があれば、f (Maybe a) のように ( ) に入れる
      TApp f a -> go names False f <> " " <> argument names a
      -- ANCHOR_END: renderApp
      -- ANCHOR: renderRecord
      -- { name : String, age : Int }  and  { r | name : String }
      -- (the fields in the order of their names)
      -- （フィールドは名前の順）
      TRecord fs rest ->
        let fields = T.intercalate ", "
              [ f <> " : " <> go names False ft | (f, ft) <- Map.toList fs ]
            restOf r = Map.findWithDefault "?" r names
        in case (rest, Map.null fs) of
             (Nothing, True)  -> "{}"
             (Just r, True)   -> restOf r
             (Nothing, False) -> "{ " <> fields <> " }"
             (Just r, False)  -> "{ " <> restOf r <> " | " <> fields <> " }"
      -- ANCHOR_END: renderRecord
    -- An argument of a type name, like a in  Box a.  Wrap it when it
    -- is a function or has arguments itself:  Box (Box a).
    -- 型の名前の引数。関数か、それ自身に引数があれば、括弧で包む
    argument names ty = case ty of
      TFun {} -> "(" <> go names False ty <> ")"
      TCon c (_ : _)
        | c `notElem` ["List", "Tuple"] -> "(" <> go names False ty <> ")"
      TApp {} -> "(" <> go names False ty <> ")"
      _ -> go names False ty

-- ANCHOR: renderScheme
-- 🖨️ A type with its classes first, like Haskell:
--    Comparable a => a -> a -> a
-- クラスの条件を前に付けた型（Haskell と同じ見せ方）
renderScheme :: Scheme -> Text
renderScheme (Forall _ ns t) =
  let names = typeNames t
      -- In the order the variables come in the type, as GHC shows
      -- them:  (Foldable f, Number a) => f a -> a
      -- 型に出てくる順。GHC と同じ
      conds = [ className c <> " " <> name
              | n <- nub (appearing t)
              , Just name <- [Map.lookup n names]
              , c <- maybe [] Set.toList (Map.lookup n ns) ]
      prefix = case conds of
        []  -> ""
        [c] -> c <> " => "
        _   -> "(" <> T.intercalate ", " conds <> ") => "
  in prefix <> renderWith names t
  where
    appearing ty = case ty of
      TVar n       -> [n]
      TFun x y     -> appearing x <> appearing y
      TCon _ ts    -> concatMap appearing ts
      TApp f x     -> appearing f <> appearing x
      TRecord fs r -> maybe [] pure r <> concatMap appearing (Map.elems fs)
      _            -> []
-- ANCHOR_END: renderScheme

-- ANCHOR: defaultNumbers
-- 🔢 Before the REPL shows the type of an answer: a number of any
--    type (Number a => a) is shown as an Int, as Haskell does
--    (default (Integer, Double)). 1 + 2 is 3 : Int.
-- REPL が答えの型を見せる前に：どの型でもよい数（Number a => a）は
-- Int にして見せる（Haskell の default と同じ）。1 + 2 は 3 : Int
defaultNumbers :: Scheme -> Scheme
defaultNumbers (Forall vs ns t) =
  let numbers = [ n | (n, cs) <- Map.toList ns, Number `Set.member` cs ]
      s       = Map.fromList [ (n, TInt) | n <- numbers ]
  in Forall (filter (`notElem` numbers) vs)
            (foldr Map.delete ns numbers)
            (applyT s t)
-- ANCHOR_END: defaultNumbers

-- ANCHOR: renderUsual
-- 🏷️ The type of a value that is not a function, and, when it has
--    numbers of any type, the type it usually is (as an Int) on a
--    second line:
--      treasures : (Comparable a, Number a, ...) => Map (a, b) String
--        -- usually : Map (Int, Int) String
--    A function stays on one line: double : Number a => a -> a really
--    works for both Int and Float.
-- 関数でない値の型。どの型にもなれる数があれば、ふつうの型（Int に
-- したもの）を2行目に添える。関数は1行のまま（Int にも Float にも
-- 本当に使えるので）
renderUsual :: Scheme -> Text
renderUsual sc@(Forall _ _ t) = case t of
  TFun _ _ -> renderScheme sc
  _ | usual /= shown -> shown <> "\n  -- usually : " <> usual
    | otherwise      -> shown
  where
    shown = renderScheme sc
    usual = renderScheme (defaultNumbers sc)
-- ANCHOR_END: renderUsual

-- 📣 A type error, as people read it.
-- 人が読む型のエラー
renderTypeError :: TypeError -> Text
renderTypeError err = case err of
  -- ANCHOR: numberAsFunction
  -- 「Int は関数ではありません。x -1 は x に -1 を渡すので、x - 1 と」
  Mismatch a b@(TFun _ _)
    | a `elem` [TInt, tFloat] ->
        renderType a <> " is not a function, but is called with an "
          <> "argument here (" <> renderType b <> " is needed): x -1 "
          <> "gives -1 to x, so write x - 1"
  -- ANCHOR_END: numberAsFunction
  -- 「Int が要るところに、Bool が来ました」
  Mismatch a b     -> "the types do not fit: " <> renderType a
                        <> " where " <> renderType b <> " is needed"
  -- 「無限に大きな型になります」
  InfiniteType n t -> "an infinite type: " <> renderType (TVar n)
                        <> " = " <> renderType t
                        <> " (is a function applied to itself?)"
  -- 「知らない名前です」
  UnknownName x    -> "unknown name: " <> x
  UnknownNameNear x near ->
    "unknown name: " <> x <> " (did you mean " <> near <> "?)"
  -- ANCHOR: numberCalled
  -- 「数と関数を取りちがえています。x -1 は x に -1 を渡すので x - 1 と、
  --   -f x は (-f) x なので -(f x) と書きます」
  NotInClass Number t@(TFun _ _) ->
    "a number and a function are mixed up (" <> renderType t
      <> " is not in the class Number): x -1 gives -1 to x (write"
      <> " x - 1), and -f x is (-f) x (write -(f x))"
  -- ANCHOR_END: numberCalled
  -- ANCHOR: renderOrElse
  -- 「?: の左には Maybe か Result が要りますが、数が来ました」
  OrElseNeeds t ->
    "?: needs a Maybe or a Result on its left, but got "
      <> (case t of
            TVar _ -> "a number"
            _      -> renderType t)
      <> " (only a Maybe or a Result can be missing its value)"
  -- ANCHOR_END: renderOrElse
  -- ANCHOR: renderNumberWhere
  -- 「String が要るところに、数が来ました」
  NotInClass Number t | plainType t ->
    "the types do not fit: a number where " <> renderType t
      <> " is needed"
  -- ANCHOR_END: renderNumberWhere
  -- 「a -> a は、Comparable のクラスに入っていません」
  NotInClass c t   -> renderType t <> " is not in the class "
                        <> className c
  -- ANCHOR: renderDataErrors
  -- 「Handler は Comparable に入っていません（Button が Int -> Int を持つ）」
  HoldsNotIn k t c inside ->
    renderType t <> " is not in the class " <> className k <> " ("
      <> c <> " holds " <> renderType inside <> ")"
  -- 「知らない構成子です」
  UnknownConstructor c -> "unknown constructor: " <> c
  -- 「Rect の引数は 2 つですが、パターンには 1 つです」
  WrongPatternArgs c n m -> c <> " needs " <> count n
                              <> ", but the pattern has " <> T.pack (show m)
  -- 「知らない型です」
  UnknownType t    -> "unknown type: " <> t
  -- 「知らない型変数です（型の名前の後ろに書いていない）」
  UnknownTypeVar a -> "unknown type variable: " <> a
  -- 「Maybe の型引数は 1 つです」
  WrongTypeArgs t n m -> "the type " <> t <> " needs " <> count n
                           <> ", but got " <> T.pack (show m)
  -- 「List は組み込みの型です」
  BuiltinType t    -> t <> " is a builtin type"
  -- ANCHOR: renderRecordErrors
  -- 「{ name : String } には、フィールド age がありません」
  MissingField f t -> renderType t <> " has no field " <> f
  -- 「next の注釈では a はどんな型でもよいのに、定義では Int です」
  -- (r of { r | ... } stands for the other fields: say so.)
  -- （{ r | ... } の r は、ほかのフィールドを表すので、そう言う）
  TooGeneral x a t -> "the annotation of " <> x <> " says " <> a
                        <> (case t of
                              TRecord {} -> " can be any other fields"
                              _          -> " can be any type")
                        <> ", but the definition needs " <> renderType t
  -- 「swap の注釈では a と b はちがってよいのに、定義では同じです」
  SameVariables x a b -> "the annotation of " <> x <> " says " <> a
                        <> " and " <> b <> " may differ, but the "
                        <> "definition needs them to be the same"
  -- 「largest の注釈に Comparable a が要ります」
  AnnotationNeeds x c a -> "the annotation of " <> x <> " must say "
                        <> className c <> " " <> a
                        <> " (the definition needs it)"
  -- 「知らないクラスです」
  UnknownClass c   -> "unknown class: " <> c
  -- 「r はレコードの残りなので、型としては使えません」
  RowAndType r     -> r <> " is the rest of a record, so it cannot be "
                        <> "a type as well"
  -- 「フィールド age が2度あります」
  DuplicateField f -> "the field " <> f <> " is written twice"
  -- 「フィールド count の既定値は、型 Int に合いません」
  BadDefault f t   -> "the default of the field " <> f
                        <> " does not fit its type " <> renderType t
  -- ANCHOR_END: renderRecordErrors
  -- ANCHOR: renderClassErrors
  -- 「Describe のインスタンスを選べません：a の型がここでは決まりません」
  AmbiguousClass c t -> "cannot choose an instance of " <> className c
                        <> ": the type " <> renderType t
                        <> " is not known here (an annotation can tell it)"
  -- 「クラス Describe は、もうあります」
  -- ANCHOR: renderModuleErrors
  -- 「JDec は、モジュール Json が見せていません（exposing にありません）」
  Hidden c m -> c <> " is hidden by the module " <> m
                  <> " (it is not in its exposing list)"
  -- 「型 Maybe は、モジュール Maybe で決めたものです」
  ForeignType what t m -> "the " <> what <> " " <> t
                            <> " belongs to the module " <> m
  -- ANCHOR_END: renderModuleErrors
  -- 「main は IO ()、[String] -> IO ()、
  --   [String] -> Map String String -> IO ()、レコード -> IO ()、
  --   それらを並べたレコードのどれかでなければなりません」
  BadMain t -> "main must be IO (), [String] -> IO (), "
                 <> "[String] -> Map String String -> IO (), "
                 <> "a record -> IO (), or a record of those (or "
                 <> "IO (Result e ()) in place of IO ()), but it is "
                 <> renderType t
  -- 「場所：……」
  TypeAt loc e -> renderLoc loc <> ": " <> renderTypeError e
  -- ANCHOR: renderHardening
  -- 「この行は Result String String を出しますが、下の行は String として
  --   使っています。行の終わりに ? を書くと、Ok の中身で続けます」
  LineGives given used ->
    "this line gives " <> renderType given
      <> ", but the lines below use it as " <> asUsed used
      <> forgotTry given used
  -- 「? は Err で do を止めるので、do は Result か Maybe を出さなければ
  --   なりませんが、下の行は IO () を出しています（main なら
  --   main : IO (Result String ())。forEach のラムダのような中のものなら、
  --   最後に pure (Ok ()) を書いて、mapM で動かす）」
  TryNeeds t ->
    "? stops the do at an Err (or a Nothing), so the do must give a "
      <> "Result or a Maybe, but the lines below it give " <> renderType t
      <> " (in main: main : IO (Result String ()); in a lambda, as for "
      <> "forEach: end it with pure (Ok ()), and run it with mapM)"
  -- 「おたがいを使う isEven と isOdd には辞書が要るので、型の注釈を
  --   書いてください」
  GroupNeedsAnnotation xs ->
    T.intercalate " and " xs <> " use each other and need a "
      <> "dictionary: write their type annotations"
  GroupIn _ e -> renderTypeError e
  -- ANCHOR_END: renderHardening
  DuplicateClass c -> "the class " <> c <> " is already declared"
  -- 「Comparable は組み込みのクラスで、仲間は言語が決めます」
  BuiltinClassInstance c -> className c <> " is a builtin class: the "
                        <> "language decides its members"
  -- 「Describe Shape のインスタンスは、もうあります」
  DuplicateInstance c t -> "there is already an instance "
                        <> className c <> " " <> t
  -- 「インスタンスの型には、型の名前と、ちがう型変数だけを書けます」
  InstanceType t   -> "an instance is for a type name with different "
                        <> "type variables, like (Maybe a), not " <> t
  -- ANCHOR: renderInstanceKind
  -- 「Foldable には、穴のある型（あと1つ型を受け取る型）が要ります」
  InstanceKind c t n -> c <> " is for a type with " <> holesText n
                        <> " (one that takes " <> T.pack (show n)
                        <> " more type" <> (if n == 1 then "" else "s")
                        <> ", like " <> example n <> "), not " <> t
  -- ANCHOR_END: renderInstanceKind
  -- 「describe の型に、クラスの型変数 a がありません」
  ClassVarMissing m a -> "the type of the method " <> m
                        <> " must have the class's " <> a
  -- 「Describe には、メソッド describ がありません」
  UnknownMethod c m -> "the class " <> c <> " has no method " <> m
  -- 「describe の定義がありません（既定のメソッドもありません）」
  MissingMethod c m -> "the method " <> m <> " of " <> c
                        <> " is not written (and has no default)"
  -- 「describe が2度書かれています」
  DuplicateMethod m -> "the method " <> m <> " is written twice"
  -- ANCHOR_END: renderClassErrors
  where
    count n = T.pack (show n) <> if n == 1 then " argument" else " arguments"
    holesText n = case n of
      0 -> "no hole"
      1 -> "a hole"
      _ -> T.pack (show n) <> " holes"
    example n = case n of
      0 -> "Int or (Maybe a)"
      1 -> "List or Maybe"
      _ -> "Result or Map"
  -- ANCHOR_END: renderDataErrors
-- ANCHOR_END: render
-- ANCHOR_END: all
