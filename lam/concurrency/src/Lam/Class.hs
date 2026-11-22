{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Classes and instances declared in a program: check them, and turn
--   them into plain values (the methods and the dictionaries).
-- プログラムで宣言したクラスとインスタンス：確かめて、ふつうの値
-- （メソッドと辞書）にする。
module Lam.Class
  ( declareClass
  , declareInstance
  , defaultName
  , methodName
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.List (nub, sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Data
import Lam.Pretty (renderTypeExpr)
import Lam.Syntax
import Lam.Types

-- ANCHOR: names
-- 🏷️ The values a class and an instance make. Their names start with
--    $, so a program can never write them.
--      $default$Describe$describeAll     a default
--      $Describe$Shape$describe          a method of an instance
-- クラスとインスタンスが作る値。名前は $ で始まるので、プログラムからは
-- 書けない
defaultName :: Name -> Name -> Name
defaultName c m = "$default$" <> c <> "$" <> m

methodName :: Name -> Name -> Name -> Name
methodName c h m = "$" <> c <> "$" <> h <> "$" <> m
-- ANCHOR_END: names

-- ANCHOR: declareClass
-- 🎓 class Describe a where ...
--    Each method gets a type with the class on a:
--      describe : Describe a => a -> String
--    and a value that takes a dictionary and picks the method from it:
--      describe = \$d -> $d.describe
--    A default is checked against the type of its method, and becomes
--    a value of its own that takes the dictionary.
--    The answer: the new table, the types of the methods, and the
--    values to compute.
-- 各メソッドは、a にクラスの付いた型と、辞書を受け取ってメソッドを
-- 取り出す値になる。既定のメソッドは、メソッドの型で確かめ、辞書を
-- 受け取る、自分の値になる。答えは、新しい表、メソッドの型、計算する値
declareClass
  :: DataTable -> TypeEnv -> ClassDecl
  -> Either TypeError (DataTable, [(Name, Scheme)], [(Name, Expr)])
declareClass table env (ClassDecl supers c a sigs defaults) = do
  when (lookupClass table c /= Nothing) $ Left (DuplicateClass c)
  -- The classes above: known ones, of the same a.
  -- 上位のクラス：知っているもので、同じ a のもの
  above <- forM supers $ \(k, b) -> do
    unless (b == a) $ Left (UnknownTypeVar b)
    maybe (Left (UnknownClass k)) Right (lookupClass table k)
  -- ANCHOR: kindOf
  -- How many types a takes: f a in a method's type means 1.
  -- a が受け取る型の数。メソッドの型に f a があれば 1
  let kind = maximum (0 : concatMap (appliedTo a . sigType . snd) sigs)
  -- ANCHOR_END: kindOf
      info0  = ClassInfo above kind [] []
      table0 = table { classes = Map.insert c info0 (classes table) }
  -- Each method's type, with Describe a in front.
  -- メソッドの型。前に Describe a を付ける
  methods <- forM sigs $ \(m, Signature _ te) -> do
    unless (a `elem` varsIn te) $ Left (ClassVarMissing m a)
    (sc, names) <- annotationScheme table0 (Signature [(c, a)] te)
    pure (m, sc, names)
  let ms = [ m | (m, _, _) <- methods ]
  -- A method's type and its default are one each.
  -- メソッドの型と既定のメソッドは、1つずつ
  forM_ (duplicates ms <> duplicates (map fst defaults)) $ \m ->
    Left (DuplicateMethod m)
  forM_ defaults $ \(m, _) ->
    unless (m `elem` ms) $ Left (UnknownMethod c m)
  let info   = ClassInfo above kind methods (map fst defaults)
      table1 = table { classes = Map.insert c info (classes table) }
      types  = [ (m, sc) | (m, sc, _) <- methods ]
      env1   = Map.fromList types `Map.union` env
      pick m = Lam "$d" (Field (Var "$d") m)
  -- ANCHOR: defaultValues
  -- A default may use every method of the class (its own, too).
  -- 既定のメソッドは、クラスのどのメソッドも使える（自分自身も）
  defaultValues <- forM defaults $ \(m, e) -> do
    (sc, names) <- typeOf m methods
    (_, e') <- inferAnnotated table1 env1 m names sc e
    pure (defaultName c m, e')
  -- ANCHOR_END: defaultValues
  pure (table1, types, [ (m, pick m) | m <- ms ] <> defaultValues)
  where
    sigType (Signature _ te) = te
    duplicates xs = nub [ x | x <- xs, count x xs > 1 ]
    count x = length . filter (== x)
-- ANCHOR_END: declareClass

-- ANCHOR: declareInstance
-- 🧩 instance Describe a => Describe (Maybe a) where describe m = ...
--    The instance goes into the table first, so that its methods can
--    use it (describe of a Maybe inside a Maybe). Then:
--      1. the classes above must fit (Describe needs nothing above),
--      2. each method is checked against its type, with Maybe a for a,
--      3. the dictionary is a record of the methods (a function of the
--         dictionaries of the context, if there is one):
--           $Describe$Maybe = \$Describe_a ->
--             let $self =
--                   { describe = $Describe$Maybe$describe $Describe_a
--                   , describeAll = $default$Describe$describeAll $self }
--             in $self
--         (a default gets the dictionary itself, $self)
--    The answer: the new table, the values to compute, and a label.
-- インスタンスは先に表に入れる。そうすれば、メソッドの中から使える
-- （Maybe の中の Maybe の describe）。それから：
--   1. 上位のクラスに合うか
--   2. メソッドを、a を Maybe a にした型で確かめる
--   3. 辞書は、メソッドを並べたレコード（文脈があれば、その辞書を
--      受け取る関数）。既定のメソッドは、辞書そのもの $self を受け取る
-- 答えは、新しい表、計算する値、見せる文字
declareInstance
  :: DataTable -> TypeEnv -> InstanceDecl
  -> Either TypeError (DataTable, [(Name, Expr)], Text)
declareInstance table env (InstanceDecl context c te defs) = do
  cls <- maybe (Left (UnknownClass c)) Right (lookupClass table c)
  info <- case Map.lookup c (classes table) of
    Just i  -> Right i
    Nothing -> Left (BuiltinClassInstance cls)
  -- The type: a name with different type variables.  Maybe a  is
  -- t0 inside Maybe.
  -- 型：名前と、ちがう型変数。Maybe a は、Maybe の中に t0
  let vars  = nub (varsIn te)
      index = Map.fromList (zip vars [0 ..])
      arity = length vars
  ty <- headType table index te
  h <- case typeHead ty of
    Just (h, args) | args == map TVar [0 .. arity - 1] -> Right h
    _ -> Left (InstanceType (renderType ty))
  -- ANCHOR: instanceKind
  -- The holes left must be as many as the class's a takes:
  -- Foldable List (1 hole), Describe (Maybe a) (none).
  -- 残った穴の数は、クラスの a が受け取る型の数と同じ
  let holesLeft = maybe 0 (subtract arity) (fullArity table h)
  unless (holesLeft == classKind info) $
    Left (InstanceKind c (renderTypeExpr 0 te) (classKind info))
  -- ANCHOR_END: instanceKind
  when (Map.member (c, h) (instances table)) $
    Left (DuplicateInstance cls h)
  -- The context: Describe a gives (Describe, 0).
  -- 文脈：Describe a は (Describe, 0)
  needs <- forM context $ \(k, b) -> do
    kc <- maybe (Left (UnknownClass k)) Right (lookupClass table k)
    i  <- maybe (Left (UnknownTypeVar b)) Right (Map.lookup b index)
    pure (kc, i)
  let ctx    = nub (sortOn (\(k, i) -> (i, k)) needs)
      table1 = table { instances = Map.insert (c, h) (Instance arity ctx)
                                     (instances table) }
  aboveDicts <- checkInstanceSupers table1 (classSuper info) ty vars ctx
  -- The methods: each one once, known, and none missing.
  -- メソッド：どれも1度だけ、知っているもので、足りないものはない
  let ms       = [ m | (m, _, _) <- classMethodsOf info ]
      written  = map fst defs
  forM_ written $ \m -> do
    unless (m `elem` ms) $ Left (UnknownMethod c m)
    when (length (filter (== m) written) > 1) $ Left (DuplicateMethod m)
  forM_ ms $ \m ->
    unless (m `elem` written || m `elem` classDefaultsOf info) $
      Left (MissingMethod c m)
  methodValues <- forM defs $ \(m, e) -> do
    (sc, names) <- typeOf m (classMethodsOf info)
    let (sc', names') = forInstance cls sc names ty arity ctx vars
    (_, e') <- checkAnnotated table1 env m names' sc' e
    pure (methodName c h m, e')
  -- ANCHOR: dictionary
  -- The dictionary is itself under the name $self: a default gets it.
  -- 辞書は、$self という名前で自分自身。既定のメソッドが受け取る
  let params = [ dictParam k (vars !! i) | (k@(Declared _), i) <- ctx ]
      self   = Var "$self"
      field m
        | m `elem` written =
            (m, foldl App (Var (methodName c h m)) (map Var params))
        | otherwise = (m, App (Var (defaultName c m)) self)
      record = Record (map field ms
                       <> [ ("$" <> n, d) | (n, d) <- aboveDicts ])
      dict   = foldr Lam (Let "$self" record self) params
  -- ANCHOR_END: dictionary
  pure (table1, methodValues <> [(dictName c h, dict)], label)
  where
    label = "instance " <> contextText <> c <> " " <> renderTypeExpr 2 te
    contextText = case context of
      []  -> ""
      [k] -> cond k <> " => "
      ks  -> "(" <> T.intercalate ", " (map cond ks) <> ") => "
    cond (k, b) = k <> " " <> b
-- ANCHOR_END: declareInstance

-- ANCHOR: appliedTo
-- 🕳️ How many types a takes, each place it is written:
--    f a -> [a]  gives [1, 0] for f.
-- a が受け取る型の数。書かれた場所ごとに
appliedTo :: Name -> TypeExpr -> [Int]
appliedTo a te = case te of
  TyApp f ts    -> [ length ts | f == a ] <> concatMap (appliedTo a) ts
  TyVar f       -> [ 0 | f == a ]
  TyCon _ ts    -> concatMap (appliedTo a) ts
  TyFun x y     -> appliedTo a x <> appliedTo a y
  TyList x      -> appliedTo a x
  TyTuple ts    -> concatMap (appliedTo a) ts
  TyRecord fs _ -> concatMap (appliedTo a . fieldType) fs
-- ANCHOR_END: appliedTo

-- ANCHOR: headType
-- 🧩 The type of an instance, which may leave holes:  List,
--    (Map k), (Result e)  are a type name with fewer types than it
--    takes. Anything else is read as a type, as usual.
-- インスタンスの型。穴を残してよい：List、(Map k)、(Result e) は、
-- 受け取る数より少ない型を持つ型の名前。ほかは、いつもどおり型として読む
headType :: DataTable -> Map.Map Name Int -> TypeExpr -> Either TypeError Type
headType table index te = case te of
  TyCon c args
    | Just n <- arityOf c, length args < n ->
        TCon c <$> traverse (toType table index) args
  _ -> toType table index te
  where
    arityOf c
      | c `elem` ["List", "Set", "IO", "Parser", "Task"] = Just 1
      | c == "Map"               = Just 2
      | otherwise = typeArity <$> Map.lookup c (dataTypes table)

-- 🔢 How many types a type name takes in all:  Map  takes 2. A tuple
--    takes as many as it has (it never leaves a hole).
-- 型の名前が受け取る型の数（全部で）。タプルは、持っている数だけ
-- （穴を残さない）
fullArity :: DataTable -> Name -> Maybe Int
fullArity table h = case h of
  _ | h `elem` ["List", "Set", "IO", "Parser", "Task"] -> Just 1
  "Map"   -> Just 2
  "Tuple" -> Nothing
  _ -> Just (maybe 0 typeArity (Map.lookup h (dataTypes table)))
-- ANCHOR_END: headType

-- 🔎 The type of a method (and the names of its type variables).
-- メソッドの型（と型変数の名前）
typeOf
  :: Name -> [(Name, Scheme, Map.Map Int Name)]
  -> Either TypeError (Scheme, Map.Map Int Name)
typeOf m methods = case [ (s, n) | (m', s, n) <- methods, m' == m ] of
  found : _ -> Right found
  []        -> Left (UnknownName m)

-- ANCHOR: forInstance
-- 🔁 The type of a method in an instance: the class's a becomes the
--    type of the instance, and its type variables get the context.
--      describe : Describe a => a -> String
--    in  instance Describe b => Describe (Maybe b)  is
--      describe : Describe b => Maybe b -> String
--    The instance's type variables get new numbers after the method's.
-- インスタンスの中のメソッドの型：クラスの a を、インスタンスの型にし、
-- その型変数に文脈を付ける。インスタンスの型変数には、メソッドの
-- 型変数のあとの、新しい番号を付ける
forInstance
  :: Class -> Scheme -> Map.Map Int Name -> Type -> Int -> [(Class, Int)]
  -> [Name] -> (Scheme, Map.Map Int Name)
forInstance cls (Forall vs ns t) names ty arity ctx written =
  let a     = case [ v | (v, cs) <- Map.toList ns, cls `Set.member` cs ] of
                v : _ -> v
                []    -> 0
      base  = 1 + maximum (0 : vs)
      moved = substitute' (Map.fromList [ (i, TVar (base + i))
                                        | i <- [0 .. arity - 1] ]) ty
      t'    = substitute' (Map.singleton a moved) t
      vs'   = filter (/= a) vs <> [ base + i | i <- [0 .. arity - 1] ]
      ns'   = Map.unionWith (<>) (Map.delete a ns)
                (Map.fromListWith (<>) [ (base + i, Set.singleton k)
                                       | (k, i) <- ctx ])
      names' = Map.union (Map.delete a names)
                 (Map.fromList (zip [ base + i | i <- [0 ..] ] written))
  in (Forall vs' ns' t', names')
  where
    substitute' = substituteVars
-- ANCHOR_END: forInstance
-- ANCHOR_END: all
