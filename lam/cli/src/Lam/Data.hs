{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | data declarations: check the types written in them, make the
--   constructors, and decide if the new type is Comparable.
-- data の宣言：書かれた型を確かめ、構成子を作り、新しい型が
-- Comparable かどうかを決める。
module Lam.Data
  ( declareData
  , declareAlias
  , annotationScheme
  , builtinTypes
  , toType
  , varsIn
  , fits
  ) where

import Control.Monad (unless, when)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lam.Syntax
import Lam.Types

-- ANCHOR: declare
-- 📐 Add a data type to the table. The answer also has each
--    constructor's name and type, for the environment.
--      data Maybe a = Nothing | Just a
--    gives  Nothing : Maybe a  and  Just : a -> Maybe a.
-- data の型を表に足す。答えには、環境に入れる構成子の名前と型も入る
declareData
  :: DataTable -> DataDecl -> Either TypeError (DataTable, [(Name, Scheme)])
declareData table (DataDecl t params cons) = do
  when (t `elem` builtinTypes) $
    Left (BuiltinType t)
  let arity  = length params
      vars   = Map.fromList (zip params [0 ..])   -- a is t0, b is t1, ...
      self   = TCon t (map TVar [0 .. arity - 1]) -- Maybe a
      -- The new type can be used inside itself: data Tree a = ... Tree a
      -- 新しい型は、自分の中でも使える
      known = table { dataTypes = Map.insert t (DataType arity Map.empty)
                                                 (dataTypes table) }
  fields <- traverse (traverse (traverse (toType known vars))) cons
  let rows =
        [ (c, Constructor t i (length ts) scheme)
        | (i, (c, ts)) <- zip [0 ..] fields
        , let scheme =
                Forall [0 .. arity - 1] Map.empty (foldr TFun self ts) ]
      -- ANCHOR: derived
      -- Each class the type can join by what it holds, and when.
      -- 中身から入れるクラスごとに、入る条件
      dt = DataType arity $ Map.fromList
             [ (cls, needed)
             | cls <- derivable
             , Right needed <- [derive table cls t fields] ]
      -- ANCHOR_END: derived
      table' = table
        { dataTypes    = Map.insert t dt (dataTypes table)
        , constructors = Map.union (Map.fromList rows) (constructors table) }
  pure (table', [ (c, conScheme con) | (c, con) <- rows ])
-- ANCHOR_END: declare

-- ANCHOR: builtinTypes
-- 🧱 The types the language has by itself: no data or alias may take
--    their names.
-- 言語がはじめから持つ型。data も別名も、この名前は使えない
builtinTypes :: [Name]
builtinTypes =
  [ "Int", "Float", "Bool", "String", "List", "Tuple", "Map", "Set"
  , "IO", "Bytes", "Regex", "Parser" ]
-- ANCHOR_END: builtinTypes

-- ANCHOR: declareAlias
-- 🏷️ Add a type alias to the table.  type alias Member = { ... }
--    The type it stands for may use the aliases above it, but not
--    itself (it would never end).
-- 型の別名を表に足す。表す型には、上にある別名は使えるが、自分自身は
-- 使えない（終わらなくなる）
declareAlias :: DataTable -> AliasDecl -> Either TypeError DataTable
declareAlias table (AliasDecl t params body) = do
  when (t `elem` builtinTypes) $
    Left (BuiltinType t)
  let vars = Map.fromList (zip params [0 ..])
  ty <- toType table vars body
  pure table
    { aliases = Map.insert t (Alias (length params) ty body)
                          (aliases table) }
-- ANCHOR_END: declareAlias

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
                  , not (fits table d t) ] of
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
fits :: DataTable -> Default -> Type -> Bool
fits table d t = case (d, t) of
  (DefaultOne (LInt _),    TInt)              -> True
  (DefaultOne (LInt _),    TCon "Float" [])   -> True
  (DefaultOne (LFloat _),  TCon "Float" [])   -> True
  (DefaultOne (LString _), TCon "String" [])  -> True
  (DefaultOne (LBool _),   TBool)             -> True
  (DefaultCon c,           TCon h _)          -> constructorOf c h
  (DefaultList ds,         TCon "List" [a])   -> all (\x -> fits table x a) ds
  _                                           -> False
  where
    constructorOf c h = case Map.lookup c (constructors table) of
      Just con -> conTypeName con == h && conArity con == 0
      Nothing  -> False
-- ANCHOR_END: fits
-- ANCHOR_END: toType

-- ANCHOR_END: all
