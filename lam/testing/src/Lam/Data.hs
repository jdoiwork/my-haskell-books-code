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

import Control.Monad (when)
import qualified Data.Map as Map

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
  , "IO", "Bytes", "Regex", "Parser", "Task"
  , "Chan", "Pid" ]
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

-- ANCHOR: movedToTypes
-- annotationScheme, toType, varsIn and fits are in Lam.Types now: the
-- type checker reads the annotations in a let and a where too.
-- annotationScheme・toType・varsIn・fits は、いまは Lam.Types にある。
-- 型検査器も、let や where の中の注釈を読むため
-- ANCHOR_END: movedToTypes

-- 🎚️ Does the default fit the type? (see fitsDefault)
-- 既定値は型に合うか（fitsDefault を見る）
fits :: DataTable -> Default -> Type -> Bool
fits = fitsDefault


-- ANCHOR_END: all
