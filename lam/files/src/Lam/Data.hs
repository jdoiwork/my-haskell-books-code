{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | data declarations: check the types written in them, make the
--   constructors, and decide if the new type is Comparable.
-- data の宣言：書かれた型を確かめ、構成子を作り、新しい型が
-- Comparable かどうかを決める。
module Lam.Data
  ( declareData
  ) where

import Control.Monad (when)
import Data.Map (Map)
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
  when (t `elem` [ "Int", "Bool", "String", "List", "Tuple", "Map", "IO"
                 , "Regex", "Parser" ]) $
    Left (BuiltinType t)
  let arity  = length params
      vars   = Map.fromList (zip params [0 ..])   -- a is t0, b is t1, ...
      self   = TCon t (map TVar [0 .. arity - 1]) -- Maybe a
      -- The new type can be used inside itself: data Tree a = ... Tree a
      -- 新しい型は、自分の中でも使える
      arities = Map.insert t arity (Map.map typeArity (dataTypes table))
  fields <- traverse (traverse (traverse (toType arities vars))) cons
  let rows =
        [ (c, Constructor t i (length ts) scheme)
        | (i, (c, ts)) <- zip [0 ..] fields
        , let scheme =
                Forall [0 .. arity - 1] Map.empty (foldr TFun self ts) ]
      -- ANCHOR: derived
      -- Each class the new type gets from its insides (by itself).
      -- 新しい型が、中身から自動で入るクラスごとに
      dt = DataType arity $ Map.fromList
             [ (cls, vs)
             | cls <- derivable
             , Right vs <- [derive table cls t fields] ]
      -- ANCHOR_END: derived
      table' = DataTable
        { dataTypes    = Map.insert t dt (dataTypes table)
        , constructors = Map.union (Map.fromList rows) (constructors table) }
  pure (table', [ (c, conScheme con) | (c, con) <- rows ])
-- ANCHOR_END: declare

-- ANCHOR: toType
-- 🏷️ A written type, as a Type.  Maybe a  is  TCon "Maybe" [TVar 0].
-- 書かれた型を、Type にする
toType :: Map Name Int -> Map Name Int -> TypeExpr -> Either TypeError Type
toType arities vars te = case te of
  TyVar a ->
    maybe (Left (UnknownTypeVar a)) (Right . TVar) (Map.lookup a vars)
  TyFun a b -> TFun <$> go a <*> go b
  TyList a -> tList <$> go a
  TyTuple ts -> tTuple <$> traverse go ts
  TyCon "Int" [] -> Right TInt
  TyCon "Bool" [] -> Right TBool
  TyCon "String" [] -> Right tString
  -- ANCHOR: toTypeMap
  -- Map Json: a map from strings to Json.
  -- Map Json：文字列から Json への Map
  TyCon "Map" [a] -> tMap <$> go a
  TyCon "Map" args -> Left (WrongTypeArgs "Map" 1 (length args))
  -- ANCHOR_END: toTypeMap
  -- ANCHOR: toTypeIO
  -- IO (): an action, kept in a data type.
  -- IO ()：data の型の中に持つ頼みごと
  TyCon "IO" [a] -> tIO <$> go a
  TyCon "IO" args -> Left (WrongTypeArgs "IO" 1 (length args))
  -- ANCHOR_END: toTypeIO
  -- ANCHOR: toTypeRegex
  -- Regex: a compiled regular expression (no type arguments).
  -- Regex：コンパイルした正規表現（型の引数はない）
  TyCon "Regex" [] -> Right (TCon "Regex" [])
  TyCon "Regex" args -> Left (WrongTypeArgs "Regex" 0 (length args))
  -- ANCHOR_END: toTypeRegex
  -- ANCHOR: toTypeParser
  -- Parser Int: a parser that gives an Int, kept in a data type.
  -- Parser Int：Int をくれるパーサー。data の型の中に持つ
  TyCon "Parser" [a] -> TCon "Parser" <$> traverse go [a]
  TyCon "Parser" args -> Left (WrongTypeArgs "Parser" 1 (length args))
  -- ANCHOR_END: toTypeParser
  TyCon c args -> case Map.lookup c arities of
    Nothing -> Left (UnknownType c)
    Just n
      | n /= length args -> Left (WrongTypeArgs c n (length args))
      | otherwise        -> TCon c <$> traverse go args
  where
    go = toType arities vars
-- ANCHOR_END: toType

-- ANCHOR_END: all
