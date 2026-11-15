-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of the chapter on data and case.
-- data と case の章のトレーニングの解答。
module Lam.Answers
  ( missingBranches
  , dataDeclDeriving
  , defineDataDeriving
  ) where

import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Text (Text)
import Text.Megaparsec (many, notFollowedBy, optional, sepBy1)

import Lam.Parser
import Lam.Program
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: missing
-- ⚠️ Step 1: for every case in the expression, the constructors that
--    no branch looks at.  case s of | Circle r -> ...  misses Rect.
--    Only the constructor at the top of each pattern counts: a branch
--    Circle 0 is taken to cover every Circle.
-- ステップ1：式の中の case ごとに、どの枝も見ていない構成子。
-- パターンのいちばん外の構成子だけを見る（Circle 0 の枝も、
-- Circle を全部見たことにする）
missingBranches :: DataTable -> Expr -> [[Name]]
missingBranches table e = here <> concatMap (missingBranches table) (inside e)
  where
    here = case e of
      Case _ branches
        | let ps = map fst branches
        , not (any catchesAll ps)
        , PCon c _ : _ <- [ p | p@(PCon _ _) <- ps ]
        , Just con <- Map.lookup c (constructors table) ->
            let seen = [ c' | PCon c' _ <- ps ]
                every = [ c' | (c', other) <- sortOn (conIndex . snd)
                                   (Map.toList (constructors table))
                             , conTypeName other == conTypeName con ]
            in [ filter (`notElem` seen) every | any (`notElem` seen) every ]
      _ -> []
    catchesAll p = case p of
      PVar _ -> True
      PWild  -> True
      _      -> False

-- 🌳 The expressions just inside an expression.
-- 式のすぐ内側の式
inside :: Expr -> [Expr]
inside e = case e of
  Lit _          -> []
  Var _          -> []
  Lam _ body     -> [body]
  App f a        -> [f, a]
  Let _ a body   -> [a, body]
  If c a b       -> [c, a, b]
  BinOp _ l r    -> [l, r]
  Tuple es       -> es
  List es        -> es
  Case s bs      -> s : map snd bs
-- ANCHOR_END: missing

-- ANCHOR: deriving
-- 🏷️ Step 4: data Shape = Circle Int | Rect Int Int deriving Comparable
--    True when  deriving Comparable  is written.
-- ステップ4：deriving Comparable と書いてあれば True
dataDeclDeriving :: Parser (DataDecl, Bool)
dataDeclDeriving = do
  keyword "data"
  t      <- conName
  params <- many name
  _      <- symbol "="
  _      <- optional bar
  cons   <- constructor `sepBy1` bar
  derive <- optional (keyword "deriving" *> symbol "Comparable")
  pure (DataDecl t params cons, derive /= Nothing)
  where
    -- deriving is not a type variable.
    -- deriving は、型変数ではない
    constructor = (,) <$> conName
      <*> many (continuing *> notFollowedBy (keyword "deriving") *> typeAtom)

-- 📐 Without deriving, the type is never Comparable. With it, the type
--    must be able to be, or the declaration is an error.
-- deriving がなければ、決して Comparable にしない。あれば、なれる型で
-- なければ、宣言のエラーにする
defineDataDeriving
  :: Bool -> Session -> DataDecl -> Either Text (Session, Text)
defineDataDeriving derive session decl = do
  (s, shown) <- defineData session decl
  let table = sessionData s
      t     = dataName decl
  case Map.lookup t (dataTypes table) of
    Just dt
      | not derive ->
          let dt' = dt { comparable = Nothing }
              table' = table { dataTypes = Map.insert t dt' (dataTypes table) }
          in Right (s { sessionData = table' }, shown)
      | comparable dt == Nothing ->
          -- 「Handler は Comparable にできません：
          --   Button が Int -> Int を持つ」
          Left $ case comparability table t (fieldsOf table t) of
            Left (c, why) -> t <> " cannot derive Comparable: " <> c
                               <> " holds " <> renderType why
            Right _       -> t <> " cannot derive Comparable"
    _ -> Right (s, shown)
-- ANCHOR_END: deriving
-- ANCHOR_END: all
