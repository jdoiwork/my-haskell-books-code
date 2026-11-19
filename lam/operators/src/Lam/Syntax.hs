{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The abstract syntax tree (AST) of Lam: a program, as a value.
-- Lam の抽象構文木（AST）：プログラムを、値として表す。
module Lam.Syntax
  ( Name
  , Expr (..)
  , Literal (..)
  , Op (..)
  , Pattern (..)
  , TypeExpr (..)
  , DataDecl (..)
  , Signature (..)
  , AliasDecl (..)
  , ClassDecl (..)
  , InstanceDecl (..)
  , Decl (..)
  , Program (..)
  , Header (..)
  , Import (..)
  , Exposing (..)
  , Exposed (..)
  , Shown (..)
  , Assoc (..)
  , Fixity (..)
  , Operators
  , Loc (..)
  , renderLoc
  , baseName
  , freeVars
  , patternVars
  ) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- ANCHOR: expr
-- 🏷️ A variable's name: x, double, fact
-- 変数の名前
type Name = Text

-- 🌳 An expression of Lam. Everything in Lam is an expression.
-- Lam の式。Lam では、すべてが式
data Expr
  = Lit Literal            -- 42, true, "hello"
  | Var Name               -- x, and constructors: Just, Circle
  | Lam Name Expr          -- \x -> body
  | App Expr Expr          -- f x
  | Let Name Expr Expr     -- let x = e in body
  | If Expr Expr Expr      -- if c then a else b
  | BinOp Op Expr Expr     -- a + b
  | Tuple [Expr]           -- (1, "one")    (two or more)
  | List [Expr]            -- [1, 2, 3]
  | Case Expr [(Pattern, Expr)]   -- case e of | p -> a | q -> b
  -- ANCHOR: recordExprs
  | Record [(Name, Expr)]          -- { name = "Lam", age = 3 }
  | Field Expr Name                -- r.name
  | Update Expr [(Name, Expr)]     -- { r | age = 4 }
  -- ANCHOR_END: recordExprs
  deriving (Eq, Show)
-- ANCHOR_END: expr

-- 🔢 The values you can write directly.
-- そのまま書ける値
data Literal
  = LInt Integer           -- 42, -7
  -- ANCHOR: lfloat
  | LFloat Double          -- 1.5, 1e3, -0.25
  -- ANCHOR_END: lfloat
  | LBool Bool             -- true, false
  | LString Text           -- "hello"
  deriving (Eq, Show)

-- ➕ The operators.
-- 演算子
data Op
  = Add | Sub | Mul | Div            -- + - * /
  -- ANCHOR: newOps
  | IntDiv                           -- //  (whole numbers)
  | DivMaybe | IntDivMaybe           -- /? //?  (Nothing for 0)
  | OrElse                           -- ?:  (Nothing, Err: the right side)
  -- ANCHOR_END: newOps
  | Eq | Ne | Lt | Le | Gt | Ge      -- == /= < <= > >=
  | Concat                           -- ++  (strings)
  | Cons                             -- :   (an item in front of a list)
  | And | Or                         -- && ||
  deriving (Eq, Show, Enum, Bounded)

-- ANCHOR: pattern
-- 🧩 A pattern: the shape a value must have, with names for its parts.
-- パターン：値の形と、部品に付ける名前
data Pattern
  = PVar Name                -- x       (anything; call it x)
  | PWild                    -- _       (anything; no name)
  | PLit Literal             -- 0, "hi", true
  | PCon Name [Pattern]      -- Just x, Circle r, Nothing
  | PTuple [Pattern]         -- (name, score)
  | PList [Pattern]          -- [], [x], [x, y]
  | PCons Pattern Pattern    -- x : rest
  -- ANCHOR: precord
  | PRecord [Name]           -- { name, age }
  -- ANCHOR_END: precord
  deriving (Eq, Show)
-- ANCHOR_END: pattern

-- ANCHOR: decl
-- 🏷️ A type, as written in a data declaration: Int, [a], Maybe a, a -> b
-- data の宣言に書く型
data TypeExpr
  = TyCon Name [TypeExpr]    -- Int, Maybe a, Tree a
  | TyVar Name               -- a
  -- ANCHOR: tyApp
  | TyApp Name [TypeExpr]    -- f a  (a type variable with types after it)
  -- ANCHOR_END: tyApp
  | TyFun TypeExpr TypeExpr  -- a -> b
  | TyList TypeExpr          -- [a]
  | TyTuple [TypeExpr]       -- (a, b)
  -- ANCHOR: tyRecord
  -- { name : String, age : Int },  and  { r | name : a }  (r stands
  -- for the other fields, if any).
  -- r は、ほかのフィールド（あれば）を表す
  | TyRecord [(Name, TypeExpr)] (Maybe Name)
  -- ANCHOR_END: tyRecord
  deriving (Eq, Show)

-- 📐 data Shape = Circle Int | Rect Int Int
--    The name of the type, its type variables, and the constructors
--    with the types of their arguments.
-- 型の名前、型変数、構成子とその引数の型
data DataDecl = DataDecl
  { dataName   :: Name                  -- Shape
  , dataParams :: [Name]                -- []   (Maybe a has [a])
  , dataCons   :: [(Name, [TypeExpr])]  -- Circle [Int], Rect [Int, Int]
  }
  deriving (Eq, Show)

-- ANCHOR: signature
-- ✍️ A type annotation:  largest : Comparable a => [a] -> Maybe a
--    The classes (by name, with their type variable), then the type.
-- 型の注釈。クラス（名前と型変数）と、型
data Signature = Signature [(Name, Name)] TypeExpr
  deriving (Eq, Show)

-- 🏷️ type alias Member = { name : String, days : Int }
--    A new name for a type that is already there (not a new type).
-- もうある型に付ける、新しい名前（新しい型ではない）
data AliasDecl = AliasDecl
  { aliasName   :: Name             -- Member
  , aliasParams :: [Name]           -- []   (Pair a has [a])
  , aliasType   :: TypeExpr         -- { name : String, days : Int }
  }
  deriving (Eq, Show)
-- ANCHOR_END: signature

-- ANCHOR: classDecl
-- 🎓 class Describe a where
--      describe : a -> String
--      describeAll : [a] -> String
--      describeAll xs = ...            (a default: may be left out)
--    The classes above it (Comparable a => ...), the name, its type
--    variable, the types of the methods, and the defaults.
-- 上位のクラス（Comparable a => ...）、名前、型変数、メソッドの型、
-- 既定のメソッド
data ClassDecl = ClassDecl
  { classSupers   :: [(Name, Name)]        -- [(Comparable, a)]
  , classDeclName :: Name                  -- Describe
  , classVar      :: Name                  -- a
  , classMethods  :: [(Name, Signature)]   -- describe : a -> String
  , classDefaults :: [(Name, Expr)]        -- describeAll xs = ...
  }
  deriving (Eq, Show)

-- 🧩 instance Describe a => Describe (Maybe a) where
--      describe m = ...
--    The classes the type variables need, the class, the type, and
--    the methods.
-- 型変数に要るクラス、クラス、型、メソッド
data InstanceDecl = InstanceDecl
  { instContext :: [(Name, Name)]          -- [(Describe, a)]
  , instClass   :: Name                    -- Describe
  , instType    :: TypeExpr                -- Maybe a
  , instMethods :: [(Name, Expr)]          -- describe m = ...
  }
  deriving (Eq, Show)
-- ANCHOR_END: classDecl

-- 📜 One line of a program: a definition, a data declaration, a type
--    annotation, a type alias, a class, or an instance.
-- プログラムの1つの宣言：定義、data の宣言、型の注釈、型の別名、
-- クラス、インスタンス
data Decl
  = DefValue Name Expr       -- let x = e
  | DefData DataDecl         -- data Shape = ...
  -- ANCHOR: declMore
  | DefSig Name Signature    -- add : Int -> Int -> Int
  | DefAlias AliasDecl       -- type alias Member = { ... }
  -- ANCHOR_END: declMore
  -- ANCHOR: declClass
  | DefClass ClassDecl       -- class Describe a where ...
  | DefInstance InstanceDecl -- instance Describe Shape where ...
  -- ANCHOR_END: declClass
  -- ANCHOR: declInfix
  | DefInfix Loc Text Fixity -- infixl 7 addPrice as <+>
  -- ANCHOR_END: declInfix
  deriving (Eq, Show)

-- ANCHOR: fixity
-- ➕ How an operator is read: which way it leans, how tight it is
--    (0 to 9, 9 is the tightest), and the function it calls.
--    LeftA:  1 - 2 - 3  is  (1 - 2) - 3.
--    RightA: 1 : 2 : xs  is  1 : (2 : xs).
--    NoneA:  a == b == c  is an error.
-- 演算子の読み方：どちらに寄るか、強さ（0〜9。9 がいちばん強い）、
-- 呼ぶ関数
data Assoc = LeftA | RightA | NoneA
  deriving (Eq, Show)

data Fixity = Fixity
  { fixityAssoc   :: Assoc
  , fixityLevel   :: Int
  , fixityTarget  :: Name   -- addPrice: the function it calls
  -- True for the operators of the Prelude that build their own tree
  -- (+ is BinOp Add, |> is App): see builtinOperator in Lam.Sugar.
  -- Prelude の演算子のうち、自分の木を作るもの（+ は BinOp Add、
  -- |> は App）なら True。Lam.Sugar の builtinOperator を参照
  , fixityBuiltin :: Bool
  }
  deriving (Eq, Show)

-- 📋 The operators a file can use: each symbol, and how it is read.
-- ファイルが使える演算子：記号ごとに、その読み方
type Operators = Map Text Fixity
-- ANCHOR_END: fixity
-- ANCHOR_END: decl

-- ANCHOR: program
-- 📜 A whole file: the module line (if any), the imports, the
--    declarations, and the expression at the end (if any).
-- ファイルの全体：module の行（あれば）、import、宣言、最後の式（あれば）
data Program = Program
  { programHeader  :: Maybe Header
  , programImports :: [Import]
  , programDecls   :: [Decl]
  , programFinal   :: Maybe Expr
  }
  deriving (Eq, Show)

-- 📍 Where a line was written: the file, the line and the column.
-- 書いた場所：ファイル、行、列
data Loc = Loc FilePath Int Int
  deriving (Eq, Show)

-- 📍 file.lam:3:1
renderLoc :: Loc -> Text
renderLoc (Loc file l c) =
  T.pack file <> ":" <> T.pack (show l) <> ":" <> T.pack (show c)
-- ANCHOR_END: program

-- ANCHOR: header
-- 🪧 module Shapes exposing (Shape (..), area)
--    Where it was written, the name, and what other files may use.
-- 書いた場所、名前、ほかのファイルが使ってよいもの
data Header = Header Loc Name Exposing
  deriving (Eq, Show)

-- 📥 import Shapes as S (area)
-- import の1行
data Import = Import
  { importLoc      :: Loc
  , importName     :: Name            -- Shapes
  , importAlias    :: Maybe Name      -- as S
  , importList     :: Maybe Exposing  -- (area)
  }
  deriving (Eq, Show)

-- 👀 What a module shows (or what an import also takes without the
--    module's name): (..) for everything, or a list.
-- モジュールが見せるもの（import なら、モジュールの名前を付けずにも
-- 使うもの）。(..) で全部か、一覧
data Exposing
  = ExposeAll                 -- (..)
  | ExposeSome [Exposed]      -- (area, Shape (..))
  deriving (Eq, Show)

-- 🏷️ One item of the list: a value, or a type (or a class) with some
--    of its constructors (or its methods).
-- 一覧の1つ：値か、型（かクラス）と、その構成子（かメソッド）のいくつか
data Exposed
  = ExposeValue Name          -- area
  | ExposeType Name Shown     -- Shape, Shape (..), Json (JInt, JStr)
  -- ANCHOR: exposeOperator
  | ExposeOperator Text       -- (<+>)
  -- ANCHOR_END: exposeOperator
  deriving (Eq, Show)

-- 🧱 Which constructors of a type (or methods of a class) are shown.
-- 型の構成子（クラスのメソッド）のうち、どれを見せるか
data Shown
  = NoConstructors            -- Shape          (the type only)
  | AllConstructors           -- Shape (..)
  | TheseConstructors [Name]  -- Json (JInt, JStr)
  deriving (Eq, Show)

-- ✂️ The name without its module:  Json.JInt  is  JInt.
-- モジュールの名前を取った名前
baseName :: Name -> Name
baseName = snd . T.breakOnEnd "."
-- ANCHOR_END: header

-- ANCHOR: freeVars
-- 🔓 The variables used but not defined inside the expression.
--    \x -> x + y  uses y from outside.
-- 式の中で使われているのに、中で決められていない変数。
-- \x -> x + y は、外の y を使っている
freeVars :: Expr -> Set Name
freeVars expr = case expr of
  Lit _           -> Set.empty
  Var x           -> Set.singleton x
  Lam x body      -> Set.delete x (freeVars body)
  App f a         -> freeVars f <> freeVars a
  -- let is recursive: x is defined in both e and body.
  -- let は再帰する：x は e の中でも body の中でも決まっている
  Let x e body    -> Set.delete x (freeVars e <> freeVars body)
  If c a b        -> freeVars c <> freeVars a <> freeVars b
  BinOp _ l r     -> freeVars l <> freeVars r
  Tuple es        -> foldMap freeVars es
  List es         -> foldMap freeVars es
  -- The names in a pattern are defined in its branch.
  -- パターンの名前は、その枝の中で決まっている
  Case e alts     -> freeVars e <> foldMap
    (\(p, body) -> freeVars body `Set.difference` patternVars p) alts
  -- ANCHOR: freeRecords
  Record fs       -> foldMap (freeVars . snd) fs
  Field e _       -> freeVars e
  Update e fs     -> freeVars e <> foldMap (freeVars . snd) fs
  -- ANCHOR_END: freeRecords

-- 🧩 The names a pattern gives:  (x, Just y)  gives x and y.
-- パターンが付ける名前
patternVars :: Pattern -> Set Name
patternVars p = case p of
  PVar x      -> Set.singleton x
  PWild       -> Set.empty
  PLit _      -> Set.empty
  PCon _ ps   -> foldMap patternVars ps
  PTuple ps   -> foldMap patternVars ps
  PList ps    -> foldMap patternVars ps
  PCons h t   -> patternVars h <> patternVars t
  -- ANCHOR: patternVarsRecord
  PRecord xs  -> Set.fromList xs
  -- ANCHOR_END: patternVarsRecord
-- ANCHOR_END: freeVars
-- ANCHOR_END: all
