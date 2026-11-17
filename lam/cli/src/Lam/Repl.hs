-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The heart of the REPL: one line in, a new session and a text out.
-- REPL の心臓：1行を受け取り、新しいセッションと文字を返す。
module Lam.Repl
  ( Input (..)
  , parseInput
  , step
  , endingText
  , helpText
  ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

import Lam.Io (Handler)
import Lam.Parser
import Lam.Program
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: input
-- ⌨️ What the user typed.
-- ユーザーが打ったもの
data Input
  = Define Name Expr     -- let double x = x * 2
  | DefineType DataDecl  -- data Shape = Circle Int | Rect Int Int
  -- ANCHOR: replRecords
  | DefineAlias AliasDecl  -- type alias User = { name : String }
  -- add : Int -> Int -> Int; add x y = x + y   (; joins the two lines)
  -- （; で2行をつなぐ）
  | DefineTyped Loc Name Signature Expr
  -- ANCHOR_END: replRecords
  | Evaluate Expr        -- double 21
  | ShowType Expr        -- :type double
  | ShowNames            -- :env
  | Load FilePath        -- :load examples/fib.lam
  -- ANCHOR: importInput
  | ImportLine Import    -- import Json as J
  | Browse Name          -- :browse Json
  -- ANCHOR_END: importInput
  | Help                 -- :help
  deriving (Eq, Show)

-- 📖 A definition, a command, or an expression.
-- 定義、コマンド、または式
parseInput :: Text -> Either Text Input
parseInput line =
  case runLamParser (spaces *> input <* eof) "<repl>" line of
    Left err -> Left (T.pack (errorBundlePretty err))
    -- One line is right in front of you: the errors need no places.
    -- 1行は目の前にあるので、エラーに場所は要らない
    Right i  -> Right (withoutPlaces i)
  where
    input = choice
      [ ShowNames <$ symbol ":env"
      , Help <$ symbol ":help"
      , ShowType <$> (symbol ":type" *> expr)
      , Load . T.unpack . T.strip <$> (symbol ":load" *> takeRest)
      , Browse <$> (symbol ":browse" *> conName)
      , ImportLine <$> importLine
      , DefineType <$> dataDecl
      , DefineAlias <$> aliasDecl
      , try typed
      , try (uncurry Define <$> topDefinition <* eof)
      , Evaluate <$> expr ]
    typed = do
      sig <- signatureHead <* symbol ";"
      (x, e) <- topDefinition
      case sig of
        DefSig loc y s | x == y -> pure (DefineTyped loc x s e)
        -- 「注釈と定義の名前がちがいます」
        _ -> fail "the annotation and the definition have different names"

-- 🧽 The same input, without the marks of places.
-- 同じ入力から、場所の印を取る
withoutPlaces :: Input -> Input
withoutPlaces i = case i of
  Define x e -> Define x (stripLocations e)
  DefineTyped l x s e -> DefineTyped l x s (stripLocations e)
  Evaluate e -> Evaluate (stripLocations e)
  ShowType e -> ShowType (stripLocations e)
  other      -> other
-- ANCHOR_END: input

-- ANCHOR: step
-- 🔁 One turn of the REPL. (:load is done by Main, which reads files.)
--    An IO expression is done by the handler, so step runs in its monad.
-- REPL の1回分（:load は、ファイルを読む Main がする）。
-- IO の式はハンドラーがやるので、step はそのモナドの中で動く
step :: Monad m => Handler m -> Session -> Text -> m (Session, Text)
step handle session line =
  case parseInput line of
    Left err -> pure (session, err)
    Right Help -> pure (session, helpText)
    -- ANCHOR: showNames
    -- The names a program can write: not $regex, which only
    -- re"..." opens into.
    -- プログラムが書ける名前だけ。re"..." が開く $regex は見せない
    Right ShowNames ->
      pure ( session
           , T.unwords (filter (not . T.isPrefixOf "$")
                               (Map.keys (sessionValues session))) )
    -- ANCHOR_END: showNames
    Right (ShowType e) -> pure $
      case inferType (sessionData session) (sessionTypes session) e of
        Left err -> (session, uncurry (<>) (placedType (explain session err)))
        Right t  -> (session, renderScheme t)
    Right (Evaluate e) -> do
      out <- evaluate handle session e
      pure (session, endingText out)
    Right (Define x e) -> pure $ case define session x e of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    Right (DefineType d) -> pure $ case defineData session d of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    -- ANCHOR: stepRecords
    Right (DefineAlias a) -> pure $ case defineAlias session a of
      Left err           -> (session, err)
      Right (s', shown)  -> (s', shown)
    Right (DefineTyped loc x sig e) ->
      pure $ case defineTyped session loc x sig e of
        Left err           -> (session, err)
        Right (s', shown)  -> (s', shown)
    -- ANCHOR_END: stepRecords
    Right (Load file) -> pure
      (session, "(:load " <> T.pack file <> " is handled by the REPL)")
    Right (ImportLine _) -> pure
      (session, "(import is handled by the REPL)")
    -- ANCHOR: browse
    -- What a module shows: its names and types, and what it hides.
    -- モジュールが見せるもの：名前と型、それに隠しているもの
    Right (Browse m) -> pure $ case Map.lookup m (sessionModules session) of
      Nothing  -> (session, "no module " <> m <> " (import it first)")
      Just md  -> (session, T.intercalate "\n" (browse md))
    -- ANCHOR_END: browse

-- ANCHOR: browseLines
-- 📖 The lines of :browse:  m.x : type, and the hidden constructors.
-- :browse の行
browse :: Module -> [Text]
browse md =
  [ moduleName md <> "." <> x <> " : " <> renderScheme sc
  | (x, (sc, _)) <- Map.toList (moduleExports md) ]
  <> [ "(hidden: " <> T.intercalate ", " hidden <> ")"
     | let hidden = concatMap snd (Map.elems (moduleShown md))
     , not (null hidden) ]
-- ANCHOR_END: browseLines

-- 🏁 What the REPL shows for an ending. exit does not end the REPL:
--    :quit does.
-- REPL が見せるもの。exit では REPL は終わらない（終わるのは :quit）
endingText :: Ending -> Text
endingText ending = case ending of
  Finished t   -> t
  Errored t    -> t
  -- 「（exit 2。REPL は続きます）」
  ExitedWith n -> "(exit " <> T.pack (show n) <> "; the REPL goes on)"

-- 📖 :help
helpText :: Text
helpText = T.intercalate "\n"
  [ "name args = expr       define a name (it can call itself; let is ok)"
  , "name : type; name ...  define it with a type annotation"
  , "type alias T = type    give a type a new name"
  , "data T a = C a | D      define a data type"
  , "expr                   compute an expression"
  , "{\"key\": expr, ...}     a map (the keys are strings)"
  , "do x <- e; f x         IO, line by line (an IO expression is done)"
  , "f\"x = {x:>5}\"          a string with values in it (< > ^ 0 , =)"
  , "re\"\\d+\"                a regular expression (Regex.find, ...)"
  , "Parser.do x <- p; ...  a parser, line by line (Parser.run p text)"
  , "x |> f                 f x  (text |> words |> length)"
  , ":type expr             show the type of an expression"
  , ":load file.lam         load the definitions in a file"
  , "import M as N (x)      use a module (M.lam, or a standard one)"
  , ":browse M              list what the module M shows"
  , ":env                   list the names you can use"
  , ":quit                  quit" ]
-- ANCHOR_END: step
-- ANCHOR_END: all
