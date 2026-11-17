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
  | Evaluate Expr        -- double 21
  | ShowType Expr        -- :type double
  | ShowNames            -- :env
  | Load FilePath        -- :load examples/fib.lam
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
      , DefineType <$> dataDecl
      , try (uncurry Define <$> definition <* eof)
      , Evaluate <$> expr ]

-- 🧽 The same input, without the marks of places.
-- 同じ入力から、場所の印を取る
withoutPlaces :: Input -> Input
withoutPlaces i = case i of
  Define x e -> Define x (stripLocations e)
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
        Left err -> (session, uncurry (<>) (placedType err))
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
    Right (Load file) -> pure
      (session, "(:load " <> T.pack file <> " is handled by the REPL)")

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
  [ "let name args = expr   define a name (it can call itself)"
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
  , ":env                   list the names you can use"
  , ":quit                  quit" ]
-- ANCHOR_END: step
-- ANCHOR_END: all
