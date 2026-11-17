{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | The builtins for parsers: small parsers, and ways to put them
--   together. The inside is megaparsec, the same tool that reads Lam
--   itself. The names start with Parser., like Regex.
-- パーサーの組み込みの関数：小さなパーサーと、それを組み合わせる道具。
-- 中身は、Lam そのものを読んでいるのと同じ megaparsec。
-- 名前は、Regex. と同じく、Parser. で始める。
module Lam.Parsing
  ( parsingBuiltins
  , tParser
  , runLamParserValue
  ) where

import Control.Monad.State.Strict (StateT, get, modify, put, runStateT)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (match)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Lam.Builtins
import Lam.Eval
import Lam.Pcre
import Lam.Regex (matchesOf, tMatch, tRegex)
import Lam.Syntax (Name)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: types
-- 🏷️ Parser a: a parser that reads a string and gives an a.
--    Like IO a, it is a value: making it reads nothing yet.
-- Parser a：文字列を読んで a をくれるパーサー。IO a と同じく値で、
-- 作っただけでは、まだ何も読まない
tParser :: Type -> Type
tParser a = TCon "Parser" [a]

-- The same stack as LamParser, for parsers that give other things.
-- LamParser と同じ重なりで、ほかのものをくれるパーサー
type P = ParsecT Void Text (StateT Farthest (Either EvalError))
-- ANCHOR_END: types

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
parsingBuiltins :: [Builtin]
parsingBuiltins =
  -- ANCHOR: small
  -- The smallest parsers: read this text, one letter, a number.
  -- いちばん小さなパーサー：この文字を読む、1文字読む、数を読む
  [ prim1 "Parser.string" (tString ~> tParser tString) $ \v -> do
      s <- string v
      Right (VParser (VString <$> C.string s))
  , prim1 "Parser.satisfy" ((tString ~> TBool) ~> tParser tString) $ \f ->
      Right (VParser (satisfyWith f))
  , Builtin "Parser.int" (closeOver (tParser TInt)) $ VParser
      (VInt <$> L.signed (pure ()) L.decimal <?> "a number")
  , Builtin "Parser.spaces" (closeOver (tParser tUnit)) $ VParser
      (unit <$ C.space)
  , Builtin "Parser.end" (closeOver (tParser tUnit)) $ VParser
      (unit <$ eof)
  -- ANCHOR_END: small

  -- ANCHOR: chain
  -- Give a value without reading, or stop with a message.
  -- 何も読まずに値を返す。または、文を添えて失敗する
  , prim1 "Parser.pure" (a ~> tParser a) $ \v ->
      Right (VParser (pure v))
  , prim1 "Parser.fail" (tString ~> tParser a) $ \v -> do
      msg <- string v
      Right (VParser (fail (T.unpack msg)))

  -- Change what a parser gives, with a Lam function.
  -- パーサーがくれる値を、Lam の関数で変える
  , prim2 "Parser.map" ((a ~> b) ~> tParser a ~> tParser b) $ \f p ->
      Right $ VParser $ do
        x <- parserOf p
        call f x

  -- Read with p, then let f choose the next parser from what p gave.
  -- Parser.bind takes the parser first (like >>= of Haskell), and
  -- Parser.andThen takes the function first (like Maybe.andThen).
  -- p で読み、p がくれた値から、次のパーサーを f に選ばせる。
  -- Parser.bind はパーサーが先（Haskell の >>= と同じ）、
  -- Parser.andThen は関数が先（Maybe.andThen と同じ）
  , prim2 "Parser.bind"
      (tParser a ~> (a ~> tParser b) ~> tParser b) $ \p f ->
      Right (VParser (chain p f))
  , prim2 "Parser.andThen"
      ((a ~> tParser b) ~> tParser a ~> tParser b) $ \f p ->
      Right (VParser (chain p f))
  -- ANCHOR_END: chain

  -- ANCHOR: choose
  -- Try the parsers from the left; the first that fits wins. Each one
  -- starts from the same place, even if the one before it read some
  -- letters before it failed.
  -- 左から試し、最初に合ったものを使う。前のパーサーが何文字か読んで
  -- から失敗しても、どれも同じ場所から読みはじめる
  , prim1 "Parser.oneOf" (tList (tParser a) ~> tParser a) $ \v -> do
      ps <- list v
      Right (VParser (firstFit (map parserOf ps)))
  -- ANCHOR_END: choose

  -- ANCHOR: repeat
  -- Read with p again and again: 0 times or more, 1 time or more.
  -- p で何度も読む：0回以上、1回以上
  , prim1 "Parser.many" (tParser a ~> tParser (tList a)) $ \p ->
      Right (VParser (VList <$> manyOf "Parser.many" (parserOf p)))
  , prim1 "Parser.some" (tParser a ~> tParser (tList a)) $ \p ->
      Right $ VParser $ do
        x  <- parserOf p
        xs <- manyOf "Parser.some" (parserOf p)
        pure (VList (x : xs))
  -- Items with something between them:  1, 2, 3
  -- 間に区切りのある並び
  , prim2 "Parser.sepBy" (tParser a ~> tParser b ~> tParser (tList a)) $
      \p sep -> Right $ VParser $ do
        first <- attempt (parserOf p)
        case first of
          Nothing -> pure (VList [])
          Just x  -> do
            xs <- manyOf "Parser.sepBy" (parserOf sep *> parserOf p)
            pure (VList (x : xs))
  , prim1 "Parser.optional" (tParser a ~> tParser (tMaybe a)) $ \p ->
      Right $ VParser $ do
        found <- attempt (parserOf p)
        pure (maybe (con "Nothing" []) (\x -> con "Just" [x]) found)
  -- ANCHOR_END: repeat

  -- ANCHOR: lazy
  -- A parser that points at itself:
  --    let expr = ... Parser.lazy (\_ -> expr) ...
  -- The function is called the first time the parser reads, so that
  -- expr is already made by then. Laziness keeps what it gave.
  -- 自分を指すパーサー。関数は、はじめて読むときに呼ぶので、そのときには
  -- expr はもうできている。一度作ったパーサーは、遅延評価がとっておく
  , prim1 "Parser.lazy" ((tUnit ~> tParser a) ~> tParser a) $ \f ->
      let inner = either stop id (apply f unit >>= parserOfE)
      in  Right (VParser inner)
  -- ANCHOR_END: lazy

  -- ANCHOR: label
  -- Give the parser a name for the errors: "expecting a key".
  -- エラーのための名前を付ける
  , prim2 "Parser.label" (tString ~> tParser a ~> tParser a) $ \n p -> do
      name <- string n
      Right (VParser (labelled name (parserOf p)))
  -- ANCHOR_END: label

  -- ANCHOR: regex
  -- A regex as one part of a parser: it must fit right HERE.
  -- 正規表現を、パーサーの部品にする。いま読んでいる「ここ」に合うこと
  , prim1 "Parser.regex" (tRegex ~> tParser tMatch) $ \v -> case v of
      VRegex r -> case compileRegex ("\\A(?:" <> regexSource r <> ")") of
        Right here -> Right (VParser (regexHere r here))
        Left msg   -> Left (BuiltinFailed "Parser.regex" msg)
      _ -> Left (BuiltinFailed "Parser.regex" "got a value that is not Regex")
  -- ANCHOR_END: regex

  -- ANCHOR: run
  -- Read the whole string. An Err tells where it stopped, and why.
  -- 文字列の全体を読む。Err は、どこで、なぜ止まったかを教える
  , prim2 "Parser.run" (tParser a ~> tString ~> tResult tString a) $
      \p v -> do
        s <- string v
        answer <- runLamParserValue p s
        Right $ case answer of
          Right x  -> con "Ok" [x]
          Left msg -> con "Err" [VString msg]
  -- ANCHOR_END: run
  ]
  where
    a = TVar 0
    b = TVar 1
    unit = VTuple []
    tMaybe t = TCon "Maybe" [t]
    tResult e x = TCon "Result" [e, x]
-- ANCHOR_END: table

-- ANCHOR: parserOf
-- 🔍 The parser inside a Parser value. It does not look at the value
--    until the parser runs, so that Parser.oneOf [.., expr] can be
--    made before expr is.
-- Parser の値の中のパーサー。パーサーが動くまで値を見ないので、
-- expr ができる前に Parser.oneOf [.., expr] を作れる
parserOf :: Value -> LamParser
parserOf v = either stop id (parserOfE v)

parserOfE :: Value -> Either EvalError LamParser
parserOfE v = case v of
  VParser p -> Right p
  _         -> Left (BuiltinFailed "Parser" "got a value that is not one")

-- 📞 Call a Lam function while reading. If it fails, the run stops.
-- 読んでいる途中で Lam の関数を呼ぶ。失敗したら、読むのを止める
call :: Value -> Value -> LamParser
call f x = either stop pure (apply f x)

-- ANCHOR: chainHelper
-- Read with the parser p, then with the parser that f gives.
-- パーサー p で読み、続きを f がくれたパーサーで読む
chain :: Value -> Value -> LamParser
chain p f = do
  x    <- parserOf p
  next <- call f x
  parserOf next
-- ANCHOR_END: chainHelper

-- 🛑 Stop the whole run with an error of the program.
-- プログラムのまちがいで、読むのを丸ごと止める
stop :: EvalError -> P x
stop = lift . lift . Left
-- ANCHOR_END: parserOf

-- ANCHOR: attempt
-- 🎣 Try p. If it fails, go back to where it started, as if it read
--    nothing, and give Nothing. Its error is kept as the farthest one,
--    if it got farther than the others.
-- p を試す。失敗したら、読みはじめた場所に戻り（何も読まなかったことに
-- して）Nothing を返す。エラーは、ほかより遠くまで読めていたら、
-- いちばん遠いエラーとしてとっておく
attempt :: P x -> P (Maybe x)
attempt p = do
  before <- getParserState
  answer <- observing p
  case answer of
    Right x  -> pure (Just x)
    Left err -> do
      setParserState before
      lift (modify (Just . maybe err (<> err)))
      pure Nothing

-- 🥇 The first parser that fits. If none does, fail with all of their
--    errors put together (the one that read the farthest wins; ties
--    are merged into "expecting a or b").
-- 最初に合ったパーサー。どれも合わなければ、全部のエラーを合わせて
-- 失敗する（いちばん遠くまで読めたものが勝ち、同じ場所なら
-- 「a か b」にまとめる）
firstFit :: [P x] -> P x
firstFit = go Nothing
  where
    go errs ps = case ps of
      [] -> maybe empty parseError errs
      p : rest -> do
        before <- getParserState
        answer <- observing p
        case answer of
          Right x  -> pure x
          Left err -> do
            setParserState before
            lift (modify (Just . maybe err (<> err)))
            go (Just (maybe err (<> err) errs)) rest
-- ANCHOR_END: attempt

-- ANCHOR: labelled
-- 🏷️ p, with a name for its errors. Like megaparsec's <?>, the name
--    takes the place of what was expected only where p started. The
--    errors that p's choices kept as the farthest get the name, too.
-- p に、エラーのための名前を付ける。megaparsec の <?> と同じく、
-- 名前が「欲しかったもの」の代わりになるのは、p が読みはじめた場所の
-- エラーだけ。p の中の選択肢がとっておいた、いちばん遠いエラーにも付ける
labelled :: Text -> LamParser -> LamParser
labelled name p = do
  outer <- lift get
  lift (put Nothing)
  start  <- getOffset
  answer <- observing (p <?> T.unpack name)
  inner  <- lift get
  let mine = relabel start <$> inner
  lift (put (maybe mine (\o -> Just (maybe o (o <>) mine)) outer))
  either parseError pure answer
  where
    relabel start err = case err of
      TrivialError at found _
        | at == start ->
            TrivialError at found (Set.singleton (Label (nonEmpty' name)))
      _ -> err
    nonEmpty' t = case T.unpack t of
      c : cs -> c :| cs
      []     -> '?' :| []
-- ANCHOR_END: labelled

-- ANCHOR: satisfy
-- 🔤 One letter that the Lam function says yes to. Look at it first,
--    and take it only if the answer is true, so that a "no" reads
--    nothing, and the error points at the letter itself.
-- Lam の関数が true と言う1文字。まず見るだけにして、true のときだけ
-- 取る。false なら何も読まず、エラーはその文字を指す
satisfyWith :: Value -> LamParser
satisfyWith f = do
  c  <- lookAhead anySingle
  ok <- call f (VString (T.singleton c))
  case ok of
    VBool True  -> VString (T.singleton c) <$ anySingle
    VBool False -> unexpected (Tokens (c :| []))
    _ -> stop (BuiltinFailed "Parser.satisfy" "the test must give a Bool")
-- ANCHOR_END: satisfy

-- ANCHOR: manyOf
-- 🔁 p again and again, until it does not fit. A p that fits without
--    reading anything would fit forever, so that is an error.
--    (megaparsec's own many would just never end.)
-- 合わなくなるまで、p をくり返す。何も読まずに合う p は、いつまでも
-- 合うので、エラーにする（megaparsec の many なら、終わらない）
manyOf :: Name -> LamParser -> P [Value]
manyOf what p = go
  where
    go = do
      before <- getOffset
      found  <- attempt p
      case found of
        Nothing -> pure []
        Just x  -> do
          after <- getOffset
          if after == before
            then stop (BuiltinFailed what
                   "the parser read nothing, so it would go on forever")
            else (x :) <$> go
-- ANCHOR_END: manyOf

-- ANCHOR: regexHere
-- 🎯 Match the regex at this very place. "here" is the same regex with
--    \A in front, so it cannot match further on. Read as many letters
--    as it matched. Match.start counts from the start of the whole
--    string, as in Regex.find.
-- 正規表現を、ちょうどこの場所で合わせる。here は、頭に \A を付けた
-- 同じ正規表現なので、先のほうでは合わない。合った文字の数だけ読む。
-- Match.start は、Regex.find と同じく、文字列全体の頭から数える
regexHere :: Regex -> Regex -> LamParser
regexHere r here = do
  at   <- getOffset
  rest <- getInput
  case regexSearch here rest of
    Left msg -> stop (BuiltinFailed "Parser.regex" msg)
    Right cut -> case matchesOf r cut of
      VCon c i [w@(VString text), VInt _, gs, ns] : _ -> do
        _ <- takeP (Just "a match") (T.length text)
        pure (VCon c i [w, VInt (fromIntegral at), gs, ns])
      -- It does not fit here: say what was here, and what was wanted.
      -- ここには合わない：ここにあったものと、欲しかったものを言う
      _ -> failure (Just (maybe EndOfInput (\(c, _) -> Tokens (c :| []))
                                (T.uncons rest)))
                   (Set.singleton (Label ('r' :| T.unpack shown)))
  where
    shown = "e\"" <> regexSource r <> "\""
-- ANCHOR_END: regexHere

-- ANCHOR: runParser
-- ▶️ Run a Parser value on the whole string. The outer Left is an
--    error of the program (a Lam function failed), and stops the
--    program. An inner Left is a string that does not fit: a value the
--    program can look at. Its message is the error that read the
--    farthest, even if a choice that failed found it.
-- Parser の値を、文字列の全体に使う。外の Left はプログラムのまちがい
-- （Lam の関数が失敗した）で、プログラムを止める。内の Left は、
-- 文字列が合わなかったということで、プログラムが調べられる値。
-- その文は、いちばん遠くまで読めたエラー（失敗した選択肢のものでも）
runLamParserValue :: Value -> Text -> Either EvalError (Either Text Value)
runLamParserValue p s = do
  (answer, farthest) <-
    runStateT (runParserT (parserOf p <* eof) "" s) Nothing
  Right $ case answer of
    Right x     -> Right x
    Left bundle ->
      let err :| _ = bundleErrors bundle
          worst    = maybe err (<> err) farthest
      in  Left (T.stripEnd (T.pack (errorBundlePretty
                 bundle { bundleErrors = worst :| [] })))
-- ANCHOR_END: runParser
-- ANCHOR_END: all
