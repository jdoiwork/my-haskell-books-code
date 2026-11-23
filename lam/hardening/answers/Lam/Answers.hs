{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The answers of the training of "Making the language sturdier".
-- 「言葉を補強する」のトレーニングの解答。
module Lam.Answers
  ( shadowWarnings
  , groupNames
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)

import Lam.Module (readProgram)
import Lam.Prelude (preludeSession)
import Lam.Program
import Lam.Syntax

-- ANCHOR: shadowWarnings
-- ⚠️ Step 2: a warning for a local name that hides a name defined at
--    the top (like -Wname-shadowing of GHC). own is the names at the
--    top. The hidden names of lam ($try, $x, ...) are not the user's.
-- ステップ2：いちばん外で定義した名前を隠す、ローカルな名前への警告
-- （GHC の -Wname-shadowing のように）。own は、いちばん外の名前。lam の
-- 隠した名前（$try、$x など）は、ユーザーのものではない
shadowWarnings :: Set Name -> Expr -> [Text]
shadowWarnings own = go Nothing
  where
    go place e = case e of
      At loc inner -> go (Just loc) inner
      Lam x body   -> hides place [x] <> go place body
      Let x a body -> hides place [x] <> go place a <> go place body
      Case x alts  -> go place x <> concat
        [ hides place (Set.toList (patternVars p)) <> go place b
        | (p, b) <- alts ]
      _            -> concatMap (go place) (children e)
    hides place xs =
      [ maybe "" (\loc -> renderLoc loc <> ": ") place
          <> "warning: the local " <> x <> " hides the definition " <> x
          <> " at the top"
      | x <- xs, x `Set.member` own ]
-- ANCHOR_END: shadowWarnings

-- ANCHOR: groupNames
-- 🔗 Step 3: the groups of a file, in the order lam checks them.
-- ステップ3：ファイルのグループを、lam が確かめる順に
groupNames :: Text -> Either Text [[Name]]
groupNames source = do
  prog <- readProgram False (sessionOperators preludeSession)
            "<groups>" source
  defs <- pairSignatures (programDecls prog)
  pure (map (map defName) (groupsOf defs))
-- ANCHOR_END: groupNames
-- ANCHOR_END: all
