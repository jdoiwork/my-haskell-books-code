-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE TemplateHaskellQuotes #-}
-- | Quasi-quoters: [re|\d+|] is a Regex, read when the program is
--   built. A wrong pattern stops the build, with the same ^ as
--   explain. [typed|…|] also writes the named groups into the type.
-- 準クォート：[re|\d+|] は Regex で、プログラムをビルドするときに読む。
-- まちがったパターンは、explain と同じ ^ を付けて、ビルドを止める。
-- [typed|…|] は、名前付きのグループを型にも書く
module MiniRegex.Quote
  ( re
  , typed
  , trusted
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Haskell.TH (Exp, Q, Type)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import MiniRegex (Regex, compile, explain)
import MiniRegex.Groups (alwaysGroups, groupNames)
import MiniRegex.Types (tree)
import MiniRegex.Typed (Part (..), TypedRegex, unsafeTyped)
-- ANCHOR_END: imports

-- ANCHOR: re
-- 🧪 [re|…|]: a Regex, checked at compile time.
-- [re|…|]：コンパイルのときに確かめた Regex
re :: QuasiQuoter
re = QuasiQuoter
  { quoteExp  = quoteRegex
  , quotePat  = onlyExpressions
  , quoteType = onlyExpressions
  , quoteDec  = onlyExpressions
  }

-- 🏗️ Compile the pattern now, at build time. If it is wrong, fail
--    with the explanation; GHC shows it as a compile error. If it is
--    right, write code that compiles it again when the program runs.
-- いま、ビルドのときにパターンを読む。まちがっていたら、説明を付けて
-- 失敗する（GHC がコンパイルエラーとして見せる）。正しければ、
-- プログラムが動くときにもう一度読むコードを書く
quoteRegex :: String -> Q Exp
quoteRegex s = do
  _ <- checked s
  [| trusted s |]

-- 🔬 Read the pattern, or stop the build.
-- パターンを読む。読めなければ、ビルドを止める
checked :: String -> Q Regex
checked s = case compile (T.pack s) of
  Right r  -> pure r
  Left err -> fail ("bad pattern in [re|…|]:\n"
                    <> T.unpack (explain (T.pack s) err))

-- 🤝 At run time: the pattern was read at build time, so it is right.
-- 動くとき：ビルドのときに読めたパターンなので、正しい
trusted :: String -> Regex
trusted s = case compile (T.pack s) of
  Right r  -> r
  Left err -> error (T.unpack (explain (T.pack s) err))

-- 🚧 A regex can only be an expression.
-- 正規表現は、式の場所にしか書けない
onlyExpressions :: String -> Q a
onlyExpressions _ =
  fail "[re|…|] and [typed|…|] can only be used as expressions"
-- ANCHOR_END: re

-- ANCHOR: typed
-- 🏷️ [typed|…|]: the same, as a TypedRegex with the named groups in
--    its type.
-- [typed|…|]：同じものを、名前付きのグループを型に書いた TypedRegex で
typed :: QuasiQuoter
typed = re { quoteExp = quoteTyped }

quoteTyped :: String -> Q Exp
quoteTyped s = do
  r <- checked s
  let t      = tree r
      always = alwaysGroups t
      names  = groupNames t
      parts  = [ (name, partOf name n) | (name, n) <- names ]
      -- a name given to two groups ((?| ... ) and the like): Sometimes
      -- 2つのグループに付いた名前（(?| ... ) など）は Sometimes
      partOf name n
        | length (filter ((== name) . fst) names) == 1
        , n `Set.member` always = 'Always
        | otherwise             = 'Sometimes
  [| unsafeTyped (trusted s) :: TypedRegex $(pure (groupsType parts)) |]

-- 📇 [("year", 'Always)] as the type '[ '("year", 'Always) ].
-- [("year", 'Always)] を、型 '[ '("year", 'Always) ] に
groupsType :: [(T.Text, TH.Name)] -> Type
groupsType = foldr cons TH.PromotedNilT
  where
    cons (name, part) rest =
      TH.PromotedConsT `TH.AppT` pair name part `TH.AppT` rest
    pair name part =
      TH.PromotedTupleT 2
        `TH.AppT` TH.LitT (TH.StrTyLit (T.unpack name))
        `TH.AppT` TH.PromotedT part
-- ANCHOR_END: typed
-- ANCHOR_END: all
