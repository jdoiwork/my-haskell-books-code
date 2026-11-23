{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | What every language Lam turns into needs: a Backend says what the
--   target has and how to write it, and the parts here (a call of a
--   function to itself, last; a record that needs itself at once; the
--   constructors a tree makes) do not depend on the language.
-- Lam を変換するどの言語にも要るもの：Backend は、変換先が何を持ち、どう
-- 書くかを言う。ここの部品（最後に自分を呼ぶ呼び出し、すぐに自分が要る
-- レコード、木が作る構成子）は、言語によらない。
module Lam.Backend
  ( Backend (..)
  , translate
  , tailCalls
  , isSelfCall
  , unApply
  , usesAtOnce
  , consIn
  , freeLocals
  ) where

import Data.List (nub)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Cli (prepareCli)
import Lam.Core
import Lam.Program (Session (..), checkMain)
import Lam.Syntax

-- ANCHOR: backend
-- 🎒 One language to turn Lam into. To add a language, make one of
--    these: what its runtime has (target), and how to write a lowered
--    program as one file of it (write). The rest is the same for all.
-- Lam を変換する言語1つ。言語を足すには、これを1つ作る：ランタイムが
-- 持っているもの（target）と、小さな木にしたプログラムを、その言語の
-- ファイル1つに書く方法（write）。ほかは、どの言語も同じ
data Backend = Backend
  { backendName    :: Text      -- JavaScript
  , backendCommand :: Text      -- to-js (lam to-js FILE.lam)
  , backendSuffix  :: Text      -- .mjs (the file after -o)
  , backendTarget  :: Target
  , backendWrite   :: FilePath -> Lowered -> Text
  }
-- ANCHOR_END: backend

-- ANCHOR: translate
-- 🚀 The program in a session as one file of the backend's language.
--    Left: why it cannot be done (no main, or names the runtime does
--    not have).
-- セッションのプログラムを、Backend の言語のファイル1つに。Left は、
-- できない理由（main がない、ランタイムにない名前がある）
translate :: Backend -> FilePath -> Session -> Either Text Text
translate backend file session
  | not (Map.member "main" (sessionTypes session)) =
      -- 「lam to-js には main が要ります」
      Left (T.pack file <> ": lam " <> backendCommand backend
            <> " needs a main")
  | otherwise = do
      ready <- prepareCli session
      start <- checkMain ready
      lowered <- either lacking Right
                   (lower (backendTarget backend) (sessionCode ready) start)
      Right (backendWrite backend file lowered)
  where
    -- 「この名前は、まだ JavaScript にありません」
    lacking names =
      Left (T.pack file <> ": these have no " <> backendName backend
            <> " yet: " <> T.intercalate ", " (nub (map written names)))
    -- A hidden name, as the program wrote it.
    -- 隠した名前を、プログラムが書いた形で
    written x = case x of
      "$regex"    -> "re\"...\""
      "$group"    -> "re\"...\""
      "$main.cli" -> "main with options (cli)"
      _           -> x
-- ANCHOR_END: translate

-- ANCHOR: loop
-- 🔎 Does the body call self with all n arguments, as the last thing?
--    The last things are the branches of if and case, the body of let,
--    and the right side of && and ||.
-- 本体は、最後のこととして、n 個の引数全部で self を呼ぶか。最後のこと
-- は、if と case の枝、let の本体、&& と || の右
tailCalls :: Core -> Int -> Core -> Bool
tailCalls self n core = case core of
  CLet x _ b    -> not (hides x) && tailCalls self n b
  CIf _ a b     -> tailCalls self n a || tailCalls self n b
  CCase _ alts  -> or [ tailCalls self n b | (p, b) <- alts
                      , not (any hides (Set.toList (patternVars p))) ]
  COp And _ r   -> tailCalls self n r
  COp Or _ r    -> tailCalls self n r
  _             -> isSelfCall self n core
  where
    hides x = self == CLocal x

isSelfCall :: Core -> Int -> Core -> Bool
isSelfCall self n core = n > 0 && spine core == Just (self, n)
  where
    spine c = case c of
      CApp f _ -> fmap (fmap (+ 1)) (spine f)
      _ | c == self -> Just (c, 0)
        | otherwise -> Nothing

-- 📞 The function and the arguments of a call: f a b is (f, [a, b]).
-- 呼び出しの関数と引数
unApply :: Core -> (Core, [Core])
unApply core = case core of
  CApp f a -> let (g, as) = unApply f in (g, as <> [a])
  _        -> (core, [])
-- ANCHOR_END: loop

-- ANCHOR: usesAtOnce
-- 🔎 Does the tree use the local x at once (not only inside a function)?
-- 木は、ローカル x を、すぐに使うか（関数の中だけでなく）
usesAtOnce :: Name -> Core -> Bool
usesAtOnce x core = case core of
  CLocal y      -> x == y
  CLam _ _      -> False
  CApp f a      -> usesAtOnce x f || usesAtOnce x a
  CLet y e b    -> x /= y && (usesAtOnce x e || usesAtOnce x b)
  CIf c a b     -> any (usesAtOnce x) [c, a, b]
  COp _ l r     -> usesAtOnce x l || usesAtOnce x r
  CTuple es     -> any (usesAtOnce x) es
  CList es      -> any (usesAtOnce x) es
  CCase e alts  -> usesAtOnce x e || any (usesAtOnce x . snd) alts
  CRecord fs    -> any (usesAtOnce x . snd) fs
  CField e _    -> usesAtOnce x e
  CUpdate e fs  -> usesAtOnce x e || any (usesAtOnce x . snd) fs
  _             -> False
-- ANCHOR_END: usesAtOnce

-- ANCHOR: consIn
-- 🧱 The constructors a tree makes (as functions, or values).
-- 木が作る構成子（関数か、値として）
consIn :: Core -> [(Name, Int, Int)]
consIn core = case core of
  CCon c i n    -> [(c, i, n)]
  CLam _ b      -> consIn b
  CApp f a      -> consIn f <> consIn a
  CLet _ e b    -> consIn e <> consIn b
  CIf c a b     -> consIn c <> consIn a <> consIn b
  COp _ l r     -> consIn l <> consIn r
  CTuple es     -> concatMap consIn es
  CList es      -> concatMap consIn es
  CCase e alts  -> consIn e <> concatMap (consIn . snd) alts
  CRecord fs    -> concatMap (consIn . snd) fs
  CField e _    -> consIn e
  CUpdate e fs  -> consIn e <> concatMap (consIn . snd) fs
  _             -> []
-- ANCHOR_END: consIn

-- ANCHOR: freeLocals
-- 🪢 The locals a tree uses but does not bind itself: the ones a
--    function made from it holds on to.
-- 木が使うが、自分では束ねないローカル。木から作った関数が、つかまえる
-- もの
freeLocals :: Core -> Set Name
freeLocals core = case core of
  CLocal x      -> Set.singleton x
  CLam x b      -> Set.delete x (freeLocals b)
  CApp f a      -> freeLocals f <> freeLocals a
  CLet x e b    -> Set.delete x (freeLocals e <> freeLocals b)
  CIf c a b     -> freeLocals c <> freeLocals a <> freeLocals b
  COp _ l r     -> freeLocals l <> freeLocals r
  CTuple es     -> foldMap freeLocals es
  CList es      -> foldMap freeLocals es
  CCase e alts  -> freeLocals e <> foldMap
                     (\(p, b) -> freeLocals b `Set.difference` patternVars p)
                     alts
  CRecord fs    -> foldMap (freeLocals . snd) fs
  CField e _    -> freeLocals e
  CUpdate e fs  -> freeLocals e <> foldMap (freeLocals . snd) fs
  _             -> Set.empty
-- ANCHOR_END: freeLocals
-- ANCHOR_END: all
