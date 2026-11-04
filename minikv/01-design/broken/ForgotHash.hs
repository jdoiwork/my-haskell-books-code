-- | This file must NOT compile with -Werror: one kind is forgotten.
-- このファイルは -Werror ではコンパイルできないはず：種類を1つ忘れている
module ForgotHash where

import MiniKv.Types

-- ANCHOR: forgot
kindOfDraft :: Value -> Kind
kindOfDraft (Str _)     = KString
kindOfDraft (List _)    = KList
kindOfDraft (Members _) = KSet
-- ANCHOR_END: forgot
