-- ANCHOR: all
-- | Trying to repeat ^ (PCRE2's error 109). The tree has no place for it.
-- ^ をくり返そうとする（PCRE2 のエラー 109）。木には、それを入れる場所がない
module RepeatAnchor where

import MiniRegex.Syntax

-- ^*
startStar :: Piece
startStar = Quantified LineStart star
-- ANCHOR_END: all
