-- | Everything you need to write [hsx| ... |].
-- [hsx| ... |] を書くのに要るもの全部。
module Hsx
  ( module Hsx.Html
  , hsx
  ) where

import Hsx.Html
import Hsx.QQ (hsx)
