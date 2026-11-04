-- ANCHOR: all
-- | Step 2 of the WebAssembly chapter: the element names, from Haskell.
-- WebAssembly の章のステップ2：属性の名前を、Haskell から
module Names where

import GHC.Wasm.Prim (JSString (..), toJSString)

foreign export javascript "elementNames sync"
  elementNames :: JSString

elementNames :: JSString
elementNames = toJSString "fire,water,grass"
-- ANCHOR_END: all
