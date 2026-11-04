-- ANCHOR: all
-- | The element chart of the language map chapter, compiled to
--   WebAssembly and called from JavaScript.
-- 言語の地図の章の相性表を WebAssembly にして、JavaScript から呼ぶ
module Matchup where

import GHC.Wasm.Prim (JSString (..), fromJSString, toJSString)

import Modern.Elements (Element (..), power)

-- ANCHOR: export
-- 📤 A function JavaScript can call: two element names in,
--    the attack power out.
-- JavaScript から呼べる関数：属性の名前を2つ受け取り、攻撃の倍率を返す
foreign export javascript "matchup sync"
  matchup :: JSString -> JSString -> JSString

matchup :: JSString -> JSString -> JSString
matchup attacker defender =
  case (element (fromJSString attacker), element (fromJSString defender)) of
    (Just a, Just d) -> toJSString (show (power a d) <> "%")
    _                -> toJSString "unknown element"

element :: String -> Maybe Element
element "fire"  = Just Fire
element "water" = Just Water
element "grass" = Just Grass
element _       = Nothing
-- ANCHOR_END: export

-- ANCHOR: import
-- 📥 A JavaScript function Haskell can call.
-- Haskell から呼べる JavaScript の関数
foreign import javascript unsafe "console.log($1)"
  consoleLog :: JSString -> IO ()

foreign export javascript "hello"
  hello :: IO ()

hello :: IO ()
hello = consoleLog (toJSString "Haskell says hello from WebAssembly!")
-- ANCHOR_END: import
-- ANCHOR_END: all
