-- ANCHOR: all
-- ANCHOR: imports
-- | A small terminal screen: 40 columns by 8 rows. Long lines wrap,
--   and when the screen is full, the top row scrolls away for good.
-- 小さなターミナルの画面：横 40 文字、縦 8 行。長い行は折り返し、
-- 画面がいっぱいになると、いちばん上の行は流れて消える
module Screen
  ( screenWidth
  , screenHeight
  , Screen (..)
  , blankScreen
  , writeChar
  , writeString
  , rows
  , render
  ) where
-- ANCHOR_END: imports

-- ANCHOR: size
-- 📐 The size of the screen. Change these to try other sizes.
-- 画面の大きさ。ほかの大きさを試すときは、ここを変える
screenWidth, screenHeight :: Int
screenWidth  = 40
screenHeight = 8
-- ANCHOR_END: size

-- ANCHOR: screen
-- 🖥️ The rows above the cursor, and the row the cursor is on.
-- カーソルより上の行たちと、カーソルのある行
data Screen = Screen
  { above   :: [String]
  , current :: String
  }
  deriving (Show, Eq)

blankScreen :: Screen
blankScreen = Screen [] ""

-- ✏️ Write one character at the cursor.
-- カーソルの位置に、1文字書く
writeChar :: Char -> Screen -> Screen
writeChar '\n' screen = newLine screen
writeChar c screen
  | length (current screen) >= screenWidth =
      writeChar c (newLine screen)
  | otherwise =
      screen { current = current screen <> [c] }

-- ↩️ Move to a new row. Only the last rows fit; the oldest
--    one scrolls away.
-- 次の行へ。入るのは最後の何行かだけ。いちばん古い行は流れて消える
newLine :: Screen -> Screen
newLine (Screen rowsAbove row) =
  let kept = rowsAbove <> [row]
      room = screenHeight - 1
  in Screen (drop (length kept - room) kept) ""

-- ✏️ Write a whole string, one character at a time.
-- 文字列を、1文字ずつ書く
writeString :: String -> Screen -> Screen
writeString text screen = foldl (flip writeChar) screen text
-- ANCHOR_END: screen

-- ANCHOR: render
-- 🖼️ All the rows on the screen, filled to the full size.
-- 画面の全部の行。大きさいっぱいまで空白で埋める
rows :: Screen -> [String]
rows (Screen rowsAbove row) =
  let shown = rowsAbove <> [row]
      blank = replicate (screenHeight - length shown) ""
  in map pad (shown <> blank)
  where
    pad r = r <> replicate (screenWidth - length r) ' '

-- 🖼️ Draw the screen with a frame.
-- 枠つきで画面を描く
render :: Screen -> String
render screen = unlines ([edge] <> map side (rows screen) <> [edge])
  where
    edge   = "+" <> replicate screenWidth '-' <> "+"
    side r = "|" <> r <> "|"
-- ANCHOR_END: render
-- ANCHOR_END: all
