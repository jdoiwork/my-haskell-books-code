💻 $ lam -e 'do
      text <- getContents
      forEach (unique (lines text)) putStrLn' < fruits.txt
