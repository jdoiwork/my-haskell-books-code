💻 $ lam -e 'do
      text <- getContents
      forEach (indexed (lines text))
        (\(i, l) -> putStrLn f"{i + 1:>3}  {l}")' < settings.conf
