💻 $ lam -e 'main = do
      text <- readFile "users.csv" ?
      rows <- pure (Result.mapError Decode.explain (Csv.parse text)) ?
      forEach rows (\r -> putStrLn (Json.encode r))
      pure (Ok ())'
