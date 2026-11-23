💻 $ lam -e 'main = do
      paths <- File.glob "downloads/**/*" ?
      infos <- mapM (\p -> map (Result.map (\i -> (i.size, p))) (File.info p))
                    paths
      sized <- pure (Result.combine infos) ?
      forEach (take 3 (sortOn (\(s, _) -> -s) sized))
        (\(s, p) -> putStrLn f"{s:>10,}  {p}")
      pure (Ok ())'
