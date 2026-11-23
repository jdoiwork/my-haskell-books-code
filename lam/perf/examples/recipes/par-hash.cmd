💻 $ lam -e 'main = do
      paths <- File.glob "downloads/*.*" ?
      let one p = map (Result.map (\h -> (p, Bytes.toHex h))) (Hash.file p)
      found <- IO.allN 4 (map one (sort paths))
      hashes <- pure (Result.combine found) ?
      forEach hashes (\(p, h) -> putStrLn f"{String.take 12 h}  {p}")
      pure (Ok ())'
