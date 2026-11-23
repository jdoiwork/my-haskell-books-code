💻 $ lam -n --begin Map.empty \
    -e 'let ws = Regex.findAll re"[a-z]+" (String.toLower line) in
        foldl (\m w -> Map.insertWith (+) w.text 1 m) acc ws' \
    --end 'let top = take 5 (sortOn (\(w, n) -> (-n, w)) (Map.toList acc)) in
           forEach top (\(w, n) -> putStrLn f"{n:>3} {w}")' words.txt
