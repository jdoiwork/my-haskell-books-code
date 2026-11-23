💻 $ lam -n --begin Map.empty \
    -e 'let ws = Regex.findAll re"[a-z]+" (String.toLower line) in
        foldl (\m w -> Map.insertWith (+) w.text 1 m) acc ws' \
    --end 'Map.toList acc
             |> sortOn (\(w, n) -> (-n, w))
             |> take 5
             |> map (\(w, n) -> f"{n:>3} {w}")
             |> String.join "\n"
             |> putStrLn' words.txt
