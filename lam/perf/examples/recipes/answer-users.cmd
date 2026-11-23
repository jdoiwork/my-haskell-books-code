💻 $ lam -an --begin Map.empty \
    -e 'let u = field (-1) in
        if String.startsWith "user=" u
          then Map.insertWith (+) (String.drop 5 u) 1 acc
          else acc' \
    --end 'Map.toList acc
             |> sortOn (\(u, n) -> (-n, u))
             |> map (\(u, n) -> f"{n:>3} {u}")
             |> String.join "\n"
             |> putStrLn' app.log
