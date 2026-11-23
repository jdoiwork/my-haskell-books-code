💻 $ lam -an --begin Map.empty \
    -e 'let u = field (-1) in
        if String.startsWith "user=" u
        then Map.insertWith (+) (String.drop 5 u) 1 acc else acc' \
    --end 'forEach (sortOn (\(u, n) -> (-n, u)) (Map.toList acc))
             (\(u, n) -> putStrLn f"{n:>3} {u}")' app.log
