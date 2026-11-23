💻 $ git -C shelf log --all --no-merges --format=%an \
    | lam -n --begin Map.empty -e 'Map.insertWith (+) line 1 acc' \
        --end 'forEach (sortOn (\(a, n) -> (-n, a)) (Map.toList acc))
                 (\(a, n) -> putStrLn f"{n:>3} {a}")'
