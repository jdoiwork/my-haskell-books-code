💻 $ git -C shelf log --all --name-only --format= \
    | lam -n --begin Map.empty \
        -e 'if line == "" then acc else Map.insertWith (+) line 1 acc' \
        --end 'forEach (take 3 (sortOn (\(f, n) -> (-n, f)) (Map.toList acc)))
                 (\(f, n) -> putStrLn f"{n:>3} {f}")'
