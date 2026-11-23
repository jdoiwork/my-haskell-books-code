💻 $ git -C shelf log --all --no-merges --format=%an \
    | lam -n --begin Map.empty -e 'Map.insertWith (+) line 1 acc' \
        --end 'Map.toList acc
                 |> sortOn (\(a, n) -> (-n, a))
                 |> map (\(a, n) -> f"{n:>3} {a}")
                 |> String.join "\n"
                 |> putStrLn'
