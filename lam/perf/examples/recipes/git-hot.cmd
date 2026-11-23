💻 $ git -C shelf log --all --name-only --format= \
    | grep -v '^$' \
    | lam -n --begin Map.empty -e 'Map.insertWith (+) line 1 acc' \
        --end 'Map.toList acc
                 |> sortOn (\(f, n) -> (-n, f))
                 |> take 3
                 |> map (\(f, n) -> f"{n:>3} {f}")
                 |> String.join "\n"
                 |> putStrLn'
