💻 $ lam -n --begin Map.empty \
    -e 'Map.insertWith (+) (slice 11 13 line) 1 acc' \
    --end 'forEach (Map.toList acc)
             (\(h, n) -> putStrLn f"{h}時 {String.repeat n "▇"} {n}")' \
    app.log
