💻 $ lam -an --begin Map.empty \
    -e 'if field 5 =~ m/^\d{3}$/
          then Map.insertWith (+) (field 5) 1 acc
          else acc' \
    --end 'forEach (Map.toList acc) (\(s, n) -> putStrLn f"{s} {n}")' \
    --print-script=cli > status.lam

💻 $ lam status-min.lam --min 2 app.log

💻 $ lam status-min.lam --help
