git log --format=%s v0.25.0..HEAD | lam -ne 'when (line =~ m/^(feat|fix)/) (putStrLn line)'
