💻 $ lam -ne 'when (String.trim line /= ""
                 && not (String.startsWith "#" line)) (putStrLn line)' \
    settings.conf
