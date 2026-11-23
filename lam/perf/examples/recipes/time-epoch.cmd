💻 $ lam -pe 'Time.toIso (Time.fromMillis (String.toInt line ?: 0))' \
    epochs.txt

💻 $ lam -ne 'do
      zone <- Time.here
      let t = Time.fromMillis (String.toInt line ?: 0)
      putStrLn (Time.format zone t)' epochs.txt
