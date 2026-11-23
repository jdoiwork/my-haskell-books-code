💻 $ lam -an \
    -e 'let ms = String.toInt (String.replace "ms" "" (field 6)) ?: 0 in
        when (ms >= 500) (putStrLn f"{ms:>5}ms {field 4}")' app.log
