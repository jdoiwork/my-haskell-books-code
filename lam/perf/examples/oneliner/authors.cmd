-ne 'do a <- Shell.out
    $"git log -1 --format=%an -- {line}"; putStrLn f"{line}: {a ?: "?"}"'
