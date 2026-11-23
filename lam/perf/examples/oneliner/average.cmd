-an --begin '(0, 0)' -e 'let (t, n) = acc in (t + (String.toInt (field 2) ?: 0), n + 1)' --end 'let (t, n) = acc in
           putStrLn f"average {toFloat t / toFloat n:.1}"' examples/oneliner/scores.txt
