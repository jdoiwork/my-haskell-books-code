-n --begin Map.empty -e 'Map.insertWith (\a b -> a + b) line 1 acc' --end 'forEach (Map.toList acc)
             (\(fruit, n) -> putStrLn f"{n:>3} {fruit}")' examples/oneliner/fruits.txt
