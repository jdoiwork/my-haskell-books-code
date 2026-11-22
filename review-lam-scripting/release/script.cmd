lam --print-script=plain -F: -an --begin Map.empty -e 'Map.insertWith (+) ((s/[(!].*//) (field 1)) 1 acc' --end 'forEach (Map.toList acc) print' > counted.lam
git log --format=%s v0.25.0..HEAD | lam counted.lam
