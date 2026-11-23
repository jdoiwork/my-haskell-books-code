💻 $ lam -e 'main = Sql.withDb "money.db" (\db -> do
      let sql = "SELECT category, count(*) AS n, sum(amount) AS total
                 FROM expenses GROUP BY category ORDER BY total DESC"
      let row = (\c n t -> [c, show n, show t])
                  <$> Decode.field "category" Decode.string
                  <*> Decode.field "n" Decode.int
                  <*> Decode.field "total" Decode.int
      rows <- Sql.queryAs row db sql [] ?
      putStrLn (Term.table ["category", "n", "total"] rows)
      pure (Ok ()))'
