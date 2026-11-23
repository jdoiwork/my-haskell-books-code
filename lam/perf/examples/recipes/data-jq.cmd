💻 $ lam --jsonl \
    -ne 'let ms = Maybe.andThen Json.int (Json.get "ms" json) ?: 0 in
         when (ms >= 100) (putStrLn (Json.show json))' requests.jsonl

💻 $ lam --json -pe 'json' books.json
