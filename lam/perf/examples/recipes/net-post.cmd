💻 $ lam fake-api.lam --port 18731 &

💻 $ lam -e 'main = do
      let note = JObj {"title": JStr "buy milk", "done": JBool false}
      answer <- Http.send "POST" "http://127.0.0.1:18731/echo"
                  {"Content-Type": "application/json"} (Json.show note) ?
      putStrLn (Http.string answer ?: "")
      pure (Ok ())'
