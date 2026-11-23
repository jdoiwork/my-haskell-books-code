💻 $ lam fake-api.lam --port 18731 &

💻 $ lam -e 'main = do
      let name = Decode.field "name" Decode.string
      names <- Http.getAs (Decode.list name) "http://127.0.0.1:18731/users" ?
      forEach names putStrLn
      pure (Ok ())'

💻 $ lam -e 'main = do
      user <- Http.get "http://127.0.0.1:18731/users/7" ?
      print user
      pure (Ok ())'
