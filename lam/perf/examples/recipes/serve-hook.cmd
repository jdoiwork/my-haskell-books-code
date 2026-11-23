💻 $ lam hook.lam --port 18733 &

💻 $ curl -s -w ' %{http_code}\n' -d '{"event": "push"}' localhost:18733/hook

💻 $ curl -s -w ' %{http_code}\n' -d 'hello' localhost:18733/hook

💻 $ cat hooks.jsonl
