💻 $ lam WatchmanOnly.lam sites.txt --port 18743 &

💻 $ curl -s 'localhost:18743/status.json?only=down' | jq 'map(.site)'
