💻 $ lam Watchman.lam sites.txt --port 18742 &

💻 $ curl -s localhost:18742/

💻 $ curl -s localhost:18742/status.json | jq '.[2]'
