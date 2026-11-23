💻 $ lam serve-dir.lam --port 18732 --dir site &

💻 $ curl -s localhost:18732/

💻 $ curl -si localhost:18732/notes.txt | grep -i -e ^HTTP -e ^content-type

💻 $ curl -s -w ' %{http_code}\n' localhost:18732/nothing.txt

💻 $ curl -s -w ' %{http_code}\n' --path-as-is localhost:18732/../lam.json
