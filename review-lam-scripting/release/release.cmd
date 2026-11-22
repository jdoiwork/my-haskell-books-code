lam --allow-run=git --allow-read=. --allow-write=. release.lam --yes
cat lam.toml
head -14 CHANGELOG.md
cat dist/SHA256SUMS
shasum -a 256 dist/lam-0.26.0.tar
git log --oneline -2
git tag
lam --allow-run=git --allow-read=. --allow-write=. release.lam --yes
