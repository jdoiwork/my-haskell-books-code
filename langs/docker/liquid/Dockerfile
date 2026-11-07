# Liquid Haskell (a GHC plugin) and the Z3 solver, for scripts/check-langs.sh.
# Liquid Haskell（GHC のプラグイン）と、条件を解く Z3。scripts/check-langs.sh が使う
FROM haskell:9.14.1-bookworm
RUN apt-get update && apt-get install -y --no-install-recommends z3 \
 && rm -rf /var/lib/apt/lists/*
ARG LIQUID=0.9.14.1
RUN cabal update -v0 \
 && cabal install -v1 --lib --package-env /opt/liquid liquidhaskell-$LIQUID
WORKDIR /work
