# ANCHOR: all
# 🐳 The TODO API as an image: lam and the two scripts on a small
#    Debian. Built in two stages, so GHC and cabal stay behind.
#      docker build -t lam-todo .
#      docker run --rm -p 8080:8080 lam-todo
# TODO の API をイメージに：小さな Debian の上に、lam とスクリプト2つ。
# 2段に分けてビルドするので、GHC と cabal は置いていく

# ANCHOR: build
# 1. Build lam. cabal's downloads and its store of built packages live
#    in BuildKit's cache mounts: they stay from build to build.
# 1. lam をビルドする。cabal のダウンロードと、ビルド済みのパッケージの
#    置き場は、BuildKit のキャッシュのマウントに置く（ビルドをまたいで残る）
FROM haskell:9.14.1-slim-bookworm AS build
RUN apt-get update \
 && apt-get install -y --no-install-recommends zlib1g-dev pkg-config \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY cabal.project lam.cabal ./
COPY src/ src/
COPY answers/ answers/
COPY app/ app/
RUN --mount=type=cache,target=/root/.cache/cabal \
    --mount=type=cache,target=/root/.local/state/cabal \
    cabal update \
 && cabal build exe:lam \
 && mkdir -p /out \
 && cp "$(cabal list-bin lam)" /out/
# ANCHOR_END: build

# ANCHOR: runtime
# 2. Run. Only lam, the C libraries it needs, and the scripts. The
#    scripts stay text: change one, and only this last layer is built
#    again. LAM_HOST=0.0.0.0 lets the requests from outside the
#    container in; --allow-net still says which port.
# 2. 動かす。lam と、それが使う C のライブラリと、スクリプトだけ。
#    スクリプトは文字のまま：書き換えても、作り直すのは最後の層だけ。
#    LAM_HOST=0.0.0.0 で、コンテナーの外からの頼みを入れる。どのポートかは、
#    これまでどおり --allow-net が言う
FROM debian:bookworm-slim
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libgmp10 zlib1g ca-certificates \
 && rm -rf /var/lib/apt/lists/*
COPY --from=build /out/lam /usr/local/bin/lam
RUN useradd --create-home --uid 1000 lam
USER lam
WORKDIR /app
COPY examples/server/todo.lam examples/server/Todos.lam ./
ENV LAM_HOST=0.0.0.0
EXPOSE 8080
CMD ["lam", "--allow-net=localhost:8080", "todo.lam", "--port", "8080"]
# ANCHOR_END: runtime
# ANCHOR_END: all
