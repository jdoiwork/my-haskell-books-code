# ANCHOR: all
# The shelf as one image: two programs (shelf, shelf-tui) on a small
# Debian, with poppler for taking in PDFs. Built in two stages, so that
# GHC and cabal stay behind.
# 本棚を1つのイメージに：小さな Debian の上に、プログラム2つ（shelf と
# shelf-tui）と、PDF の取り込みのための poppler。2段でビルドするので、
# GHC と cabal は置いていく

# ANCHOR: build
# 1. Build. cabal's downloads and its store of built packages live in
#    BuildKit's cache mounts, so they survive from build to build: when
#    our code or the cabal file changes, the dependencies are not built
#    again.
# 1. ビルドする。cabal のダウンロードと、ビルド済みのパッケージの置き場は、
#    BuildKit のキャッシュのマウントに置くので、ビルドをまたいで残る：
#    自分のコードや cabal ファイルを変えても、依存はビルドし直さない
FROM haskell:9.14.1-slim-bookworm AS build
RUN apt-get update \
 && apt-get install -y --no-install-recommends libpq-dev pkg-config zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY cabal.project tsundoku.cabal ./
COPY src/ src/
COPY answers/ answers/
COPY app/ app/
COPY static/ static/
RUN --mount=type=cache,target=/root/.cache/cabal \
    --mount=type=cache,target=/root/.local/state/cabal \
    cabal update \
 && cabal build exe:shelf exe:shelf-tui \
 && mkdir -p /out \
 && cp "$(cabal list-bin shelf)" "$(cabal list-bin shelf-tui)" /out/
# ANCHOR_END: build

# ANCHOR: runtime
# 2. Run. Only the programs and the C libraries they need. htmx and the
#    style are inside the program already (file-embed).
# 2. 動かす。プログラムと、それが使う C のライブラリだけ。htmx と見た目は、
#    もうプログラムの中にある（file-embed）
FROM debian:bookworm-slim
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libpq5 libgmp10 zlib1g poppler-utils ca-certificates \
 && rm -rf /var/lib/apt/lists/*
RUN useradd --create-home --uid 1000 shelf \
 && mkdir /data && chown shelf /data
USER shelf
WORKDIR /data
COPY --from=build /out/shelf /out/shelf-tui /usr/local/bin/
ENV TSUNDOKU_DB=sqlite:/data/tsundoku.db \
    TSUNDOKU_HOST=0.0.0.0
EXPOSE 8080
VOLUME /data
CMD ["shelf", "serve", "8080"]
# ANCHOR_END: runtime
# ANCHOR_END: all
