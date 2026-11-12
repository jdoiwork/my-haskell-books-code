# ANCHOR: all
# The shelf as one image: two programs (shelf, shelf-tui) and the React
# screen on a small Debian, with poppler for taking in PDFs. Built in
# stages, so that GHC, cabal, and Node stay behind.
# 本棚を1つのイメージに：小さな Debian の上に、プログラム2つ（shelf と
# shelf-tui）と React の画面、PDF の取り込みのための poppler。段に分けて
# ビルドするので、GHC と cabal と Node は置いていく

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

# ANCHOR: web
# 2. Build the React screen (Mantine) into plain files. npm's downloads
#    live in a cache mount too. The TypeScript types from Haskell
#    (tsundoku-api.ts) are in web/src already, so no GHC is needed here.
# 2. React の画面（Mantine）を、ただのファイルにビルドする。npm の
#    ダウンロードも、キャッシュのマウントに置く。Haskell から作った
#    TypeScript の型（tsundoku-api.ts）はもう web/src にあるので、ここに
#    GHC は要らない
FROM node:26-bookworm-slim AS web
WORKDIR /web
COPY web/package.json web/package-lock.json ./
RUN --mount=type=cache,target=/root/.npm npm ci
COPY web/ ./
RUN npm run build
# ANCHOR_END: web

# ANCHOR: runtime
# 3. Run. Only the programs, the C libraries they need, and the React
#    screen's files; TSUNDOKU_WEB tells `shelf serve` to hand them out
#    (instead of the htmx pages).
# 3. 動かす。プログラムと、それが使う C のライブラリと、React の画面の
#    ファイルだけ。TSUNDOKU_WEB で、shelf serve が（htmx のページの
#    代わりに）それを配る
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
COPY --from=web /web/dist /usr/local/share/tsundoku/web
ENV TSUNDOKU_DB=sqlite:/data/tsundoku.db \
    TSUNDOKU_HOST=0.0.0.0 \
    TSUNDOKU_WEB=/usr/local/share/tsundoku/web
EXPOSE 8080
VOLUME /data
CMD ["shelf", "serve", "8080"]
# ANCHOR_END: runtime
# ANCHOR_END: all
