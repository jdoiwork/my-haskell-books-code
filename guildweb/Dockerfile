# ANCHOR: all
# Build in three stages, and keep only what is needed to run.
# 3つの段階でビルドし、動かすのに要るものだけを残す

# ANCHOR: frontend
# 1. Build the React app into plain files (HTML, JavaScript, CSS).
# 1. React のアプリを、ただのファイル（HTML・JavaScript・CSS）にする
FROM node:26-bookworm-slim AS frontend
WORKDIR /app/frontend
COPY frontend/package.json frontend/package-lock.json ./
RUN npm ci
COPY frontend/ ./
RUN npm run build
# ANCHOR_END: frontend

# ANCHOR: backend
# 2. Build the Haskell server. The dependencies are built first, so that
#    changing our own code does not rebuild them.
# 2. Haskell のサーバーをビルドする。依存を先にビルドしておくので、
#    自分のコードを変えても、依存はビルドし直さない
FROM haskell:9.14.1-slim-bookworm AS backend
RUN apt-get update \
 && apt-get install -y --no-install-recommends libpq-dev pkg-config \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /app
RUN cabal update
COPY cabal.project guildweb.cabal ./
RUN cabal build --only-dependencies all
COPY src/ src/
COPY app/ app/
RUN cabal build exe:guild-web \
 && mkdir -p /app/bin \
 && cp "$(cabal list-bin guild-web)" /app/bin/
# ANCHOR_END: backend

# ANCHOR: runtime
# 3. A small image: the server, the built files, and the C libraries.
# 3. 小さなイメージ：サーバーと、ビルドしたファイルと、C のライブラリ
FROM debian:bookworm-slim
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libpq5 libgmp10 ca-certificates \
 && rm -rf /var/lib/apt/lists/*
RUN useradd --create-home guild
USER guild
WORKDIR /home/guild
COPY --from=backend /app/bin/guild-web ./guild-web
COPY --from=frontend /app/frontend/dist ./static
ENV PORT=8080 STATIC_DIR=static
EXPOSE 8080
CMD ["./guild-web"]
# ANCHOR_END: runtime
# ANCHOR_END: all
