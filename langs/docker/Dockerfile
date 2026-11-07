# The languages that are not installed on the author's Mac. Used by
# scripts/check-langs.sh, which builds the image when it is missing.
# 著者の Mac に入れていない言語の道具。scripts/check-langs.sh が使う。
# イメージがなければ、scripts/check-langs.sh が作る
#
#   Gleam   BEAM の上で動かす（土台のイメージの Erlang/OTP）
#   Kotlin  kotlinc でコンパイルして java で動かす（コルーチンの jar も入れる）
#   Scala   scala-cli で動かす
#   Koka    koka でコンパイルして動かす
#   Elm     elm make で JavaScript にして、node で動かす
#   Scheme  Guile で動かす
#
# The versions are in notes/tooling.md, too. Update both together.
# 版は notes/tooling.md にも書いてある。変えるときは両方を直す
FROM erlang:29
ARG TARGETARCH

# Java (for Kotlin and Scala) and Node (for Elm), from their official images.
# Java（Kotlin と Scala のため）と Node（Elm のため）は、公式のイメージから持ってくる
COPY --from=eclipse-temurin:25-jdk /opt/java/openjdk /opt/java/openjdk
COPY --from=node:26-trixie /usr/local/bin/node /usr/local/bin/node
COPY --from=node:26-trixie /usr/local/lib/node_modules /usr/local/lib/node_modules
RUN ln -s ../lib/node_modules/npm/bin/npm-cli.js /usr/local/bin/npm
ENV JAVA_HOME=/opt/java/openjdk
ENV PATH=/opt/java/openjdk/bin:/opt/kotlinc/bin:/opt/elm/node_modules/.bin:$PATH
RUN apt-get update && apt-get install -y --no-install-recommends unzip guile-3.0 \
 && rm -rf /var/lib/apt/lists/*

ARG GLEAM=1.19.2
RUN arch=$([ "$TARGETARCH" = arm64 ] && echo aarch64 || echo x86_64) \
 && curl -fsSL https://github.com/gleam-lang/gleam/releases/download/v$GLEAM/gleam-v$GLEAM-$arch-unknown-linux-musl.tar.gz \
  | tar xz -C /usr/local/bin

ARG KOTLIN=2.4.21
ARG COROUTINES=1.11.0
RUN curl -fsSL -o /tmp/k.zip https://github.com/JetBrains/kotlin/releases/download/v$KOTLIN/kotlin-compiler-$KOTLIN.zip \
 && unzip -q /tmp/k.zip -d /opt && rm /tmp/k.zip \
 && curl -fsSL -o /opt/kotlinc/lib/kotlinx-coroutines-core.jar \
    https://repo1.maven.org/maven2/org/jetbrains/kotlinx/kotlinx-coroutines-core-jvm/$COROUTINES/kotlinx-coroutines-core-jvm-$COROUTINES.jar

# scala-cli downloads the compiler on the first run, so run it once here.
# scala-cli は、最初に動かしたときにコンパイラを取ってくるので、ここで1回動かしておく
ARG SCALA_CLI=1.18.0
ARG SCALA=3.10.0
RUN arch=$([ "$TARGETARCH" = arm64 ] && echo aarch64 || echo x86_64) \
 && curl -fsSL https://github.com/VirtusLab/scala-cli/releases/download/v$SCALA_CLI/scala-cli-$arch-pc-linux.gz \
  | gunzip > /usr/local/bin/scala-cli && chmod +x /usr/local/bin/scala-cli \
 && echo '@main def hello() = println("ok")' > /tmp/hello.scala \
 && scala-cli run --server=false --scala $SCALA /tmp/hello.scala && rm /tmp/hello.scala

ARG KOKA=3.2.9
RUN arch=$([ "$TARGETARCH" = arm64 ] && echo arm64 || echo x64) \
 && curl -fsSL https://github.com/koka-lang/koka/releases/download/v$KOKA/koka-v$KOKA-linux-$arch.tar.gz \
  | tar xz -C /usr/local

# Elm keeps its packages in ELM_HOME. Fetch elm/core and elm/json here.
# Elm は ELM_HOME にパッケージを置く。elm/core と elm/json を、ここで取ってくる
ARG ELM=0.19.3-0
ENV ELM_HOME=/opt/elm/home
RUN mkdir -p /opt/elm && cd /opt/elm && npm install --silent elm@$ELM \
 && mkdir /tmp/e && cd /tmp/e && yes | elm init >/dev/null && yes | elm install elm/json >/dev/null && rm -rf /tmp/e

WORKDIR /work
