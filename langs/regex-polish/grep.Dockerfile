# GNU grep and ripgrep, to compare with minigrep (regex-polish.md). Used
# by check_grep.py, which builds the image when it is missing.
# GNU grep と ripgrep。minigrep（regex-polish.md）とくらべるため。
# check_grep.py が使う。イメージがなければ、check_grep.py が作る
#
#   GNU grep 3.12  Debian forky's package
#   ripgrep 15.2.0 the binary of its GitHub release (musl)
FROM debian:forky-slim
ARG TARGETARCH
ARG RIPGREP=15.2.0
RUN apt-get update \
 && apt-get install -y --no-install-recommends grep ca-certificates curl locales \
 && arch=$([ "$TARGETARCH" = arm64 ] && echo aarch64 || echo x86_64) \
 && curl -fsSL https://github.com/BurntSushi/ripgrep/releases/download/$RIPGREP/ripgrep-$RIPGREP-$arch-unknown-linux-musl.tar.gz \
  | tar xz -C /tmp \
 && mv /tmp/ripgrep-$RIPGREP-$arch-unknown-linux-musl/rg /usr/local/bin/rg \
 && apt-get purge -y curl && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* /tmp/ripgrep-*
ENV LANG=C.UTF-8
WORKDIR /work
