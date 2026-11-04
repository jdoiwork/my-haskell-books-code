# ANCHOR: all
# A container with the same GHC and cabal as this book.
# この本と同じ GHC と cabal が入ったコンテナー
FROM mcr.microsoft.com/devcontainers/base:ubuntu

USER vscode
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.14.1 \
    BOOTSTRAP_HASKELL_CABAL_VERSION=3.16.1.0 \
    BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
    BOOTSTRAP_HASKELL_ADJUST_BASHRC=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
# ANCHOR_END: all
