# hasktorch in Linux. On the author's Mac (macOS 27, Xcode's new linker),
# libtorch-ffi does not link yet (notes/tooling.md), so we build it here.
# PyTorch's Linux libtorch download is for x86_64 only, so on arm64 we use
# the libtorch inside the pip wheel of torch, the same version (2.9.1) that
# libtorch-ffi 2.0.2.0 expects. hasktorch and PyTorch then share one libtorch.
# Linux の中の hasktorch。著者の Mac（macOS 27、Xcode の新しいリンカー）では、
# まだ libtorch-ffi がリンクできない（notes/tooling.md）ので、ここで組む。
# PyTorch が配る Linux 用の libtorch は x86_64 だけなので、arm64 では、pip の
# torch の中の libtorch を使う。版は libtorch-ffi 2.0.2.0 が待つ 2.9.1。
# hasktorch と PyTorch が、同じ libtorch を使うことになる
#
#   docker build -t ml-hasktorch -f docker/Dockerfile .   (code/ml/hasktorch で)
#   docker run --rm ml-hasktorch
FROM haskell:9.10.3-slim-bookworm

ARG TORCH=2.9.1
RUN apt-get update && apt-get install -y --no-install-recommends python3 python3-venv g++ \
 && rm -rf /var/lib/apt/lists/* \
 && python3 -m venv /opt/py \
 && /opt/py/bin/pip install --no-cache-dir torch==$TORCH --extra-index-url https://download.pytorch.org/whl/cpu
ENV TORCH_DIR=/opt/py/lib/python3.11/site-packages/torch
ENV LD_LIBRARY_PATH=$TORCH_DIR/lib
# Tell libtorch-ffi not to download libtorch.
# libtorch-ffi に、libtorch を取ってこないように伝える
ENV LIBTORCH_SKIP_DOWNLOAD=1

WORKDIR /work
COPY ml-hasktorch.cabal ./
COPY docker/cabal.project.docker ./cabal.project
RUN cabal update -v0 \
 && cabal build -v1 --only-dependencies
COPY app ./app
COPY docker/bench ./bench
RUN cabal build -v1 \
 && cp "$(cabal list-bin hasktorch-matmul)" /usr/local/bin/
# NumPy, only to quiet PyTorch's warning. In the last layer, so that adding
# it does not rebuild hasktorch.
# NumPy は PyTorch の警告を消すためだけ。hasktorch を組み直さないよう、最後の層に置く
RUN /opt/py/bin/pip install --no-cache-dir numpy
CMD ["sh", "bench/run.sh"]
