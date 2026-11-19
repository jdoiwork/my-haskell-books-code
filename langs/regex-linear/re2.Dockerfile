# RE2 (C++), to compare with the Pike VM of regex-linear.md. Used by
# check_re2.py, which builds the image when it is missing.
# RE2（C++）。regex-linear.md の Pike VM とくらべるため。check_re2.py が
# 使う。イメージがなければ、check_re2.py が作る
FROM debian:trixie-slim
RUN apt-get update \
 && apt-get install -y --no-install-recommends g++ libre2-dev pkg-config \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /work
