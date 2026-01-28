FROM debian:13.3-slim

WORKDIR /app/

ENV LANG=C.utf8
ENV LC_ALL=C.utf8

RUN apt-get update && \
  apt-get install --no-install-recommends -y \
  ca-certificates \
  curl \
  g++ \
  gnupg \
  just \
  lsb-release \
  tar \
  wget \
  zstd && \
  curl -sSL https://get.haskellstack.org/ | sh && \
  rm -rf /var/lib/apt/lists/* && \
  curl -O https://apt.llvm.org/llvm.sh && \
  chmod +x llvm.sh && \
  ./llvm.sh 21 && \
  ln -s /usr/bin/clang-21 /usr/bin/clang

COPY build/package.template.yaml /app/build/package.template.yaml

RUN cp /app/build/package.template.yaml /app/package.yaml

COPY stack.yaml /app/

RUN stack build --only-dependencies

RUN apt-get update && \
  apt-get install --no-install-recommends -y \
  hyperfine \
  libcairo2-dev \
  libpixman-1-dev \
  libsdl-pango-dev \
  npm \
  pkg-config \
  zsh && \
  rm -rf /var/lib/apt/lists/*
