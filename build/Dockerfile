FROM debian:13.3-slim

WORKDIR /app/

ENV LANG=C.utf8
ENV LC_ALL=C.utf8

RUN apt-get update && \
  apt-get install --no-install-recommends -y \
  ca-certificates \
  cmake \
  curl \
  g++ \
  gnupg \
  just \
  lsb-release \
  ninja-build \
  python3 \
  tar \
  wget \
  xz-utils \
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

ENV WASMTIME_VERSION=46.0.1

RUN case "$(dpkg --print-architecture)" in \
      amd64) wasmtime_arch=x86_64 ;; \
      arm64) wasmtime_arch=aarch64 ;; \
      *) echo "unsupported architecture: $(dpkg --print-architecture)" >&2; exit 1 ;; \
    esac && \
  wasmtime_dir="wasmtime-v${WASMTIME_VERSION}-${wasmtime_arch}-linux" && \
  curl -sSL -o /tmp/wasmtime.tar.xz \
  "https://github.com/bytecodealliance/wasmtime/releases/download/v${WASMTIME_VERSION}/${wasmtime_dir}.tar.xz" && \
  tar -xJf /tmp/wasmtime.tar.xz -C /tmp && \
  mv "/tmp/${wasmtime_dir}/wasmtime" /usr/local/bin/wasmtime && \
  rm -rf /tmp/wasmtime.tar.xz "/tmp/${wasmtime_dir}"
