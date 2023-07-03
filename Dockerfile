FROM haskell:9.2.8-buster

WORKDIR /app/

RUN apt-get update && \
  apt-get install --no-install-recommends -y \
  curl \
  gnupg \
  lsb-release \
  software-properties-common \
  tar \
  wget \
  xxd \
  zstd && \
  rm -rf /var/lib/apt/lists/*

RUN wget --progress=dot:giga https://apt.llvm.org/llvm.sh && \
  chmod +x llvm.sh && \
  ./llvm.sh 14 && \
  apt-get install --no-install-recommends -y \
  libclang-rt-14-dev && \
  ln -s /usr/bin/clang-14 /usr/bin/clang

COPY package.yaml /app/

COPY stack.yaml /app/

RUN stack build --only-dependencies
