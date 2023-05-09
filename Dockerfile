FROM haskell:9.2.6-buster

WORKDIR /app/

RUN apt-get update && \
  apt-get install --no-install-recommends -y \
  curl=7.64.0-4+deb10u6 \
  gnupg=2.2.12-1+deb10u2 \
  lsb-release=10.2019051400 \
  software-properties-common=0.96.20.2-2 \
  tar=1.30+dfsg-6 \
  wget=1.20.1-1.1 \
  zstd=1.3.8+dfsg-3+deb10u2 && \
  rm -rf /var/lib/apt/lists/*

RUN wget --progress=dot:giga https://apt.llvm.org/llvm.sh && \
  chmod +x llvm.sh && \
  ./llvm.sh 14 && \
  apt-get install --no-install-recommends -y \
  libclang-rt-14-dev=1:14.0.6~++20230131082218+f28c006a5895-1~exp1~20230131082246.174 && \
  ln -s /usr/bin/clang-14 /usr/bin/clang

COPY package.yaml /app/

COPY stack.yaml /app/

RUN stack build --only-dependencies
