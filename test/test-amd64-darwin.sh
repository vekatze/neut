#!/bin/zsh

# e.g. COMPILER_VERSION=0.2.0.0 ./test-amd64-darwin.sh ./data/

TARGET_ARCH=amd64
CLANG_PATH=/usr/bin/clang

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd)
LSAN_FILE=$SCRIPT_DIR/lsan.supp

TARGET_DIR=$(cd "$1"; pwd)
cd $TARGET_DIR

for i in $(find . -d 1 -type d | sort); do
  cd $i
  echo $(basename $i)
  NEUT_TARGET_ARCH=$TARGET_ARCH neut clean
  NEUT_TARGET_ARCH=$TARGET_ARCH NEUT_CLANG=$CLANG_PATH neut build --clang-option "-fsanitize=address"
  # https://stackoverflow.com/questions/64126942
  echo $(pwd)
  MallocNanoZone=0 ./.build/$TARGET_ARCH-darwin/$COMPILER_VERSION/executable/$(basename $i)
  cd ..
done
