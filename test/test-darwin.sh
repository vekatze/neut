#!/bin/zsh

# e.g. COMPILER_VERSION=0.2.0.0 ./test-arm64-darwin.sh ./data/

TARGET_ARCH=arm64
CLANG_PATH=/opt/homebrew/opt/llvm/bin/clang

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
  MallocNanoZone=0 ASAN_OPTIONS=detect_leaks=1 LSAN_OPTIONS=suppressions=$LSAN_FILE ./.build/$TARGET_ARCH-darwin/$COMPILER_VERSION/executable/$(basename $i)
  cd ..
done
