#!/bin/sh

# TARGET_ARCH=amd64

TARGET_DIR=$(cd "$1"; pwd)
cd $TARGET_DIR

for i in $(find . -maxdepth 1 -mindepth 1 -type d | sort); do
  cd $i
  echo $(basename $i)
  NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT clean
  NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT build --clang-option "-fsanitize=address"
  ASAN_OPTIONS=detect_leaks=1 ./.build/$TARGET_ARCH-linux/compiler-$COMPILER_VERSION/build-option-rF8AhaYk5v3_QpQ1ErUpo1C-MO7U4-JJtuLrxsf6pWI=/executable/$(basename $i)
  cd ..
done