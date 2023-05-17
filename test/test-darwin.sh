#!/bin/zsh

TARGET_ARCH=arm64
CLANG_PATH=/opt/homebrew/opt/llvm/bin/clang

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd)
LSAN_FILE=$SCRIPT_DIR/lsan.supp

TARGET_DIR=$(cd "$1"; pwd)
cd $TARGET_DIR

exit_code=0

for i in $(find . -d 1 -type d | sort); do
  cd $i
  echo $(basename $i)
  NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT clean
  NEUT_TARGET_ARCH=$TARGET_ARCH NEUT_CLANG=$CLANG_PATH $NEUT build --clang-option "-fsanitize=address"
  # https://stackoverflow.com/questions/64126942
  MallocNanoZone=0 ASAN_OPTIONS=detect_leaks=1 LSAN_OPTIONS=suppressions=$LSAN_FILE ./.build/$TARGET_ARCH-darwin/compiler-$COMPILER_VERSION/build-option-cLHs7km0Z68odNMcuhrD2pgQSCxXZuyQVSxpwrPsbVY=/executable/$(basename $i)
  last_exit_code=$?
  if [ $last_exit_code -eq 1 ]; then
    exit_code=$last_exit_code
  fi
  cd ..
done

exit $exit_code
