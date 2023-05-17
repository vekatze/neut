#!/bin/sh

TARGET_DIR=$(cd "$1"; pwd)
cd $TARGET_DIR

exit_code=0

for i in $(find . -maxdepth 1 -mindepth 1 -type d | sort); do
  cd $i
  echo $(basename $i)
  NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT clean
  NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT build --clang-option "-fsanitize=address"
  ASAN_OPTIONS=detect_leaks=1 ./.build/$TARGET_ARCH-linux/compiler-$COMPILER_VERSION/build-option-cLHs7km0Z68odNMcuhrD2pgQSCxXZuyQVSxpwrPsbVY=/executable/$(basename $i) > actual
  last_exit_code=$?
  if [ $last_exit_code -ne 0 ]; then
    exit_code=$last_exit_code
  fi
  diff expected actual
  last_exit_code=$?
  if [ $last_exit_code -ne 0 ]; then
    exit_code=$last_exit_code
  fi
  cd ..
done

exit $exit_code
