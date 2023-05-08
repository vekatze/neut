#!/bin/zsh

# e.g. TARGET_PLATFORM=arm64-linux COMPILER_VERSION=0.2.0.0 ./test-arm64-linux.sh

for i in $(find . -maxdepth 1 -mindepth 1 -type d | sort); do
  cd $i
  echo $(basename $i)
  NEUT_TARGET_ARCH=arm64 neut clean
  NEUT_TARGET_ARCH=arm64 neut build --clang-option "-fsanitize=address"
  # # https://stackoverflow.com/questions/64126942/malloc-nano-zone-abandoned-due-to-inability-to-preallocate-reserved-vm-space
  ASAN_OPTIONS=detect_leaks=1 ./.build/$TARGET_PLATFORM/$COMPILER_VERSION/executable/$(basename $i)
  cd ..
done
