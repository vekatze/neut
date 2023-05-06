#!/bin/zsh

# e.g. TARGET_PLATFORM=aarch64-darwin COMPILER_VERSION=0.2.0.0 ./test-aarch64-darwin.sh

for i in $(find . -d 1 -type d | sort); do
    cd $i
    echo $(basename $i)
    NEUT_TARGET_ARCH=arm64 neut clean
    NEUT_TARGET_ARCH=arm64 NEUT_CLANG=/opt/homebrew/opt/llvm/bin/clang neut build --clang-option "-fsanitize=address"
    # https://stackoverflow.com/questions/64126942/malloc-nano-zone-abandoned-due-to-inability-to-preallocate-reserved-vm-space
    MallocNanoZone=0 ASAN_OPTIONS=detect_leaks=1 LSAN_OPTIONS=suppressions=../lsan.supp ./.build/$TARGET_PLATFORM/$COMPILER_VERSION/executable/$(basename $i)
    cd ..
done
