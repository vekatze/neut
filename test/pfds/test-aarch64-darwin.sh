#!/bin/zsh

for i in $(find . -d 1 -type d | sort); do
    cd $i
    echo $(basename $i)
    neut clean
    NEUT_TARGET_ARCH=aarch64 NEUT_CLANG=/opt/homebrew/opt/llvm/bin/clang neut build --clang-option "-fsanitize=address"
    # https://stackoverflow.com/questions/64126942/malloc-nano-zone-abandoned-due-to-inability-to-preallocate-reserved-vm-space
    MallocNanoZone=0 ASAN_OPTIONS=detect_leaks=1 LSAN_OPTIONS=suppressions=../lsan.supp ./target/$TARGET_PLATFORM/$COMPILER_VERSION/executable/$(basename $i)
    cd ..
done
