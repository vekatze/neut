#!/bin/zsh
for i in $(find . -d 1 -type d | sort); do
    cd $i
    echo $(basename $i)
    neut clean
    NEUT_TARGET_ARCH=aarch64 NEUT_CLANG=/opt/homebrew/opt/llvm/bin/clang neut build --clang-option "-fsanitize=address"
    ASAN_OPTIONS=detect_leaks=1 LSAN_OPTIONS=suppressions=../lsan.supp ./target/executable/$(basename $i)
    cd ..
done
