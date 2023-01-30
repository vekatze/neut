#!/bin/zsh
for i in $(find . -d 1 -type d | sort); do
    cd $i
    echo $(basename $i)
    neut clean
    neut build --emit llvm --skip-link
    /opt/homebrew/opt/llvm/bin/clang -fsanitize=address -Wno-override-module ./target/artifact/*.ll -o ./target/a.out
    ASAN_OPTIONS=detect_leaks=1 LSAN_OPTIONS=suppressions=../lsan.supp ./target/a.out
    cd ..
done
