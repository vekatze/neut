#!/bin/zsh

# e.g. TARGET_PLATFORM=aarch64-darwin COMPILER_VERSION=0.2.0.0 ./test-aarch64-darwin.sh
OLD_VERSION=0.2.0.3
NEW_VERSION=0.2.0.7

for i in $(find . -d 1 -type d | sort); do
  cd $i
  rg $OLD_VERSION > /dev/null
  if [[ $? == 0 ]]; then
    echo $(basename $i)
    neut add core https://github.com/vekatze/neut-core/raw/main/release/${NEW_VERSION}.tar.zst
    echo ""
  fi
  cd ..
done
