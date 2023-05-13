#!/bin/zsh

# e.g. ./update-core.sh ./data

OLD_VERSION=0.2.0.9
NEW_VERSION=0.2.0.10

TARGET_DIR=$(cd "$1"; pwd)
cd $TARGET_DIR

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
