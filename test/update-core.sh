#!/bin/zsh

# e.g. ./update-core.sh ./pfds

NEW_VERSION=0-2-4

TARGET_DIR=$(cd "$1"; pwd)
cd $TARGET_DIR

for i in $(find . -d 1 -type d | sort); do
  cd $i
  echo $(basename $i)
  neut add core https://github.com/vekatze/neut-core/raw/main/release/${NEW_VERSION}.tar.zst
  cd ..
done
