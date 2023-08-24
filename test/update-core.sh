#!/bin/zsh

base_dir=$(pwd)

for target_dir in "$@"; do
  cd $base_dir
  cd $target_dir

  for i in $(find . -d 1 -type d | sort); do
    cd $i
    echo $(basename $i)
    neut add core https://github.com/vekatze/neut-core/raw/main/release/${NEW_VERSION}.tar.zst
    cd ..
  done
done
