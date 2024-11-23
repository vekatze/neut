#!/bin/zsh

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd)

for target_dir in $(find "$SCRIPT_DIR/../action" -maxdepth 1 -mindepth 1 -type d | sort); do
  cd $target_dir
  echo $target_dir
  size=$(cat ./test-size.txt)
  $NEUT clean
  $NEUT build "$(basename ${target_dir})-nt" --execute $((size / 10))
  echo ""
done
