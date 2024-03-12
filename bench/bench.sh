#!/bin/zsh

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd)

for target_dir in $(find $SCRIPT_DIR/action -type d -d 1 | sort); do
  cd $target_dir
  size=$(cat ./test-size.txt)
  for executable in $(find $target_dir/bin -type f | sort); do
    echo ${executable:t}
    hyperfine -r 1 -P SIZE $((size/10)) $size -D $((size/10)) "${executable} {SIZE}" --export-json ../../result/${executable:t}.json
  done
done
