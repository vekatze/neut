#!/bin/zsh

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd)

for target_dir in $(find "$SCRIPT_DIR/../action" -d 1 -type d | sort); do
  cd $target_dir
  stack install --local-bin-path ./bin
  $NEUT build --install ./bin
  size=$(cat ./test-size.txt)
  step=3
  for executable in $(find $target_dir/bin -type f | sort); do
    echo ${executable:t}
    hyperfine -r 1 -P SIZE $((size/step)) $size -D $((size/step)) "${executable} {SIZE}" --export-json ../../result/json/${executable:t}.json
  done
done
