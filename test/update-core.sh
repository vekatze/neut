#!/bin/zsh

base_dir=$(pwd)

pids=()

for target_dir in "$@"; do
  cd $base_dir
  cd $target_dir

  for i in $(find . -d 1 -type d | sort); do
    cd $i
    (
      echo $(basename $i)
      neut get core https://github.com/vekatze/neut-core/raw/main/archive/${NEW_VERSION}.tar.zst
    ) &
    pids+=($!)
    cd ..
  done
done

exit_code=0

for pid in $pids; do
  wait $pid
  result=$?
  if [ $result -ne 0 ]; then
    exit_code=$result
  fi
done

exit $exit_code
