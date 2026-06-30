#!/bin/zsh

setopt no_bg_nice

base_dir=$(pwd)

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd)
LSAN_FILE=$SCRIPT_DIR/lsan.supp

pids=()
exit_code=0
max_jobs=${NEUT_TEST_JOBS:-0}

if ! [[ $max_jobs =~ '^[0-9]+$' ]]; then
  echo "NEUT_TEST_JOBS must be a non-negative integer"
  exit 1
fi

wait_one() {
  pid=$pids[1]
  shift pids
  wait $pid
  result=$?
  if [ $result -ne 0 ]; then
    exit_code=$result
  fi
}

for target_dir in "$@"; do
  cd $base_dir
  cd $target_dir

  for i in $(find . -d 1 -type d | sort); do
    cd $i
    echo $(basename $i)
    (
      exit_code=0
      NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT clean
      LSAN_OPTIONS=suppressions=$LSAN_FILE NEUT_TARGET_ARCH=$TARGET_ARCH NEUT_CLANG=$CLANG_PATH $NEUT build $(basename $i) --report none --execute > /dev/null
      output=$(LSAN_OPTIONS=suppressions=$LSAN_FILE NEUT_TARGET_ARCH=$TARGET_ARCH NEUT_CLANG=$CLANG_PATH $NEUT build $(basename $i) --report none --execute 2>&1 1> actual)
      last_exit_code=$?
      if [ $last_exit_code -ne 0 ]; then
        echo "\033[1;31merror:\033[0m a test failed: $(basename $i)\n$output"
        exit_code=$last_exit_code
      fi
      mismatch=$(diff expected actual 2>&1)
      last_exit_code=$?
      if [ $last_exit_code -ne 0 ]; then
        echo "\033[1;31merror:\033[0m found an unexpected result in: $(basename $i)\n$mismatch"
        exit_code=$last_exit_code
      fi
      exit $exit_code
    ) &
    pids+=($!)
    if [ $max_jobs -gt 0 ]; then
      while [ ${#pids[@]} -ge $max_jobs ]; do
        wait_one
      done
    fi
    cd ..
  done
done

while [ ${#pids[@]} -gt 0 ]; do
  wait_one
done

exit $exit_code
