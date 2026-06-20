#!/bin/bash

base_dir=$(pwd)

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd)

pids=()
exit_code=0

detect_job_count() {
  if command -v nproc > /dev/null 2>&1; then
    nproc
  elif command -v getconf > /dev/null 2>&1; then
    getconf _NPROCESSORS_ONLN
  else
    echo 1
  fi
}

max_jobs=${NEUT_TEST_JOBS:-$(detect_job_count)}

if ! [[ $max_jobs =~ ^[1-9][0-9]*$ ]]; then
  echo "NEUT_TEST_JOBS must be a positive integer"
  exit 1
fi

wait_one() {
  pid=${pids[0]}
  pids=("${pids[@]:1}")
  wait $pid
  result=$?
  if [ $result -ne 0 ]; then
    exit_code=$result
  fi
}

for target_dir in "$@"; do
  cd $base_dir
  cd $target_dir

  for i in $(find . -maxdepth 1 -mindepth 1 -type d | sort); do
    cd $i
    echo $(basename $i)
    (
      exit_code=0
      rm -rf ./cache
      NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT clean
      ASAN_OPTIONS=detect_leaks=1 NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT build $(basename $i) --execute 2>&1 > /dev/null
      output=$(ASAN_OPTIONS=detect_leaks=1 NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT build $(basename $i) --execute 2>&1 > actual)
      last_exit_code=$?
      if [ $last_exit_code -ne 0 ]; then
        printf "\033[1;31merror:\033[0m a test failed: $(basename $i)\n$output\n"
        exit_code=$last_exit_code
      fi
      mismatch=$(diff expected actual 2>&1)
      last_exit_code=$?
      if [ $last_exit_code -ne 0 ]; then
        printf "\033[1;31merror:\033[0m found an unexpected result in: $(basename $i)\n$mismatch\n"
        exit_code=$last_exit_code
      fi
      exit $exit_code
    ) &
    pids+=($!)
    while [ ${#pids[@]} -ge $max_jobs ]; do
      wait_one
    done
    cd ..
  done
done

while [ ${#pids[@]} -gt 0 ]; do
  wait_one
done

exit $exit_code
