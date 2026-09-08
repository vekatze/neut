#!/bin/bash

base_dir=$(pwd)

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd)

pids=()

target_directory="$@"

cd $target_directory
echo $(basename $target_directory)
exit_code=0
$NEUT clean
ASAN_OPTIONS=detect_leaks=1 $NEUT build $(basename $target_directory) --report none --execute > /dev/null 2>&1
output=$(ASAN_OPTIONS=detect_leaks=1 $NEUT build $(basename $target_directory) --report none --execute 2>&1 1> actual)
last_exit_code=$?
if [ $last_exit_code -ne 0 ]; then
  printf "\033[1;31merror:\033[0m a test failed: $(basename $target_directory)\n$output\n"
  exit_code=$last_exit_code
elif [ -n "$output" ]; then
  printf "\033[1;31merror:\033[0m found unexpected output on standard error in: $(basename $target_directory)\n$output\n"
  exit_code=1
fi
mismatch=$(diff expected actual 2>&1)
last_exit_code=$?
if [ $last_exit_code -ne 0 ]; then
  printf "\033[1;31merror:\033[0m found an unexpected result in: $(basename $target_directory)\n$mismatch\n"
  exit_code=$last_exit_code
fi
exit $exit_code
