#!/bin/bash

base_dir=$(pwd)

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd)
clang_option="-fsanitize=address"

cd $SCRIPT_DIR/meta
NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT build --clang-option $clang_option

pids=()

target_directory="$@"

cd $target_directory
echo $(basename $target_directory)
exit_code=0
NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT clean
output=$(ASAN_OPTIONS=detect_leaks=1 NEUT_TARGET_ARCH=$TARGET_ARCH $NEUT build --clang-option $clang_option --execute 2>&1 > actual)
last_exit_code=$?
if [ $last_exit_code -ne 0 ]; then
  printf "\033[1;31merror:\033[0m a test failed: $(basename $target_directory)\n$output\n"
  exit_code=$last_exit_code
fi
mismatch=$(diff expected actual 2>&1)
last_exit_code=$?
if [ $last_exit_code -ne 0 ]; then
  printf "\033[1;31merror:\033[0m found an unexpected result in: $(basename $target_directory)\n$mismatch\n"
  exit_code=$last_exit_code
fi
exit $exit_code
