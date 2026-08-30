#!/bin/bash

target_directory="$@"

cd $target_directory
name=$(basename $target_directory)
echo $name
exit_code=0
rm -rf ./cache
$NEUT build "$name-wasm" --report none --execute > /dev/null
output=$($NEUT build "$name-wasm" --report none --execute 2>&1 1> actual-wasm)
last_exit_code=$?
if [ $last_exit_code -ne 0 ]; then
  printf "\033[1;31merror:\033[0m a test failed: $name\n$output\n"
  exit_code=$last_exit_code
fi
mismatch=$(diff expected actual-wasm 2>&1)
last_exit_code=$?
if [ $last_exit_code -ne 0 ]; then
  printf "\033[1;31merror:\033[0m found an unexpected result in: $name\n$mismatch\n"
  exit_code=$last_exit_code
fi
exit $exit_code
