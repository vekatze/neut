#!/bin/bash


base_dir=$(pwd)

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

# These reach `core::sync`, and wasi has no threads. They are not skipped: the
# lane asserts that each one is rejected at build time, which is the whole
# point of declaring the capability.
unbuildable=(
  flow
  mutable
  noema
)

is_unbuildable() {
  local candidate
  for candidate in "${unbuildable[@]}"; do
    if [ "$candidate" = "$1" ]; then
      return 0
    fi
  done
  return 1
}

echo "wasm32 lane: ${#unbuildable[@]} test(s) must fail to build, for want of a capability wasi does not have:"
for unbuildable_name in "${unbuildable[@]}"; do
  echo "  $unbuildable_name"
done
echo

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
    name=$(basename $i)
    echo $name
    (
      exit_code=0
      rm -rf ./cache
      if is_unbuildable $name; then
        output=$($NEUT build "$name-wasm" --report none 2>&1)
        if [ $? -eq 0 ]; then
          printf "\033[1;31merror:\033[0m expected a capability error, but the build succeeded: $name\n"
          exit 1
        fi
        if ! printf "%s" "$output" | grep -q "is not available on wasm32"; then
          printf "\033[1;31merror:\033[0m expected a capability error in: $name\n$output\n"
          exit 1
        fi
        exit 0
      fi
      $NEUT build "$name-wasm" --report none --execute > /dev/null 2>&1
      output=$($NEUT build "$name-wasm" --report none --execute 2>&1 1> actual-wasm)
      last_exit_code=$?
      if [ $last_exit_code -ne 0 ]; then
        printf "\033[1;31merror:\033[0m a test failed: $name\n$output\n"
        exit_code=$last_exit_code
      elif [ -n "$output" ]; then
        printf "\033[1;31merror:\033[0m found unexpected output on standard error in: $name\n$output\n"
        exit_code=1
      fi
      mismatch=$(diff expected actual-wasm 2>&1)
      last_exit_code=$?
      if [ $last_exit_code -ne 0 ]; then
        printf "\033[1;31merror:\033[0m found an unexpected result in: $name\n$mismatch\n"
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
