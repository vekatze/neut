mimalloc-version := "v3.4.3"

mimalloc-url := "https://github.com/microsoft/mimalloc.git"

mimalloc-src-dir := justfile_directory() + "/cache/mimalloc/src"

mimalloc-build-dir := justfile_directory() + "/cache/mimalloc/build"

mimalloc-out-dir := justfile_directory() + "/cache/mimalloc/out"

build-mimalloc platform:
    @just _fetch-mimalloc
    @just _build-mimalloc {{platform}} host
    @just _build-mimalloc {{platform}} wasm32

_fetch-mimalloc:
    #!/usr/bin/env bash
    set -euo pipefail
    src={{mimalloc-src-dir}}
    if [ -f "$src/.stamp" ] && [ "$(cat "$src/.stamp")" = "{{mimalloc-version}}" ]; then
      exit 0
    fi
    rm -rf "$src"
    mkdir -p "$(dirname "$src")"
    git clone --branch {{mimalloc-version}} --depth 1 {{mimalloc-url}} "$src"
    printf "%s" "{{mimalloc-version}}" > "$src/.stamp"

_build-mimalloc platform kind:
    #!/usr/bin/env bash
    set -euo pipefail
    build={{mimalloc-build-dir}}/{{platform}}/{{kind}}
    options=(
      -DCMAKE_BUILD_TYPE=Release
      -DMI_BUILD_SHARED=OFF
      -DMI_BUILD_STATIC=ON
      -DMI_BUILD_OBJECT=OFF
      -DMI_BUILD_TESTS=OFF
    )
    case {{kind}} in
      host)
        out={{mimalloc-out-dir}}/release
        ;;
      wasm32)
        out={{mimalloc-out-dir}}/wasm32
        toolchain={{toolchain-dir}}/{{platform}}
        options+=(
          -DCMAKE_SYSTEM_NAME=WASI
          -DCMAKE_SYSTEM_VERSION=1
          -DCMAKE_SYSTEM_PROCESSOR=wasm32
          -DCMAKE_C_COMPILER="$toolchain/bin/clang"
          -DCMAKE_C_COMPILER_TARGET=wasm32-unknown-wasip1
          -DCMAKE_C_COMPILER_WORKS=1
          -DCMAKE_AR="$toolchain/bin/llvm-ar"
          -DCMAKE_RANLIB="$toolchain/bin/llvm-ranlib"
          -DCMAKE_C_FLAGS="--sysroot=$toolchain/share/wasi-sysroot"
        )
        ;;
      *) echo "unsupported kind: {{kind}}" >&2; exit 1 ;;
    esac
    stamp="{{mimalloc-version}} {{llvm-version}} {{wasi-sdk-version}} ${options[*]}"
    if [ ! -f "$build/.stamp" ] || [ "$(cat "$build/.stamp")" != "$stamp" ]; then
      cmake -S {{mimalloc-src-dir}} -B "$build" "${options[@]}"
      cmake --build "$build" --target mimalloc-static
      printf "%s" "$stamp" > "$build/.stamp"
    fi
    mkdir -p "$out"
    if ! cmp -s "$build/libmimalloc.a" "$out/libmimalloc.a"; then
      cp "$build/libmimalloc.a" "$out/libmimalloc.a"
    fi
    test -f "$out/libmimalloc.a"
