llvm-version := "22.1.8"

llvm-dir := justfile_directory() + "/cache/llvm"

llvm-patch-dir := justfile_directory() + "/build/llvm-patch"

_fetch-llvm:
    #!/usr/bin/env bash
    set -euo pipefail
    shopt -s nullglob
    src={{llvm-dir}}/src
    patch_stamp=$( { cat {{llvm-patch-dir}}/*.patch 2>/dev/null || true; } | cksum | cut -d " " -f 1)
    stamp="{{llvm-version}} $patch_stamp"
    if [ -f "$src/.stamp" ] && [ "$(cat "$src/.stamp")" = "$stamp" ]; then
      exit 0
    fi
    prefix=llvm-project-{{llvm-version}}.src
    archive={{llvm-dir}}/$prefix.tar.xz
    rm -rf "$src"
    mkdir -p "$src"
    if [ ! -f "$archive" ]; then
      rm -f {{llvm-dir}}/llvm-project-*.src.tar.xz
      curl -sSL -o "$archive.tmp" \
        "https://github.com/llvm/llvm-project/releases/download/llvmorg-{{llvm-version}}/$prefix.tar.xz"
      mv "$archive.tmp" "$archive"
    fi
    tar -xJf "$archive" -C "$src" --strip-components=1 \
      "$prefix/llvm" "$prefix/clang" "$prefix/lld" "$prefix/compiler-rt" \
      "$prefix/runtimes" "$prefix/cmake" "$prefix/third-party" \
      "$prefix/libunwind/include" "$prefix/LICENSE.TXT"
    for p in {{llvm-patch-dir}}/*.patch; do
      echo "llvm: applying $(basename "$p")"
      patch -p1 -d "$src" -i "$p"
    done
    printf "%s" "$stamp" > "$src/.stamp"

_build-llvm platform:
    #!/usr/bin/env bash
    set -euo pipefail
    src={{llvm-dir}}/src
    build={{llvm-dir}}/build/{{platform}}
    out={{llvm-dir}}/out/{{platform}}
    case {{platform}} in
      arm64-*) arch=AArch64 ;;
      amd64-*) arch=X86 ;;
      *) echo "unsupported platform: {{platform}}" >&2; exit 1 ;;
    esac
    options=(
      -DCMAKE_BUILD_TYPE=Release
      -DCMAKE_INSTALL_PREFIX="$out"
      -DLLVM_ENABLE_PROJECTS="clang;lld"
      -DLLVM_ENABLE_RUNTIMES=compiler-rt
      -DLLVM_TARGETS_TO_BUILD="$arch;WebAssembly"
      -DLLVM_DISTRIBUTION_COMPONENTS="clang;lld;clang-resource-headers;llvm-ar;llvm-ranlib"
      -DLLVM_ENABLE_ASSERTIONS=OFF
      -DLLVM_INCLUDE_TESTS=OFF
      -DLLVM_INCLUDE_BENCHMARKS=OFF
      -DLLVM_INCLUDE_EXAMPLES=OFF
      -DLLVM_ENABLE_ZLIB=OFF
      -DLLVM_ENABLE_ZSTD=OFF
      -DLLVM_ENABLE_LIBXML2=OFF
      -DLLVM_ENABLE_CURL=OFF
      -DLLVM_ENABLE_LIBEDIT=OFF
      -DLLVM_ENABLE_BINDINGS=OFF
      -DLLVM_ENABLE_PLUGINS=OFF
      -DCLANG_PLUGIN_SUPPORT=OFF
      -DCLANG_ENABLE_STATIC_ANALYZER=OFF
      -DCLANG_ENABLE_OBJC_REWRITER=OFF
      -DCOMPILER_RT_BUILD_SANITIZERS=ON
      -DCOMPILER_RT_SANITIZERS_TO_BUILD=asan
      -DCOMPILER_RT_BUILD_LIBFUZZER=OFF
      -DCOMPILER_RT_BUILD_MEMPROF=OFF
      -DCOMPILER_RT_BUILD_ORC=OFF
      -DCOMPILER_RT_BUILD_PROFILE=OFF
      -DCOMPILER_RT_BUILD_XRAY=OFF
      -DCOMPILER_RT_ENABLE_IOS=OFF
      -DCOMPILER_RT_ENABLE_TVOS=OFF
      -DCOMPILER_RT_ENABLE_WATCHOS=OFF
      -DCOMPILER_RT_ENABLE_XROS=OFF
      -DCOMPILER_RT_ENABLE_MACCATALYST=OFF
    )
    sdk_stamp=""
    case {{platform}} in
      *-darwin)
        sdk_stamp="$(xcrun --show-sdk-path) $(xcrun --show-sdk-build-version 2>/dev/null || echo unknown)"
        ;;
    esac
    patch_stamp=$( { cat {{llvm-patch-dir}}/*.patch 2>/dev/null || true; } | cksum | cut -d " " -f 1)
    config_stamp="{{llvm-version}} $sdk_stamp ${options[*]}"
    stamp="$config_stamp $patch_stamp"
    if [ -f "$out/.stamp" ] && [ "$(cat "$out/.stamp")" = "$stamp" ]; then
      echo "llvm: {{platform}} is up to date"
      exit 0
    fi
    if [ -d "$build" ] && [ "$(cat "$build/.config-stamp" 2>/dev/null || true)" != "$config_stamp" ]; then
      echo "llvm: the configuration of {{platform}} changed; discarding its build directory"
      rm -rf "$build"
    fi
    just _fetch-llvm
    cmake -S "$src/llvm" -B "$build" -G Ninja "${options[@]}"
    printf "%s" "$config_stamp" > "$build/.config-stamp"
    ninja -C "$build" install-distribution-stripped install-compiler-rt-stripped
    test -x "$out/bin/clang"
    test -x "$out/bin/wasm-ld"
    printf "%s" "$stamp" > "$out/.stamp"
