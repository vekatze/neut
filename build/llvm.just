llvm-version := "22.1.8"

llvm-dir := justfile_directory() + "/cache/llvm"

_fetch-llvm:
    #!/usr/bin/env bash
    set -euo pipefail
    src={{llvm-dir}}/src
    if [ -f "$src/.stamp" ] && [ "$(cat "$src/.stamp")" = "{{llvm-version}}" ]; then
      exit 0
    fi
    prefix=llvm-project-{{llvm-version}}.src
    archive={{llvm-dir}}/$prefix.tar.xz
    rm -rf "$src"
    mkdir -p "$src"
    curl -sSL -o "$archive" \
      "https://github.com/llvm/llvm-project/releases/download/llvmorg-{{llvm-version}}/$prefix.tar.xz"
    tar -xJf "$archive" -C "$src" --strip-components=1 \
      "$prefix/llvm" "$prefix/clang" "$prefix/lld" "$prefix/compiler-rt" \
      "$prefix/runtimes" "$prefix/cmake" "$prefix/third-party" \
      "$prefix/libunwind/include" "$prefix/LICENSE.TXT"
    rm "$archive"
    printf "%s" "{{llvm-version}}" > "$src/.stamp"

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
      -DLLVM_ENABLE_TERMINFO=OFF
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
    stamp="{{llvm-version}} ${options[*]}"
    if [ -f "$out/.stamp" ] && [ "$(cat "$out/.stamp")" = "$stamp" ]; then
      echo "llvm: {{platform}} is up to date"
      exit 0
    fi
    just _fetch-llvm
    cmake -S "$src/llvm" -B "$build" -G Ninja "${options[@]}"
    ninja -C "$build" install-distribution-stripped install-compiler-rt-stripped
    test -x "$out/bin/clang"
    test -x "$out/bin/wasm-ld"
    printf "%s" "$stamp" > "$out/.stamp"
