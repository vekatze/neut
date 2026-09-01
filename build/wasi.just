wasi-sdk-version := "34.0"

wasi-dir := justfile_directory() + "/cache/wasi"

_fetch-wasi:
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p {{wasi-dir}}
    just _fetch-wasi-item wasi-sysroot wasi-sysroot-{{wasi-sdk-version}}.tar.gz
    just _fetch-wasi-item builtins libclang_rt-{{wasi-sdk-version}}.tar.gz

_fetch-wasi-item name asset:
    #!/usr/bin/env bash
    set -euo pipefail
    dest={{wasi-dir}}/{{name}}
    if [ -d "$dest" ]; then
      exit 0
    fi
    tmp={{wasi-dir}}/.tmp-{{name}}
    rm -rf "$tmp"
    mkdir -p "$tmp/out"
    curl -sSL -o "$tmp/{{asset}}" \
      "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-{{ replace(wasi-sdk-version, ".0", "") }}/{{asset}}"
    tar -xzf "$tmp/{{asset}}" -C "$tmp/out" --strip-components=1
    mv "$tmp/out" "$dest"
    rm -rf "$tmp"
