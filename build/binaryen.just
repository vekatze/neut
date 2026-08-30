binaryen-version := "132"

binaryen-dir := justfile_directory() + "/cache/binaryen"

_fetch-binaryen platform:
    #!/usr/bin/env bash
    set -euo pipefail
    out={{binaryen-dir}}/{{platform}}
    if [ -x "$out/bin/wasm-opt" ] && [ "$(cat "$out/.stamp" 2>/dev/null)" = "{{binaryen-version}}" ]; then
      exit 0
    fi
    case {{platform}} in
      arm64-darwin) asset=arm64-macos ;;
      arm64-linux) asset=aarch64-linux ;;
      amd64-linux) asset=x86_64-linux ;;
      *) echo "unsupported platform: {{platform}}" >&2; exit 1 ;;
    esac
    name=binaryen-version_{{binaryen-version}}
    rm -rf "$out"
    mkdir -p "$out"
    curl -sSL -o "$out/$name.tar.gz" \
      "https://github.com/WebAssembly/binaryen/releases/download/version_{{binaryen-version}}/$name-$asset.tar.gz"
    tar -xzf "$out/$name.tar.gz" -C "$out" --strip-components=1 "$name/bin/wasm-opt" "$name/lib"
    rm "$out/$name.tar.gz"
    printf "%s" "{{binaryen-version}}" > "$out/.stamp"
