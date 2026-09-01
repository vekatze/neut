import 'build/llvm.just'
import 'build/wasi.just'
import 'build/binaryen.just'
import 'build/mimalloc.just'

version := "0.18.1"

image-amd64 := "neut-haskell-amd64"

image-arm64 := "neut-haskell-arm64"

toolchain-dir := justfile_directory() + "/cache/toolchain"

build-images:
    @just _build-images-in-parallel amd64-linux arm64-linux

build-toolchain-arm64-darwin:
    @just _build-toolchain arm64-darwin

build-toolchain-arm64-linux:
    @just _run-arm64-linux "just _build-toolchain arm64-linux"

build-toolchain-amd64-linux:
    @just _run-amd64-linux "just _build-toolchain amd64-linux"

build-image-amd64-linux:
    @docker build . -f build/Dockerfile --platform linux/amd64 -t {{image-amd64}}

build-image-arm64-linux:
    @docker build . -f build/Dockerfile --platform linux/arm64 -t {{image-arm64}}

build-compilers:
    @just build-compiler-amd64-linux
    @just build-compiler-arm64-linux
    @just build-compiler-arm64-darwin

build-compiler-amd64-linux:
    @just _run-amd64-linux "just _build-toolchain amd64-linux && just build-mimalloc amd64-linux && just _generate-package-yaml && stack install neut --allow-different-user --local-bin-path ./bin/tmp-amd64-linux"
    @just _run-amd64-linux "mv ./bin/tmp-amd64-linux/neut ./bin/neut-amd64-linux"
    @just _run-amd64-linux "rm -r ./bin/tmp-amd64-linux"

build-compiler-arm64-linux:
    @just _run-arm64-linux "just _build-toolchain arm64-linux && just build-mimalloc arm64-linux && just _generate-package-yaml && stack install neut --allow-different-user --local-bin-path ./bin/tmp-arm64-linux"
    @just _run-arm64-linux "mv ./bin/tmp-arm64-linux/neut ./bin/neut-arm64-linux"
    @just _run-arm64-linux "rm -r ./bin/tmp-arm64-linux"

build-compiler-arm64-darwin:
    @just build-toolchain-arm64-darwin
    @just build-mimalloc arm64-darwin
    @just _build-compiler-darwin arm64

install:
    @just build-toolchain-arm64-darwin
    @just _install-toolchain arm64-darwin
    @just build-mimalloc arm64-darwin
    @just _build-compiler-darwin arm64
    stack install

_install-toolchain platform:
    #!/usr/bin/env bash
    set -euo pipefail
    dest="${XDG_DATA_HOME:-$HOME/.local/share}/neut/toolchain/{{platform}}"
    rm -rf "$dest"
    mkdir -p "$(dirname "$dest")"
    cp -R {{toolchain-dir}}/{{platform}} "$dest"
    echo "installed the toolchain to $dest"

test:
    @just test-amd64-linux
    @just test-arm64-linux
    @just test-arm64-darwin

test-amd64-linux:
    @just _run-amd64-linux "NEUT=/app/bin/neut-amd64-linux NEUT_HOME=/app/cache /app/test/test-linux.sh /app/test/term /app/test/statement /app/test/pfds /app/test/misc"

test-arm64-linux:
    @just _run-arm64-linux "NEUT=/app/bin/neut-arm64-linux NEUT_HOME=/app/cache /app/test/test-linux.sh /app/test/term /app/test/statement /app/test/pfds /app/test/misc"

test-amd64-linux-single target:
    @just _run-amd64-linux "NEUT=/app/bin/neut-amd64-linux NEUT_HOME=/app/cache /app/test/test-linux-single.sh /app/test/{{target}}"

test-arm64-linux-single target:
    @just _run-arm64-linux "NEUT=/app/bin/neut-arm64-linux NEUT_HOME=/app/cache /app/test/test-linux-single.sh /app/test/{{target}}"

test-arm64-darwin:
    @NEUT={{justfile_directory()}}/bin/neut-arm64-darwin NEUT_HOME={{justfile_directory()}}/cache ./test/test-darwin.sh ./test/term ./test/statement ./test/pfds ./test/misc

test-arm64-darwin-single target:
    @NEUT={{justfile_directory()}}/bin/neut-arm64-darwin NEUT_HOME={{justfile_directory()}}/cache ./test/test-darwin-single.sh ./test/{{target}}

test-amd64-linux-wasm:
    @just _run-amd64-linux "NEUT=/app/bin/neut-amd64-linux NEUT_HOME=/app/cache /app/test/test-wasm.sh /app/test/term /app/test/statement /app/test/pfds /app/test/misc"

test-arm64-linux-wasm:
    @just _run-arm64-linux "NEUT=/app/bin/neut-arm64-linux NEUT_HOME=/app/cache /app/test/test-wasm.sh /app/test/term /app/test/statement /app/test/pfds /app/test/misc"

test-arm64-linux-wasm-single target:
    @just _run-arm64-linux "NEUT=/app/bin/neut-arm64-linux NEUT_HOME=/app/cache /app/test/test-wasm-single.sh /app/test/{{target}}"

test-arm64-darwin-wasm:
    @NEUT={{justfile_directory()}}/bin/neut-arm64-darwin NEUT_HOME={{justfile_directory()}}/cache ./test/test-wasm.sh ./test/term ./test/statement ./test/pfds ./test/misc

test-arm64-darwin-wasm-single target:
    @NEUT={{justfile_directory()}}/bin/neut-arm64-darwin NEUT_HOME={{justfile_directory()}}/cache ./test/test-wasm-single.sh ./test/{{target}}

update-core new-version:
    @NEW_VERSION={{new-version}} ./test/update-core.sh ./test/statement ./test/term ./test/misc ./test/pfds

release:
    @echo "creating a release: {{version}}"
    @echo "press any key to proceed..."
    @read -n 1 -s -r -p ""
    @echo "building images..."
    @just build-images
    @echo "building compilers..."
    @just build-compilers
    @echo "testing..."
    @just test
    @echo "uploading..."
    @git checkout main
    @git show-ref --tags {{version}} --quiet || git tag {{version}}
    @git push origin main
    @git push origin {{version}}
    @gh release create {{version}} ./bin/neut-* --latest --generate-notes

_build-toolchain platform:
    #!/usr/bin/env bash
    set -euo pipefail
    just _fetch-wasi
    just _fetch-binaryen {{platform}}
    just _build-llvm {{platform}}
    out={{toolchain-dir}}/{{platform}}
    rm -rf "$out"
    mkdir -p "$out"
    cp -R {{llvm-dir}}/out/{{platform}}/. "$out/"
    cp -R {{binaryen-dir}}/{{platform}}/bin/wasm-opt "$out/bin/"
    cp -R {{binaryen-dir}}/{{platform}}/lib/. "$out/lib/"
    mkdir -p "$out/share/wasi-sysroot/include" "$out/share/wasi-sysroot/lib"
    cp {{wasi-dir}}/wasi-sysroot/VERSION "$out/share/wasi-sysroot/"
    cp -R {{wasi-dir}}/wasi-sysroot/include/wasm32-wasip1 "$out/share/wasi-sysroot/include/"
    cp -R {{wasi-dir}}/wasi-sysroot/lib/wasm32-wasip1 "$out/share/wasi-sysroot/lib/"
    cp -R {{wasi-dir}}/wasi-sysroot/share "$out/share/wasi-sysroot/share"
    cp -R {{wasi-dir}}/builtins/. "$out/lib/clang/$(ls {{llvm-dir}}/out/{{platform}}/lib/clang | head -1)/lib/"
    cp -R {{justfile_directory()}}/licenses "$out/licenses"
    test -x "$out/bin/clang"
    test -x "$out/bin/wasm-ld"
    test -x "$out/bin/wasm-opt"
    test -d "$out/share/wasi-sysroot"

_build-images-in-parallel +args:
    @printf "%s\n" {{args}} | xargs -P 0 -I {} just build-image-{}

_build-compilers-in-parallel +args:
    @printf "%s\n" {{args}} | xargs -P 0 -I {} just build-compiler-{}

_test-in-parallel +args:
    @printf "%s\n" {{args}} | xargs -P 0 -I {} just test-{}

_build-compiler-darwin arch-name:
    @just _generate-package-yaml
    @stack install neut --allow-different-user --local-bin-path ./bin/tmp-{{arch-name}}-darwin
    @mv ./bin/tmp-{{arch-name}}-darwin/neut ./bin/neut-{{arch-name}}-darwin
    @rm -r ./bin/tmp-{{arch-name}}-darwin

_build-native platform:
    @just build-mimalloc {{platform}}
    @just _generate-package-yaml
    @stack install neut --allow-different-user --local-bin-path ./bin/tmp-{{platform}}
    @mv ./bin/tmp-{{platform}}/neut ./bin/neut-{{platform}}
    @rm -r ./bin/tmp-{{platform}}

_generate-package-yaml:
    @sed -e "s/^version: 0$/version: {{version}}/g" ./build/package.template.yaml > ./package.yaml

_run-amd64-linux *rest:
    @docker run -v $(pwd):/app --platform linux/amd64 --rm {{image-amd64}} sh -c "{{rest}}"

_run-arm64-linux *rest:
    @docker run -v $(pwd):/app --platform linux/arm64 --rm {{image-arm64}} sh -c "{{rest}}"
