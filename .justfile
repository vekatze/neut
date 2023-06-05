version := "0.3.0.0"

image-amd64 := "neut-haskell-amd64"

image-arm64 := "neut-haskell-arm64"

build-images:
    @just build-image-amd64-linux
    @just build-image-arm64-linux

build-image-amd64-linux:
    @docker build . --platform linux/amd64 -t {{image-amd64}}

build-image-arm64-linux:
    @docker build . --platform linux/arm64 -t {{image-arm64}}

build-compilers:
    @just build-compiler-amd64-linux
    @just build-compiler-arm64-linux
    @just build-compiler-amd64-darwin

build-compiler-amd64-linux:
    @just _run-amd64-linux stack install neut --allow-different-user --local-bin-path ./bin/tmp-amd64-linux
    @just _run-amd64-linux mv ./bin/tmp-amd64-linux/neut ./bin/neut-amd64-linux
    @just _run-amd64-linux rm -r ./bin/tmp-amd64-linux

build-compiler-arm64-linux:
    @just _run-arm64-linux stack install neut --allow-different-user --local-bin-path ./bin/tmp-arm64-linux
    @just _run-arm64-linux mv ./bin/tmp-arm64-linux/neut ./bin/neut-arm64-linux
    @just _run-arm64-linux rm -r ./bin/tmp-arm64-linux

build-compiler-amd64-darwin:
    @stack install neut --allow-different-user --local-bin-path ./bin/tmp-amd64-darwin
    @mv ./bin/tmp-amd64-darwin/neut ./bin/neut-amd64-darwin
    @rm -r ./bin/tmp-amd64-darwin

test-amd64-linux:
    @just _run-amd64-linux NEUT=/app/bin/neut-amd64-linux COMPILER_VERSION={{version}} TARGET_ARCH=amd64 /app/test/test-linux.sh /app/test/term /app/test/statement /app/test/pfds /app/test/misc

test-arm64-linux:
    @just _run-arm64-linux NEUT=/app/bin/neut-arm64-linux COMPILER_VERSION={{version}} TARGET_ARCH=arm64 /app/test/test-linux.sh /app/test/term /app/test/statement /app/test/pfds /app/test/misc

test-amd64-darwin:
    @NEUT={{justfile_directory()}}/bin/neut-amd64-darwin COMPILER_VERSION={{version}} ./test/test-amd64-darwin.sh ./test/term ./test/statement ./test/pfds ./test/misc

_run-amd64-linux *rest:
    @docker run -v $(pwd):/app --platform linux/amd64 --rm {{image-amd64}} sh -c "{{rest}}"

_run-arm64-linux *rest:
    @docker run -v $(pwd):/app --platform linux/arm64 --rm {{image-arm64}} sh -c "{{rest}}"
