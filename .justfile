version := "0.2.0.0"

image-amd64 := "neut-haskell-amd64"

image-arm64 := "neut-haskell-arm64"

build-images:
    @docker build . --platform linux/amd64 -t {{image-amd64}}
    @docker build . --platform linux/arm64 -t {{image-arm64}}

build-compilers:
    @just _run-amd64 stack install neut --local-bin-path ./bin/amd64-linux
    @just _run-arm64 stack install neut --local-bin-path ./bin/arm64-linux
    @stack install neut --local-bin-path ./bin/amd64-darwin

test-darwin:
    @cd ./test && COMPILER_VERSION={{version}} ./test-darwin.sh ./data
    @cd ./test && COMPILER_VERSION={{version}} ./test-darwin.sh ./pfds

test-linux-arm64:
    just _run-arm64 NEUT=/app/bin/arm64-linux/neut COMPILER_VERSION={{version}} TARGET_ARCH=arm64 /app/test/test-linux.sh /app/test/data
    just _run-arm64 NEUT=/app/bin/arm64-linux/neut COMPILER_VERSION={{version}} TARGET_ARCH=arm64 /app/test/test-linux.sh /app/test/pfds

test-linux-amd64:
    just _run-amd64 NEUT=/app/bin/amd64-linux/neut COMPILER_VERSION={{version}} TARGET_ARCH=amd64 /app/test/test-linux.sh /app/test/data
    just _run-amd64 NEUT=/app/bin/amd64-linux/neut COMPILER_VERSION={{version}} TARGET_ARCH=amd64 /app/test/test-linux.sh /app/test/pfds

_run-amd64 *rest:
    docker run -v $(pwd):/app --platform linux/amd64 --rm {{image-amd64}} sh -c "{{rest}}"

_run-arm64 *rest:
    docker run -v $(pwd):/app --platform linux/arm64 --rm {{image-arm64}} sh -c "{{rest}}"
