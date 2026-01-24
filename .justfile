set dotenv-load

image-amd64 := "neut-haskell-amd64"

image-arm64 := "neut-haskell-arm64"

build-images:
    @just _build-images-in-parallel amd64-linux arm64-linux

build-image-amd64-linux:
    @docker build . -f build/Dockerfile --platform linux/amd64 -t {{image-amd64}}

build-image-arm64-linux:
    @docker build . -f build/Dockerfile --platform linux/arm64 -t {{image-arm64}}

build-compilers:
    @just build-compiler-amd64-linux
    @just build-compiler-arm64-linux
    @just build-compiler-arm64-darwin

build-compiler-amd64-linux:
    @just _run-amd64-linux "just _generate-package-yaml && stack install neut --allow-different-user --local-bin-path ./bin/tmp-amd64-linux"
    @just _run-amd64-linux "mv ./bin/tmp-amd64-linux/neut ./bin/neut-amd64-linux"
    @just _run-amd64-linux "rm -r ./bin/tmp-amd64-linux"

build-compiler-arm64-linux:
    @just _run-arm64-linux "just _generate-package-yaml && stack install neut --allow-different-user --local-bin-path ./bin/tmp-arm64-linux"
    @just _run-arm64-linux "mv ./bin/tmp-arm64-linux/neut ./bin/neut-arm64-linux"
    @just _run-arm64-linux "rm -r ./bin/tmp-arm64-linux"

build-compiler-arm64-darwin:
    @just _build-compiler-darwin arm64

bench-arm64-darwin:
    @just build-compiler-arm64-darwin
    @NEUT={{justfile_directory()}}/bin/neut-arm64-darwin PLATFORM=arm64-darwin {{justfile_directory()}}/bench/script/bench-darwin.sh
    @echo "\nGenerating graphs..."
    @sh -c "cd {{justfile_directory()}}/bench/script/render && rm -rf node_modules && npm install && ./node_modules/.bin/ts-node ./main.ts arm64-darwin"

bench-darwin platform:
    @just _build-native {{platform}}
    @NEUT={{justfile_directory()}}/bin/neut-{{platform}} PLATFORM={{platform}} {{justfile_directory()}}/bench/script/bench-darwin.sh
    @echo "\nGenerating graphs..."
    @sh -c "cd {{justfile_directory()}}/bench/script/render && rm -rf node_modules && npm install && ./node_modules/.bin/ts-node ./main.ts {{platform}}"

bench-linux platform: # platform \in {amd64-linux, arm64-linux}
    @just _build-native {{platform}}
    @NEUT={{justfile_directory()}}/bin/neut-{{platform}} PLATFORM={{platform}} {{justfile_directory()}}/bench/script/bench-linux.sh
    @echo "\nGenerating graphs..."
    @sh -c "cd {{justfile_directory()}}/bench/script/render && rm -rf node_modules && npm install && ./node_modules/.bin/ts-node ./main.ts {{platform}}"

test:
    @just test-amd64-linux
    @just test-arm64-linux
    @just test-arm64-darwin

test-amd64-linux:
    @just _run-amd64-linux "NEUT=/app/bin/neut-amd64-linux TARGET_ARCH=amd64 /app/test/test-linux.sh /app/test/term /app/test/statement /app/test/pfds /app/test/misc"

test-arm64-linux:
    @just _run-arm64-linux "NEUT=/app/bin/neut-arm64-linux TARGET_ARCH=arm64 /app/test/test-linux.sh /app/test/term /app/test/statement /app/test/pfds /app/test/misc"

test-arm64-linux-single target:
    @just _run-arm64-linux "NEUT=/app/bin/neut-arm64-linux TARGET_ARCH=arm64 /app/test/test-linux-single.sh /app/test/{{target}}"

test-arm64-darwin:
    @NEUT={{justfile_directory()}}/bin/neut-arm64-darwin TARGET_ARCH=arm64 CLANG_PATH=${NEUT_ARM64_DARWIN_CLANG_PATH} ./test/test-darwin.sh ./test/term ./test/statement ./test/pfds ./test/misc

test-bench-darwin:
    @NEUT={{justfile_directory()}}/bin/neut-arm64-darwin PLATFORM=arm64-darwin {{justfile_directory()}}/bench/script/bench-darwin-mini.sh

update-core new-version:
    @NEW_VERSION={{new-version}} ./test/update-core.sh ./test/statement ./test/term ./test/misc ./test/pfds ./bench/action

release:
    @echo "creating a release: $VERSION"
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
    @git show-ref --tags $VERSION --quiet || git tag $VERSION
    @git push origin main
    @git push origin $VERSION
    @gh release create $VERSION ./bin/neut-* --latest --generate-notes

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
    @just _generate-package-yaml
    @stack install neut --allow-different-user --local-bin-path ./bin/tmp-{{platform}}
    @mv ./bin/tmp-{{platform}}/neut ./bin/neut-{{platform}}
    @rm -r ./bin/tmp-{{platform}}

_generate-package-yaml:
    @sed "s/^version: 0$/version: $VERSION/g" ./build/package.template.yaml > ./package.yaml

_run-amd64-linux *rest:
    @docker run -v $(pwd):/app --platform linux/amd64 --rm {{image-amd64}} sh -c "{{rest}}"

_run-arm64-linux *rest:
    @docker run -v $(pwd):/app --platform linux/arm64 --rm {{image-arm64}} sh -c "{{rest}}"
