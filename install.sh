#!/bin/bash

os=$(uname -s)
arch=$(uname -m)

executable_url=""

if [ "$os" = "Darwin" ] && [ "$arch" = "arm64" ]; then
  executable_url="https://github.com/vekatze/neut/releases/latest/download/neut-arm64-darwin"
elif [ "$os" = "Linux" ] && [ "$arch" = "x86_64" ]; then
  executable_url="https://github.com/vekatze/neut/releases/latest/download/neut-amd64-linux"
elif [ "$os" = "Linux" ] && [ "$arch" = "aarch64" ]; then
  executable_url="https://github.com/vekatze/neut/releases/latest/download/neut-arm64-linux"
else
  echo "unsupported platform: $os ($arch)"
  exit 1
fi

HAS_CLANG=0
HAS_CURL=0
HAS_TAR=0
HAS_ZSTD=0

ESC=$(printf '\033')

RED="${ESC}[1;31m%s${ESC}[m"
MAGENTA="${ESC}[1;35m%s${ESC}[m"
BLUE="${ESC}[1;34m%s${ESC}[m"
CYAN="${ESC}[1;36m%s${ESC}[m"

clangs=(
  "clang"
  "clang-15"
  "clang-16"
  "clang-17"
  "clang-18"
  "clang-19"
  "clang-20"
)

for CLANG in "${clangs[@]}"; do
  if command -v $CLANG >/dev/null 2>&1; then
    version=$(echo | $CLANG -dM -E - | grep __clang_major__ | awk '{print $3}')
    if [ $version -ge 15 ]; then
      HAS_CLANG=1
      break
    fi
  fi
done

if command -v curl >/dev/null 2>&1; then
  HAS_CURL=1
fi

if command -v tar >/dev/null 2>&1; then
  HAS_TAR=1
fi

if command -v zstd >/dev/null 2>&1; then
  HAS_ZSTD=1
fi

if [ $HAS_CLANG -eq 0 ]; then
  printf $MAGENTA "warning: "
  echo "\`clang\` (>= 15.0.0) is missing"
fi

if [ $HAS_TAR -eq 0 ]; then
  printf $MAGENTA "warning: "
  echo "\`tar\` is missing"
fi

if [ $HAS_CURL -eq 0 ]; then
  printf $MAGENTA "warning: "
  echo "\`curl\` is missing"
fi

if [ $HAS_ZSTD -eq 0 ]; then
  printf $MAGENTA "warning: "
  echo "\`zstd\` is missing"
fi

if [ $HAS_CLANG -eq 0 ] || [ $HAS_TAR -eq 0 ] || [ $HAS_CURL -eq 0 ] || [ $HAS_ZSTD -eq 0 ]; then
  if command -v apt-get >/dev/null 2>&1; then
    printf $BLUE "note: "
    echo "You can install all the dependencies by:"
    echo "  apt-get install -y --no-install-recommends curl tar zstd lsb-release wget software-properties-common gnupg && bash -c \"\$(wget -O - https://apt.llvm.org/llvm.sh)\" -s 16"
    # packages from llvm.sh can be uninstalled by: apt remove clang-16 lldb-16 lld-16 clangd-16
  elif command -v pacman >/dev/null 2>&1; then
    printf $BLUE "note: "
    echo "You can install all the dependencies by:"
    echo "  pacman -S curl tar zstd clang"
  elif [ "$os" = "Darwin" ] && command -v brew >/dev/null 2>&1; then
    os_version=$(sw_vers -productVersion | awk -F . '{print $1}')
    if [ $os_version -ge 14 ]; then # 14 == Sonoma
      printf $BLUE "note: "
      echo "You can install all the dependencies by:"
      echo "  brew install zstd"
    else
      printf $BLUE "note: "
      echo "You can install all the dependencies by:"
      echo "  brew install llvm zstd"
    fi
  fi
  printf $BLUE "note: "
  echo "Please re-run this script after installing all the dependencies."
  exit 1
fi

printf $BLUE "✓ "
echo "found all the dependencies."

echo ""

target_dir="$HOME/.local/bin"
mkdir -p $target_dir

printf $BLUE "note: "
echo "downloading the compiler..."

echo ""

curl -SL -o $target_dir/neut $executable_url
chmod +x $target_dir/neut

echo ""

printf $BLUE "✓ "
printf "installed the compiler to "
printf $CYAN "$HOME/.local/bin/neut"
echo ""

if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
  echo ""
  printf $MAGENTA "warning: "
  echo "~/.local/bin is not in your \$PATH. "
fi

echo ""

printf $BLUE "note: "
echo "please restart your shell after adding the following environment variables to your shell config:"

echo ""
echo "export NEUT_CORE_MODULE_URL=\"https://github.com/vekatze/neut-core/raw/main/archive/0-51-10.tar.zst\""
echo "export NEUT_CORE_MODULE_DIGEST=\"NV9vKfEBwxCbNW381wEGXsqYkJqFpxBceqhwyaw4oog\""

if command -v apt-get >/dev/null 2>&1; then
  echo "export NEUT_CLANG=$CLANG"
elif [ "$os" = "Darwin" ] && command -v brew >/dev/null 2>&1 && [ $HAS_CLANG -eq 0 ]; then
  echo "export NEUT_CLANG=$(brew --prefix)/opt/llvm/bin/clang-N # N == 15, 16, etc."
fi

echo ""

printf $BLUE "note: "
echo "you can run \`neut version\` to see if the compiler is installed correctly."

echo ""
