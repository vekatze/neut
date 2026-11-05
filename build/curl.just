curl-version := "8.23.0"

openssl-version := "3.5.9"

curl-dir := justfile_directory() + "/cache/curl"

_fetch-curl:
    @just _fetch-curl-item curl-{{curl-version}} "https://curl.se/download/curl-{{curl-version}}.tar.gz"
    @just _fetch-curl-item openssl-{{openssl-version}} "https://github.com/openssl/openssl/releases/download/openssl-{{openssl-version}}/openssl-{{openssl-version}}.tar.gz"

_fetch-curl-item name url:
    #!/usr/bin/env bash
    set -euo pipefail
    src={{curl-dir}}/src/{{name}}
    if [ -d "$src" ]; then
      exit 0
    fi
    archive={{curl-dir}}/{{name}}.tar.gz
    mkdir -p {{curl-dir}}/src
    if [ ! -f "$archive" ]; then
      curl -fsSL -o "$archive.tmp" "{{url}}"
      mv "$archive.tmp" "$archive"
    fi
    rm -rf "$src.tmp"
    mkdir -p "$src.tmp"
    tar -xzf "$archive" -C "$src.tmp" --strip-components=1
    mv "$src.tmp" "$src"

_build-curl platform:
    #!/usr/bin/env bash
    set -euo pipefail
    build={{curl-dir}}/build/{{platform}}
    out={{curl-dir}}/out/{{platform}}
    openssl_root="$build/openssl-root"
    openssl_options=(
      no-shared no-module no-dso no-engine no-legacy no-tests no-docs no-apps no-quic
      no-comp no-ssl3 no-dtls no-srp no-psk no-weak-ssl-ciphers no-ui-console -Os
    )
    curl_options=(
      --disable-shared
      --without-zlib --without-brotli --without-zstd --without-nghttp2 --without-ngtcp2
      --without-nghttp3 --without-quiche --without-libpsl --without-libidn2 --without-libssh2
      --without-libssh --without-libgsasl --without-libuv --without-librtmp --without-gssapi
      --disable-ldap --disable-ldaps --disable-ftp --disable-ipfs --disable-rtsp
      --disable-dict --disable-telnet --disable-tftp --disable-pop3 --disable-imap --disable-smtp
      --disable-gopher --disable-mqtt --disable-websockets --disable-smb
      --disable-manual --disable-docs --disable-libcurl-option --disable-ntlm --disable-kerberos-auth
      --disable-negotiate-auth --disable-aws --disable-ech --disable-httpsrr
      --disable-openssl-auto-load-config
      --without-zsh-functions-dir --without-fish-functions-dir
    )
    case {{platform}} in
      arm64-darwin) openssl_target=darwin64-arm64-cc ;;
      arm64-linux) openssl_target=linux-aarch64 ;;
      amd64-linux) openssl_target=linux-x86_64 ;;
      *) echo "unsupported platform: {{platform}}" >&2; exit 1 ;;
    esac
    sdk_stamp=""
    case {{platform}} in
      *-darwin)
        export MACOSX_DEPLOYMENT_TARGET=12.0
        curl_options+=(--with-apple-sectrust)
        cflags="-Os"
        ldflags="-Wl,-dead_strip"
        sdk_stamp="$(xcrun --show-sdk-path) $(xcrun --show-sdk-build-version 2>/dev/null || echo unknown)"
        ;;
      *-linux)
        openssl_options+=(-ffunction-sections -fdata-sections)
        curl_options+=(--without-ca-bundle --without-ca-path)
        cflags="-Os -ffunction-sections -fdata-sections"
        ldflags="-Wl,--gc-sections"
        ;;
    esac
    stamp="{{curl-version}} {{openssl-version}} $sdk_stamp ${openssl_options[*]} ${curl_options[*]} $cflags $ldflags"
    if [ -x "$out/bin/curl" ] && [ "$(cat "$out/.stamp" 2>/dev/null || true)" = "$stamp" ]; then
      echo "curl: {{platform}} is up to date"
      exit 0
    fi
    just _fetch-curl
    jobs=$(getconf _NPROCESSORS_ONLN)
    rm -rf "$build" "$out"
    mkdir -p "$build" "$out/bin"
    cp -R {{curl-dir}}/src/openssl-{{openssl-version}} "$build/openssl"
    (
      cd "$build/openssl"
      ./Configure "$openssl_target" --libdir=lib "${openssl_options[@]}"
      make -j"$jobs" build_sw
      make install_sw DESTDIR="$openssl_root"
    )
    rm -rf "$openssl_root/usr/local/lib/pkgconfig"
    cp -R {{curl-dir}}/src/curl-{{curl-version}} "$build/curl"
    (
      cd "$build/curl"
      ./configure --with-openssl="$openssl_root/usr/local" "${curl_options[@]}" \
        CFLAGS="$cflags" LDFLAGS="$ldflags"
      make -j"$jobs"
    )
    strip -o "$out/bin/curl" "$build/curl/src/curl"
    test -x "$out/bin/curl"
    printf "%s" "$stamp" > "$out/.stamp"
