# Introspection

You can get some states of the compiler by using `introspect`. In particular, you can get the information of the target platform. This information can then be exploited to change the behavior of a function according to the target platform:

```neut
define my-func(): i64 {
  introspect target-os {
  - linux =>
    1
  - darwin =>
    2
  }
}

define my-func-2(): i64 {
  introspect target-arch {
  - arm64 =>
    1
  - amd64 =>
    2
  }
}

define my-func-3(): i64 {
  introspect target-platform {
  - arm64-linux =>
    1
  - arm64-darwin =>
    2
  - amd64-linux =>
    3
  - amd64-darwin =>
    4
  }
}
```

The target platform is, by default, the same as the platform in which the compiler is executed. You can change it using the following two environment variables:

- `NEUT_TARGET_ARCH`
- `NEUT_TARGET_OS`
