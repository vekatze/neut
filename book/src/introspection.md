# Introspection

You can get some states of the compiler by using `introspect`. Currently, you can get the information about the target platform. This information can then be exploited to change the behavior of a function according to the target platform:

```neut
define os-dependent-constant(): int {
  introspect target-os {
  - linux =>
    1
  - darwin =>
    2
  }
}

define arch-dependent-constant(): int {
  introspect target-arch {
  - arm64 =>
    1
  - amd64 =>
    2
  }
}

define platform-dependent-constant(): int {
  introspect target-platform {
  - arm64-linux =>
    1
  - arm64-darwin =>
    2
  - amd64-linux =>
    3
  }
}
```

The target platform is, by default, the same as the platform in which the compiler is executed. You can change it using the following two environment variables:

- `NEUT_TARGET_ARCH`
- `NEUT_TARGET_OS`
