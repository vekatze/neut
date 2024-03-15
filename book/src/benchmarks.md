# Benchmarks

Configurations:

| Platform      | Hardware configuration               |
| ------------- | ------------------------------------ |
| Linux (AMD64) | HP OMEN 16 2023 (7840HS, Arch Linux) |
| macOS (ARM64) | Apple MacBook Pro 14 (M1 Max)        |

I generated the graphs on this page using the following command:

```sh
just bench-linux amd64-linux # Linux (AMD64)
just bench-darwin arm64-darwin # macOS (ARM64)
```

Other notes:

- You can find source files in the above benchmarks here.
- Please tell me (hopefully gently) if this comparison is unfair due to a reason that I overlooked.

## Bubble Sort

This test creates a random list of length `N` and bubble-sort the list.

### Linux (AMD64)

![bubble sort](./image/graph/arm64-linux/bubble.png "bubble sort (placeholder)")

### macOS (ARM64)

![bubble sort](./image/graph/arm64-darwin/bubble.png "bubble sort")

## Dictionary (Create & Lookup)

This test creates a random dictionary of size `N` and performs random lookups from the dictionary for `N` times.

### Linux (AMD64)

![dictionary](./image/graph/arm64-linux/dictionary.png "dictionary (placeholder)")

### macOS (ARM64)

![dictionary](./image/graph/arm64-darwin/dictionary.png "dictionary")

## IntMap (Create & Lookup)

This test creates a random intmap of size `N` and performs random lookups from the intmap for `N` times.

### Linux (AMD64)

![IntMap](./image/graph/arm64-linux/intmap.png "IntMap (placeholder)")

### macOS (ARM64)

![IntMap](./image/graph/arm64-darwin/intmap.png "IntMap")
