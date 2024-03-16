# Benchmarks

## Bubble Sort

This test creates a random list of length `N` and bubble-sort the list.

### Linux (AMD64)

![bubble sort](./image/graph/amd64-linux/bubble.png "bubble sort")

### macOS (ARM64)

![bubble sort](./image/graph/arm64-darwin/bubble.png "bubble sort")

## Dictionary (Create & Lookup)

This test creates a random dictionary of size `N` and performs random lookups from the dictionary for `N` times.

### Linux (AMD64)

![dictionary](./image/graph/amd64-linux/dictionary.png "dictionary")

### macOS (ARM64)

![dictionary](./image/graph/arm64-darwin/dictionary.png "dictionary")

## IntMap (Create & Lookup)

This test creates a random intmap of size `N` and performs random lookups from the intmap for `N` times.

### Linux (AMD64)

![IntMap](./image/graph/amd64-linux/intmap.png "IntMap")

### macOS (ARM64)

![IntMap](./image/graph/arm64-darwin/intmap.png "IntMap")

---

## Additional Notes

I used the following hardware in the tests above:

| Platform      | Hardware                         |
| ------------- | -------------------------------- |
| Linux (AMD64) | HP OMEN 16 2023 (7840HS, Ubuntu) |
| macOS (ARM64) | Apple MacBook Pro 14 (M1 Max)    |

I used the following commands to run benchmarks and generate the graphs:

```sh
just bench-linux amd64-linux # Linux (AMD64)
just bench-darwin arm64-darwin # macOS (ARM64)
```

You can find source files in the above benchmarks [here](https://github.com/vekatze/neut/placeholder).

Please tell me (hopefully gently) if some of the results are unfair for a reason I overlooked.
