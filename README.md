# Fast compression for Haskell ByteStrings

This library implements Haskell bindings to [LZ4][], a fast
compression library. It links against the system `liblz4` (including
`lz4frame`), so builds require the LZ4 headers and library to be
installed (for example `liblz4-dev` on Debian/Ubuntu, or `lz4` via
Homebrew).

## Formats

- `Codec.Compression.LZ4.compress` / `decompress` keep the historical
  length-prefixed format used by this package.
- `Codec.Compression.LZ4.compressFrame` / `decompressFrame` provide a
  separate framed API for compatibility with standard `lz4` frame files.
- `compressFrameEither` / `decompressFrameEither` provide the stable
  typed framed error contract for encode/decode, while
  `compressFrame` / `decompressFrame` remain backward-compatible
  `Maybe` wrappers.
- `decompressFrameBounded` / `decompressFrameBoundedFrom` provide a
  bounded framed decode path that yields partial output with an explicit
  continuation offset when a caller-supplied output limit is reached.
- Legacy and framed formats are intentionally separate: legacy payloads
  are decoded only by `decompress`, while framed payloads are decoded
  only by `decompressFrame`.
- The framed API uses native `LZ4F` bindings and does not shell out to
  the `lz4` executable at runtime.

## CI expectations

GitHub Actions CI runs:

- `cabal test properties`
- `cabal test oracle` (requires the `lz4` CLI in `PATH`)
- `cabal build --enable-benchmarks lz4:bench:bench1`

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install lz4
```

# Join in

File bugs in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/mwotton/lz4hs.git`

# Authors

See `AUTHORS.txt`.

# License.

BSD3. See `LICENSE.txt` for terms of copyright and redistribution.

[LZ4]: https://github.com/lz4/lz4
[issue tracker]: https://github.com/mwotton/lz4hs/issues
[gh]: https://github.com/mwotton/lz4hs
[Hackage]: https://hackage.haskell.org/package/lz4
