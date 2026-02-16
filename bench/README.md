# Local CI-equivalent prerequisites

Native tools are required for the CI-equivalent local workflow:

- Library builds link against system `liblz4` and require headers (and `pkg-config`).
- `bench1` needs Snappy headers/libs through the Haskell `snappy` package.
- `oracle` tests shell out to the `lz4` CLI.

On Debian/Ubuntu:

```bash
sudo apt-get install -y liblz4-dev pkg-config libsnappy-dev lz4
```

`quicklz` is provided by the Haskell package itself and does not require a
separate system package.

To mirror the CI gates locally:

```bash
cabal test properties
MINITHESIS_NO_DATABASE=1 MINITHESIS_MAX_EXAMPLES=40 cabal test oracle
cabal build --enable-benchmarks lz4:bench:bench1
```
