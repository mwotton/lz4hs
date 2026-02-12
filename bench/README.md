# Benchmark prerequisites

The `bench1` benchmark depends on native Snappy headers/libs through the
Haskell `snappy` package.

On Debian/Ubuntu:

```bash
sudo apt-get install -y libsnappy-dev
```

`quicklz` is provided by the Haskell package itself and does not require a
separate system package.
