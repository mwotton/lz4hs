# Changelog

## 0.3.0

- Breaking (build): this package no longer vendors the upstream LZ4 C sources.
  Builds now link against the system `liblz4` (including `lz4frame`) via `pkg-config`.
  Install `liblz4-dev` and `pkg-config` (Debian/Ubuntu) or `lz4` (Homebrew).
