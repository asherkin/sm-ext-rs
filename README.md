# SourceMod + Rust

FFI wrappers and helpers for writing SourceMod extensions in Rust.

## Building

* Documentation
  ```
  cargo +nightly doc --no-deps
  ```

* Windows Debug Build
  ```
  RUSTFLAGS="-C target-feature=+crt-static" cargo build --all --all-targets --target=i686-pc-windows-msvc
  ```

* Windows Release Build
  ```
  RUSTFLAGS="-C target-feature=+crt-static -C codegen-units=1" cargo build --all --all-targets --target=i686-pc-windows-msvc --release
  ```

* Linux Debug Build
  ```
  cargo build --all --all-targets --target=i686-unknown-linux-gnu
  ```

* Linux Release Build
  ```
  cargo build --all --all-targets --target=i686-unknown-linux-gnu --release
  ```

You'll probably want to wrap something around the cargo build to rename the output binary to match SourceMod's convention, and on Linux you'll want to strip release binaries before distribution with `strip --strip-debug`.
