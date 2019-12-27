# SourceMod + Rust

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