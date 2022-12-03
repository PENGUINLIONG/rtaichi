# rtaichi: Taichi Frontend for Rust

This repository lies the code for the Taichi Hackathon 2022 project "rtaichi".

A project design documentation can be found [here](https://docs.qq.com/pdf/DT1JreFJTTVpha1BZ?u=08133394e0764e43a14513ecd1ef7a89) (Chinese).

## Overview

This project will utilize Rust's [`procedural macro`](https://doc.rust-lang.org/book/ch19-06-macros.html#procedural-macros-for-generating-code-from-attributes) mechanism to translate the Rust syntax tree of an attributed function into AOT python script. Because the lastest Taichi Runtime C-API supports creating AOT modules from Zip archives, we run the script to generate AOT modules in `.tcm`s and keep them in byte array literals. At runtime we use a singleton runtime to keep AOT module objects and help dispatching the kernels with runtime objects from the [`taichi-runtime`](https://crates.io/crates/taichi-runtime) crate.

## License

Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
