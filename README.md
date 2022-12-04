# rTaichi: Taichi Frontend for Rust

[![Crate](https://img.shields.io/crates/v/rtaichi)](https://crates.io/crates/rtaichi)
[![Documentation](https://docs.rs/rtaichi/badge.svg)](https://docs.rs/rtaichi)

This repository lies the code for the Taichi Hackathon 2022 project "rtaichi".

A project design documentation can be found [here](https://docs.qq.com/pdf/DT1JreFJTTVpha1BZ?u=08133394e0764e43a14513ecd1ef7a89) (Chinese).

## Overview

```rust
use rtaichi as ti;

#[ti::kernel]
fn fractal(t: f32, #[ti(ndim=2)] pixels: NdArray<i32>) {
    for (i, j) in pixels { // Parallelized over all pixels
        let c = [-0.8, ti::cos(t) * 0.2];
        let z = [i / 8 - 1, j / 8 - 0.5] * 2;
        let iterations = 0;
        while z.norm() < 20 && iterations < 50 {
            z = [z[0]*z[0] - z[1]*z[1], z[1] * z[0] * 2] + c;
            iterations += 1;
        }
        pixels[(i, j)] = ti::select(1 - iterations * 0.02 < 0.5, 1, 0);
    }
}

fn main() {
    let runtime = ti::init(ti::Arch::Vulkan).unwrap();
    let arr = runtime.allocate_ndarray::<i32>()
        .host_access(true)
        .shape([16, 8])
        .build()
        .unwrap();

    let mut t = 0.0;
    loop {
        fractal(t, &arr).unwrap();
        t += 0.3;
    }
}
```

This project will utilize Rust's [`procedural macro`](https://doc.rust-lang.org/book/ch19-06-macros.html#procedural-macros-for-generating-code-from-attributes) mechanism to translate the Rust syntax tree of an attributed function into AOT python script. Because the lastest Taichi Runtime (TiRT) C-API supports creating AOT modules from Zip archives, we run the script to generate AOT modules in `.tcm`s and keep them in byte array literals. At runtime we use a singleton runtime to keep AOT module objects and help dispatching the kernels with runtime objects from the [`taichi-runtime`](https://crates.io/crates/taichi-runtime) crate.

The project is subdivided into three parts:
- `rtaichi_attr`: Proc-macro crate to generate Taichi AOT module and to rewrite Rust syntax tree during compilation.
- `rtaichi_attr_impl`: Implemenration of Taichi AOT module generation and Rust syntax tree rewrite, with unit tests.
- `rtaichi`: A meta crate to re-export items in `rtaichi_attr` and `taichi-runtime` with a global thread-local runtime to realize CUDA-like development experience. The only crate for a user to depend on.

Because the translation is done on a syntactic level, you have access to all the math library calls as in Python. Here are some of the equivalent syntactic structures available in rTaichi.

|Description|Taichi (Python)|rTaichi|
|-|-|-|
|Type cast|`ti.i32(x)`|`x as i32`|
|Vector construction|`ti.Vector([1, 2, 3])`|`[1, 2, 3]`|
|Qualified function call|`ti.math.acos(x)`|`ti::math::acos(x)`|
|Member function call|`vec.norm()`|`vec.norm()`|
|ND-array slicing|`arr[i, j]`|`arr[(i, j)]`|

## How it works

The powerful [procedural macro](https://doc.rust-lang.org/reference/procedural-macros.html) allows us to implement compiler plugins in publishable crates to rewrite the token stream of a syntactic item. In this case, we used procedural macro attribute to rewrite the definition of a Rust function. We implemented [a simple stack machine](rtaichi_attr_impl\src\instr.rs) to translate Rust syntax tree into an internal IR. Then, in [`print.rs`](rtaichi_attr_impl\src\print.rs) a temporary Python script is generated to build an AOT module archive. After that, in [`macro_gen.rs`](rtaichi_attr_impl\src\macro_gen.rs) we generate a new Rust function body to launch the kernel. Concretely, it does the following:

1. Lazily create a AOT module in the thread-local `ti::Runtime` singleton.
2. Get the compute graph handle from it.
3. Bind arguments.
4. launch the kernel.
5. Synchronize.

We enforce synchronization at the end of a launch at the moment but, in the future, it will be a feature that you can opt out.

## Installation

To work with rTaichi, you need to build Taichi with the following CMake flags:

- `TI_WITH_C_API=ON`
- `Ti_WITH_VULKAN=ON`

Then, set the following environment variable `TAICHI_C_API_INSTALL_DIR` to the installation directory. For example, if you have your Taichi repository cloned in `C:\Users\PENGUINLIONG\Repositories\taichi\`, your C-API install directory might be `C:\Users\PENGUINLIONG\Repositories\taichi\_skbuild\win-amd64-3.8\cmake-install\c_api`.

You can run the [fractal](rtaichi\examples\fractal.rs) example to verify your installation.

```bash
cargo run --example fractal
```

## Compatibility

Currently we only tested rTaichi on macOS 12 (aarch64) and Windows 10 (x86_64). On Windows `cargo test` somehow hang on `ti_destroy_runtime` and I haven't figure out why at the moment. It seems some resource is not cleaned up in the right order.

## License

Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
