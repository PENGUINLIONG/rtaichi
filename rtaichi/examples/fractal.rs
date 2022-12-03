use rtaichi as ti;
use std::fmt::Write;

fn print_data(pixels: Vec<i32>) {
    let mut out_str = String::new();
    for h in 0..8 {
        for w in 0..16 {
            write!(out_str, "{} ", &pixels[h * 16 + w]).unwrap();
        }
        writeln!(out_str, "").unwrap();
        std::thread::sleep(std::time::Duration::from_millis(16));
    }
    println!("{}", out_str);
}

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
        print_data(arr.to_vec::<i32>().unwrap());
        t += 0.3;
    }
}
