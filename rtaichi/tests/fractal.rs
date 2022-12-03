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

fn guarded_main() -> ti::Result<()> {
    let runtime = ti::init(ti::Arch::Vulkan)?;
    let arr = runtime.allocate_ndarray::<i32>()
        .host_access(true)
        .shape([16, 8])
        .build()?;

    fractal(1.0, &arr)?;

    let actual = arr.to_vec::<i32>()?;
    let expected = vec![
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,
        0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0,
        0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0,
        0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,
        0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_main() {
    guarded_main().unwrap();
}
