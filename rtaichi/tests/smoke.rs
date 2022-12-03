use rtaichi as ti;

#[ti::kernel]
fn f(a: i32, b: f32, #[ti(ndim=2)] c: NdArray<i32>) {
    let a = 1;
    let b = 2;
    c[(a, a)] = a + b;
}

fn guarded_main() -> ti::Result<()> {
    let runtime = ti::init(ti::Arch::Vulkan)?;
    let arr = runtime.allocate_ndarray::<i32>()
        .host_access(true)
        .shape([3, 3])
        .build()?;

    f(1, 1.0, &arr)?;
    
    let actual = arr.to_vec::<i32>()?;
    let expected = vec![0, 0, 0, 0, 3, 0, 0, 0, 0];
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_main() {
    guarded_main().unwrap();
}
