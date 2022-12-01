use rtaichi::kernel;
use taichi_runtime as ti;

#[kernel]
fn f(a: i32, b: f32, #[ti(ndim=2)] c: NdArray<i32>) {
    let a = 1;
    let b = 2;
    c[(a, a)] = a + b;
}

fn guarded_main() -> ti::Result<()> {
    let runtime = ti::Runtime::new(ti::sys::TiArch::Vulkan)?;
    let arr = runtime.allocate_ndarray::<i32>()
        .host_read(true)
        .host_write(true)
        .shape([4, 4])
        .usage(ti::sys::TiMemoryUsageFlags::STORAGE_BIT)
        .build()?;



    let aot_module = runtime.create_aot_module(tcm)?;

    let mut data = std::iter::repeat(1).take(16).collect::<Vec<_>>();
    println!("{:?}", data);

    let mut cgraph = aot_module.get_compute_graph("g")?;
    cgraph
        .set_arg_i32("a", 1)?
        .set_arg_f32("b", 1.0)?
        .set_arg_ndarray("c", &arr)?
        .launch()?;

    runtime.wait()?;



    arr.read(&mut data)?;
    println!("{:?}", data);

    Ok(())
}

#[test]
fn test_main() {
    guarded_main().unwrap();
    panic!();
}
