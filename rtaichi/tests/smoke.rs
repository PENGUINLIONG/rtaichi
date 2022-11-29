use rtaichi as ti;

#[ti::kernel]
fn example(a: i32, b: f32, #[ti(ndim=2)] c: NdArray<f32>) -> i32 {
  let x = 1.0 + b;
  c[a] = x;
}
