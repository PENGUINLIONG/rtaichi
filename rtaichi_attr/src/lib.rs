use proc_macro2::TokenStream;
use proc_macro_error::{abort, proc_macro_error, ResultExt};
use rtaichi_attr_impl::{kernel::parse_kernel, error::ErrorStore, print::print_kernel, macro_gen::gen_kernel_interface_tt};
use syn::ItemFn;

#[proc_macro_error]
#[proc_macro_attribute]
pub fn kernel(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let attr = TokenStream::from(attr);
    let item = TokenStream::from(item);

    let mut es = ErrorStore::new();
    let i: ItemFn = syn::parse2(item.into()).unwrap_or_abort();

    let kernel = parse_kernel(&mut es, &i);
    for x in es.into_iter() {
        abort!(x);
    }

    let dir = match tempdir::TempDir::new("rtaichi_") {
        Ok(x) => x,
        Err(e) => abort!(attr, "cannot create temp dir: {}", e),
    };
    let base_dir = dir.path().to_string_lossy();

    let python_script = print_kernel(&kernel, &base_dir);
    let python_script_path = format!("{base_dir}/module.py");
    match std::fs::write(&python_script_path, &python_script) {
        Err(e) => abort!(attr, "cannot dump python script to '{}': {}", &python_script_path, e),
        _ => {},
    }

    let mut proc = std::process::Command::new("python")
        .arg(&python_script_path)
        .env("KMP_DUPLICATE_LIB_OK", "TRUE")
        .current_dir(&dir)
        .spawn()
        .unwrap();
    proc.wait().unwrap();

    let aot_module_path = format!("{base_dir}/module.tcm");
    let tcm = match std::fs::read(&aot_module_path) {
        Ok(x) => x,
        Err(e) => abort!(attr, "cannot generate taichi aot module to '{}': {}", aot_module_path, e),
    };

    let expanded = gen_kernel_interface_tt(&kernel, &tcm).unwrap_or_abort();
    expanded.into()
}
