use proc_macro::{TokenStream};
use proc_macro2::Literal;
use proc_macro_error::{abort, proc_macro_error, ResultExt};
use quote::quote;
use rtaichi_impl::{kernel::parse_kernel, error::ErrorStore, print::print_kernel};
use syn::{ItemFn, Lit};

#[proc_macro_error]
#[proc_macro_attribute]
pub fn kernel(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut es = ErrorStore::new();
    let i: ItemFn = syn::parse2(item.into()).unwrap_or_abort();

    let kernel = parse_kernel(&mut es, &i);
    for x in es.into_iter() {
        abort!(x);
    }

    let dir = tempdir::TempDir::new("rtaichi_").unwrap();
    let base_dir = dir.path().to_string_lossy();

    let python_script = print_kernel(kernel, &base_dir);
    let python_script_path = format!("{base_dir}/module.py");
    std::fs::write(python_script_path, python_script).unwrap();

    let mut proc = std::process::Command::new("python")
        .arg("module.py")
        .current_dir(&dir)
        .spawn()
        .unwrap();
    proc.wait().unwrap();

    let data = std::fs::read(format!("{base_dir}/module.tcm")).unwrap();

    let bytes = Literal::byte_string(&data);
    let bytes_lit = Lit::Verbatim(bytes);

    let expended = quote! {
        const tcm: &'static [u8] = #bytes_lit;
    };

    expended.into()
}
