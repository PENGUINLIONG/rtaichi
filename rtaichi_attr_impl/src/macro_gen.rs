use proc_macro2::{TokenStream, Ident, Span, Literal};
use quote::quote;
use syn::{Lit, LitStr};
use taichi_runtime as ti;
use crate::{kernel::Kernel, instr::Operand, Result, arg_ty::KernelArgType};

fn get_dtype_tt(dtype: &ti::DataType) -> TokenStream {
    match dtype {
        //TiDataType::F16 => quote! { f16 },
        ti::DataType::F32 => quote! { f32 },
        ti::DataType::F64 => quote! { f64 },
        ti::DataType::I8 => quote! { i8 },
        ti::DataType::I16 => quote! { i16 },
        ti::DataType::I32 => quote! { i32 },
        ti::DataType::I64 => quote! { i64 },
        ti::DataType::U8 => quote! { u8 },
        ti::DataType::U16 => quote! { u16 },
        ti::DataType::U32 => quote! { u32 },
        ti::DataType::U64 => quote! { u64 },
        _ => panic!("'{dtype:?}' is not a valid data type"),
    }
}

fn gen_kernel_arg_list_tt(kernel: &Kernel) -> Result<TokenStream> {
    let mut arg_list_tt = TokenStream::new();
    for instr in kernel.instrs.iter() {
        match &instr.operand {
            Operand::Arg { name, ty } => {
                let name = Ident::new(&name, Span::call_site());
                match ty {
                    KernelArgType::Scalar { dtype } => {
                        let dtype_tt = get_dtype_tt(dtype);
                        let tt = quote! {
                            #name: #dtype_tt,
                        };
                        arg_list_tt.extend(tt);
                    },
                    KernelArgType::NdArray { dtype, .. } => {
                        let dtype_tt = get_dtype_tt(dtype);
                        let tt = quote! {
                            #name: &::rtaichi::NdArray<#dtype_tt>,
                        };
                        arg_list_tt.extend(tt);
                    },
                    _ => panic!("unexpected kernel arg type: {ty:?}"),
                }
            },
            _ => {},
        }
    }
    Ok(arg_list_tt)
}

fn gen_kernel_arg_setter_list_tt(kernel: &Kernel) -> Result<TokenStream> {
    let mut arg_setter_lit_tt = TokenStream::new();
    for instr in kernel.instrs.iter() {
        match &instr.operand {
            Operand::Arg { name, ty } => {
                let name_ident = Ident::new(&name, Span::call_site());
                let name_lit = LitStr::new(name, Span::call_site());
                match ty {
                    KernelArgType::Scalar { dtype } => {
                        let tt = match dtype {
                            ti::DataType::F32 => quote! {
                                cgraph.set_arg_f32(#name_lit, #name_ident)?;
                            },
                            ti::DataType::I32 => quote! {
                                cgraph.set_arg_i32(#name_lit, #name_ident)?;
                            },
                            _ => panic!("unsupported scalar type"),
                        };
                        arg_setter_lit_tt.extend(tt);
                    },
                    KernelArgType::NdArray { .. } => {
                        let tt = quote! {
                            cgraph.set_arg_ndarray(#name_lit, #name_ident)?;
                        };
                        arg_setter_lit_tt.extend(tt);
                    },
                    _ => panic!("unexpected kernel arg type: {ty:?}"),
                }
            },
            _ => {},
        }
    }
    Ok(arg_setter_lit_tt)
}

pub fn gen_kernel_interface_tt(kernel: &Kernel, tcm: &[u8]) -> Result<TokenStream> {
    let arg_list_tt = gen_kernel_arg_list_tt(kernel)?;
    let arg_setter_list_tt = gen_kernel_arg_setter_list_tt(kernel)?;

    let bytes = Literal::byte_string(&tcm);
    let bytes_lit = Lit::Verbatim(bytes);

    let fn_name = Ident::new(&kernel.name, Span::call_site());
    let expanded = quote! {
        fn #fn_name(#arg_list_tt) -> ::rtaichi::Result<()> {
            const TCM: &'static [u8] = #bytes_lit;
            let mut cgraph = ::rtaichi::get_cgraph(TCM)?;
            #arg_setter_list_tt
            cgraph.launch()?;
            ::rtaichi::sync()?;
            Ok(())
        }
    };

    // (penguinliong) Debug helper. DO NOT REMOVE.
    // std::fs::write(format!("rtaichi_{}.rs", kernel.name), expanded.to_string()).unwrap();

    Ok(expanded)
}
