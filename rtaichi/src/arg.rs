use syn::{Attribute, FnArg, Pat, Type, TypePath, Ident, Meta, MetaNameValue, __private::ToTokens, Path};
use taichi_runtime::sys;
use proc_macro_error::{abort};

use crate::{Literal, expr_utils::{get_path_ident, get_lit_lit}, KernelArgType};


enum TypeHint {
    Reserved(),
    NDim(u32),
}

fn parse_ty_hint_pair(name_value: &MetaNameValue) -> TypeHint {
    let key = get_path_ident(&name_value.path).to_string();
    let value = get_lit_lit(&name_value.lit);
    match key.as_str() {
        "ndim" => match value {
            Literal::Int(x) => {
                if x < 0 {
                    abort!(name_value.lit, "`ndim` cannot be a negative integer");
                }
                TypeHint::NDim(x as u32)
            },
            _ => abort!(name_value, "`ndim` only accepts an integer"),
        },
        _ => abort!(name_value, "unsupported type hint {}", key.as_str()),
    }
}
fn parse_ty_hint(attr: &Attribute) -> Option<Vec<TypeHint>> {
    let mut attr2 = attr.path.to_token_stream();
    attr2.extend(attr.tokens.to_token_stream());

    let args: syn::MetaList = syn::parse(attr2.into()).unwrap();
    let mut out = Vec::new();
    for meta in args.nested {
        match meta {
            syn::NestedMeta::Meta(Meta::NameValue(name_value)) => {
                let hint = parse_ty_hint_pair(&name_value);
                out.push(hint);
            },
            _ => abort!(meta, "invalid type hint"),
        }
    }
    Some(out)
}

fn find_ty_hint_attr<'a>(attrs: &'a [Attribute]) -> Option<&'a Attribute> {
    assert!(attrs.len() <= 1);
    attrs.iter()
        .find(|x| {
            if let Some(ident) = x.path.get_ident() {
                if ident.to_string() == "ti" {
                    return true
                }
            }
            false
        })
}

fn parse_prim_ty_name(ident: &Ident) -> sys::TiDataType {
    match ident.to_string().as_str() {
        "f16" => sys::TiDataType::F16,
        "f32" => sys::TiDataType::F32,
        "f64" => sys::TiDataType::F64,
        "i8" => sys::TiDataType::I8,
        "i16" => sys::TiDataType::I16,
        "i32" => sys::TiDataType::I32,
        "i64" => sys::TiDataType::I64,
        "u8" => sys::TiDataType::U8,
        "u16" => sys::TiDataType::U16,
        "u32" => sys::TiDataType::U32,
        "u64" => sys::TiDataType::U64,
        _ => abort!("unknown primitive data type `{}`", ident.to_string()),
    }
}
fn parse_prim_ty(ty: &Type) -> sys::TiDataType {
    match ty {
        Type::Path(path) => {
            if path.qself.is_some() {
                abort!(path, "`self` is not available in taichi scope");
            }
            if let Some(ident) = path.path.get_ident() {
                return parse_prim_ty_name(ident);
            }
        },
        _ => {},
    }
    abort!(ty, "expected an primitive data type identifier");
}

fn parse_ty_path(path: &TypePath, hints: &[TypeHint]) -> KernelArgType {
    assert!(path.path.segments.len() == 1);
    let last_seg = path.path.segments.last().unwrap();
    match last_seg.ident.to_string().as_str() {
        "NdArray" => {
            let dtype = match &last_seg.arguments {
                syn::PathArguments::AngleBracketed(x) => {
                    assert!(x.args.len() == 1);
                    let arg = x.args.first().unwrap();
                    match arg {
                        syn::GenericArgument::Type(ty) => parse_prim_ty(ty),
                        _ => abort!(arg, "expected a type generic argument"),
                    }
                },
                _ => abort!(last_seg.arguments, "`NdArray` takes a primitive data type as generic argument, but `{:?}` is received", last_seg.arguments),
            };
            let mut ndim = None;
            for hint in hints {
                match hint {
                    TypeHint::NDim(x) => {
                        ndim = Some(*x);
                    },
                    _ => {},
                }
            }
            if ndim.is_none() {
                abort!(path, "expected `ndim` in type hint attribute");
            }
            KernelArgType::NdArray { dtype, ndim: ndim.unwrap() }
        },
        _ => {
            let dtype = parse_prim_ty_name(&last_seg.ident);
            KernelArgType::Scalar { dtype }
        },
    }
}

fn parse_ty(ty: &Type, hints: &[TypeHint]) -> KernelArgType {
    match ty {
        Type::Path(path) => parse_ty_path(&path, hints),
        _ => abort!(ty, "expected type path"),
    }
}

pub struct KernelArg {
    name: String,
    ty: KernelArgType,
}
impl KernelArg {
    pub fn parse_arg(arg: &FnArg) -> KernelArg {
        match arg {
            syn::FnArg::Receiver(_) => {
                // Argument name without a type, a.k.a. `self`.
                abort!(arg, "kernel arg must be explicitly typed");
            },
            syn::FnArg::Typed(x) => {
                let hints = find_ty_hint_attr(&x.attrs)
                    .and_then(parse_ty_hint)
                    .unwrap_or_default();
                let arg_name = match &*x.pat {
                    Pat::Ident(ident) => {
                        assert!(ident.attrs.is_empty());
                        assert!(ident.by_ref.is_none());
                        assert!(ident.mutability.is_none());
                        assert!(ident.subpat.is_none());
                        ident.ident.to_string()
                    },
                    _ => abort!(x.pat, "kernel arg name must be an identifier"),
                };
                let arg_ty = parse_ty(&x.ty, &hints);
    
                KernelArg {
                    name: arg_name,
                    ty: arg_ty,
                }
            },
        }
    }

    pub fn pytaichi_kernel_arg_ty(&self) -> String {
        format!("{}: {}", self.name, self.ty.pytaichi_type())
    }
}
