use syn::{FnArg, Pat, visit::Visit};

use crate::{arg_ty::{parse_arg_ty, KernelArgType}, error::{ErrorStore}, type_hint::{parse_type_hint, TypeHint}, abort, abort_if};

pub struct KernelArg {
    pub name: String,
    pub ty: KernelArgType,
}

struct ArgumentListParser<'ast> {
    es: &'ast mut ErrorStore,
    arg: Option<KernelArg>,
}
impl<'ast> Visit<'ast> for ArgumentListParser<'ast> {
    fn visit_pat_type(&mut self, i: &'ast syn::PatType) {
        let hints = parse_type_hint(self.es, &i.attrs);
        let mut ty = if let Some(x) = parse_arg_ty(self.es, &i.ty) {
            x
        } else {
            abort!(self.es => (&i.ty, "cannot parse arg type"));
        };
        let arg_name = match &*i.pat {
            Pat::Ident(ident) => {
                assert!(ident.attrs.is_empty());
                assert!(ident.by_ref.is_none());
                assert!(ident.mutability.is_none());
                assert!(ident.subpat.is_none());
                ident.ident.to_string()
            },
            _ => abort!(self.es => (&i.pat, "kernel arg name must be an identifier")),
        };

        // Decorate types.
        match &mut ty {
            KernelArgType::NdArray { ndim, .. } => {
                for hint in hints {
                    match hint {
                        TypeHint::NDim(x) => *ndim = Some(x),
                        _ => abort!(self.es => (i, "unsupported type hint")),
                    }
                }

                abort_if!(ndim.is_none(), self.es => (i, "expected `ndim` in type hint attribute"));
            },
            _ => {},
        }

        let arg = KernelArg {
            name: arg_name,
            ty: ty,
        };

        self.arg = Some(arg);
    }

    fn visit_fn_arg(&mut self, i: &'ast FnArg) {
        match i {
            FnArg::Receiver(x) => {
                // Argument name without a type, a.k.a. `self`.
                abort!(self.es => (x, "kernel arg must be explicitly typed"));
            },
            syn::FnArg::Typed(x) => {
                self.visit_pat_type(x)
            },
        }
    }
}

pub fn parse_arg<'ast>(es: &'ast mut ErrorStore, i: &'ast FnArg) -> Option<KernelArg> {
    let mut parser = ArgumentListParser {
        es,
        arg: None,
    };
    parser.visit_fn_arg(i);
    if let Some(x) = parser.arg {
        Some(x)
    } else {
        abort!(es => (i, "cannot parse argument"));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;
    use taichi_sys::TiDataType;

    #[test]
    pub fn test_parse_f32_arg() {
        let mut es = ErrorStore::new();
        let i: FnArg = syn::parse2(quote!(a: f32)).unwrap();
        match parse_arg(&mut es, &i) {
            Some(KernelArg { name, ty: KernelArgType::Scalar { dtype } }) => {
                assert_eq!(name, "a");
                assert_eq!(dtype, TiDataType::F32);
            }
            _ => panic!(),
        }
        assert!(es.is_empty());
    }
    #[test]
    pub fn test_parse_i32_arg() {
        let mut es = ErrorStore::new();
        let i: FnArg = syn::parse2(quote!(a: i32)).unwrap();
        match parse_arg(&mut es, &i) {
            Some(KernelArg { name, ty: KernelArgType::Scalar { dtype } }) => {
                assert_eq!(name, "a");
                assert_eq!(dtype, TiDataType::I32);
            }
            _ => panic!(),
        }
        assert!(es.is_empty());
    }
    #[test]
    pub fn test_parse_ndarray_arg() {
        let mut es = ErrorStore::new();
        let i: FnArg = syn::parse2(quote!(#[ti(ndim=2)] a: NdArray<i32>)).unwrap();
        match parse_arg(&mut es, &i) {
            Some(KernelArg { name, ty: KernelArgType::NdArray { dtype, ndim } }) => {
                assert_eq!(name, "a");
                assert_eq!(dtype, TiDataType::I32);
                assert_eq!(ndim, Some(2));
            }
            _ => panic!(),
        }
        assert!(es.is_empty());
    }
}
