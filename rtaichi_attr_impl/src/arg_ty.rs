use syn::{visit::Visit, TypePath, Type};
use taichi_runtime as ti;

use crate::{error::ErrorStore, abort};


#[derive(Clone, Debug)]
pub enum KernelArgType {
    Void {},
    Scalar {
        dtype: ti::DataType,
    },
    NdArray {
        dtype: ti::DataType,
        ndim: Option<u32>,
    },
}

struct ArgumentTypeParser<'ast> {
    es: &'ast mut ErrorStore,
    ident: String,
    gen_args: Vec<KernelArgType>,
    success: bool,
}
impl<'ast> Visit<'ast> for ArgumentTypeParser<'ast> {
    fn visit_generic_argument(&mut self, i: &'ast syn::GenericArgument) {
        match i {
            syn::GenericArgument::Type(x) => {
                if let Some(gen_arg) = parse_arg_ty(self.es, x) {
                    self.gen_args.push(gen_arg);
                    return;
                }
            },
            _ => {},
        }
        abort!(self.es => (i, "invlaid generic argument"));
    }
    fn visit_path_arguments(&mut self, i: &'ast syn::PathArguments) {
        match i {
            syn::PathArguments::None => {},
            syn::PathArguments::AngleBracketed(x) => {
                for arg in &x.args {
                    self.visit_generic_argument(arg);
                }
            },
            _ => abort!(self.es => (i, "invalid path argument pack")),
        }
    }
    fn visit_type_path(&mut self, i: &'ast TypePath) {
        if i.path.segments.len() != 1 {
            abort!(self.es => (i, "unknown argument type"));
        }
        if i.qself.is_some() {
            abort!(self.es => (i, "kernels cannot be associated to `self`"));
        }

        let seg = &i.path.segments[0];
        self.ident = seg.ident.to_string();
        self.visit_path_arguments(&seg.arguments);
    }
    fn visit_type(&mut self, i: &'ast Type) {
        match i {
            Type::Path(x) => {
                if x.qself.is_some() {
                    abort!(self.es => (x, "`self` is not available in taichi scope"));
                }
                self.visit_type_path(x);
                // Acknowledge here.
                self.success = true;
            },
            _ => abort!(self.es => (i, "expected a type")),
        }
    }
}

pub fn parse_arg_ty<'ast>(
    es: &'ast mut ErrorStore,
    i: &'ast Type
) -> Option<KernelArgType> {
    let mut x = ArgumentTypeParser {
        es,
        ident: String::new(),
        gen_args: Vec::new(),
        success: false,
    };
    x.visit_type(i);

    if !x.success {
        abort!(es => (i, "cannot parse argument type"));
    }

    let out = match x.ident.as_str() {
        // Primitive data types.
        "f16" => KernelArgType::Scalar { dtype: ti::DataType::F16 },
        "f32" => KernelArgType::Scalar { dtype: ti::DataType::F32 },
        "f64" => KernelArgType::Scalar { dtype: ti::DataType::F64 },
        "i8" => KernelArgType::Scalar { dtype: ti::DataType::I8 },
        "i16" => KernelArgType::Scalar { dtype: ti::DataType::I16 },
        "i32" => KernelArgType::Scalar { dtype: ti::DataType::I32 },
        "i64" => KernelArgType::Scalar { dtype: ti::DataType::I64 },
        "u8" => KernelArgType::Scalar { dtype: ti::DataType::U8 },
        "u16" => KernelArgType::Scalar { dtype: ti::DataType::U16 },
        "u32" => KernelArgType::Scalar { dtype: ti::DataType::U32 },
        "u64" => KernelArgType::Scalar { dtype: ti::DataType::U64 },
        // Opaque types.
        "NdArray" => {
            if x.gen_args.len() != 1 {
                abort!(es => (i, "invalid generic argument list"));
            }

            let dtype = match x.gen_args[0] {
                KernelArgType::Scalar { dtype } => dtype,
                _ => abort!(es => (i, "NdArray elements must have a primitive data type")),
            };

            KernelArgType::NdArray { dtype, ndim: None }
        },
        _ => abort!(es => (i, "unsupported type")),
    };
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn test_parse_i32() {
        let mut es = ErrorStore::new();
        let i: Type = syn::parse2(quote!(i32)).unwrap();
        match parse_arg_ty(&mut es, &i) {
            Some(KernelArgType::Scalar { dtype }) => {
                assert_eq!(dtype, ti::DataType::I32);
            },
            _ => panic!(),
        }
        assert!(es.is_empty());
    }

    #[test]
    fn test_parse_f32() {
        let mut es = ErrorStore::new();
        let i: Type = syn::parse2(quote!(f32)).unwrap();
        match parse_arg_ty(&mut es, &i) {
            Some(KernelArgType::Scalar { dtype }) => {
                assert_eq!(dtype, ti::DataType::F32);
            },
            _ => panic!(),
        }
        assert!(es.is_empty());
    }

    #[test]
    fn test_parse_ndarray() {
        let mut es = ErrorStore::new();
        let i: Type = syn::parse2(quote!(NdArray<f32>)).unwrap();
        match parse_arg_ty(&mut es, &i) {
            Some(KernelArgType::NdArray { dtype, ndim }) => {
                assert_eq!(dtype, ti::DataType::F32);
                assert_eq!(ndim, None);
            }
            _ => panic!(),
        }
        assert!(es.is_empty());
    }

}
