use std::{panic};

mod expr_utils;
mod arg;

use proc_macro::TokenStream;
use proc_macro_error::{proc_macro_error, abort};
use syn::{ItemFn, Pat, Expr};

use taichi_runtime::sys as sys;

fn rs2py_expr(expr: &Expr) -> String {
    match expr {
        Expr::Assign(assign) => {
            assert!(assign.attrs.is_empty());
            let left = rs2py_expr(&*assign.left);
            let right = rs2py_expr(&*assign.right);
            format!("{} = {}", left, right)
        },
        Expr::AssignOp(assign_op) => {
            assert!(assign_op.attrs.is_empty());
            let left = rs2py_expr(&*assign_op.left);
            let right = rs2py_expr(&*assign_op.right);
            let op = match assign_op.op {
                syn::BinOp::Eq(_) => "=",
                syn::BinOp::AddEq(_) => "+=",
                syn::BinOp::SubEq(_) => "=",
                syn::BinOp::MulEq(_) => "*=",
                syn::BinOp::DivEq(_) => "/=",
                syn::BinOp::RemEq(_) => "%=",
                syn::BinOp::BitXorEq(_) => "^=",
                syn::BinOp::BitAndEq(_) => "&=",
                syn::BinOp::BitOrEq(_) => "|=",
                syn::BinOp::ShlEq(_) => "<<=",
                syn::BinOp::ShrEq(_) => ">>=",
                _ => abort!(assign_op.op, "unknown assign op"),
            };
            format!("{} {} {}", left, op, right)
        }
        Expr::Binary(binary) => {
            let left = rs2py_expr(&binary.left);
            let right = rs2py_expr(&binary.right);
            let op = match binary.op {
                syn::BinOp::ShrEq(_) => "+",
                syn::BinOp::Add(_) => "+",
                syn::BinOp::Sub(_) => "-",
                syn::BinOp::Mul(_) => "*",
                syn::BinOp::Div(_) => "/",
                syn::BinOp::Rem(_) => "%",
                syn::BinOp::And(_) => " and ",
                syn::BinOp::Or(_) => " or ",
                syn::BinOp::BitXor(_) => "^",
                syn::BinOp::BitAnd(_) => "&",
                syn::BinOp::BitOr(_) => "|",
                syn::BinOp::Shl(_) => "<<",
                syn::BinOp::Shr(_) => ">>",
                syn::BinOp::Eq(_) => "==",
                syn::BinOp::Lt(_) => "-",
                syn::BinOp::Le(_) => "<=",
                syn::BinOp::Ne(_) => "!=",
                syn::BinOp::Ge(_) => ">=",
                syn::BinOp::Gt(_) => ">",
                _ => abort!(binary.op, "unknown binary op"),
            };
            format!("({}{}{})", left, op, right)
        },
        Expr::Lit(lit) => {
            assert!(lit.attrs.is_empty());
            match &lit.lit {
                syn::Lit::Int(x) => x.to_string(),
                syn::Lit::Float(x) => x.to_string(),
                syn::Lit::Bool(x) => {
                    if x.value {
                        "True".to_string()
                    } else {
                        "False".to_string()
                    }
                },
                _ => abort!(lit, "unknown literal"),
            }
        },
        Expr::Paren(paren) => {
            assert!(paren.attrs.is_empty());
            // Any inner expression is forced to be parenthesized.
            rs2py_expr(&*paren.expr)
        },
        Expr::Path(path) => {
            assert!(path.attrs.is_empty());
            path.path.segments.iter()
                .map(|x| {
                    x.ident.to_string()
                })
                .collect::<Vec<_>>()
                .join(".")
        },
        Expr::Index(index) => {
            assert!(index.attrs.is_empty());
            let expr = rs2py_expr(&index.expr);
            let idx = rs2py_expr(&index.index);
            format!("{}[{}]", expr, idx)
        }
        Expr::Return(ret) => {
            assert!(ret.attrs.is_empty());
            let value = ret.expr.as_ref()
                .map(|x| rs2py_expr(&*x))
                .unwrap_or_default();
            format!("return {}", value)
        },
        _ => abort!(expr, "unknown expression"),
    }
}

enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

enum KernelArgType {
    Scalar {
        dtype: sys::TiDataType,
    },
    NdArray{
        dtype: sys::TiDataType,
        ndim: u32,
    },
}
impl KernelArgType {
    pub fn pytaichi_type(&self) -> String {
        match self {
            KernelArgType::Scalar { dtype } => {
                match dtype {
                    sys::TiDataType::F32 => "ti.f32".to_owned(),
                    sys::TiDataType::I32 => "ti.i32".to_owned(),
                    _ => unimplemented!(),
                }
            },
            KernelArgType::NdArray { dtype, ndim } => {
                let ty_name = match dtype {
                    sys::TiDataType::F16 => "ti.f16",
                    sys::TiDataType::F32 => "ti.f32",
                    sys::TiDataType::F64 => "ti.f64",
                    sys::TiDataType::I8 => "ti.i8",
                    sys::TiDataType::I16 => "ti.i16",
                    sys::TiDataType::I32 => "ti.i32",
                    sys::TiDataType::I64 => "ti.i64",
                    sys::TiDataType::U8 => "ti.u8",
                    sys::TiDataType::U16 => "ti.u16",
                    sys::TiDataType::U32 => "ti.u32",
                    sys::TiDataType::U64 => "ti.u64",
                    _ => unimplemented!(),
                };
                format!("ti.types.ndarray(dtype={ty_name}, ndim={ndim})")
            },
        }
    }
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn kernel(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let kernel_impl: ItemFn = syn::parse(item).unwrap();

    let name = kernel_impl.sig.ident.to_string();
    println!("@ti.kernel");
    print!("def {}(", name);
    for arg in kernel_impl.sig.inputs.into_iter() {
        let kernel_arg = arg::KernelArg::parse_arg(&arg);
        print!("{}, ", kernel_arg.pytaichi_kernel_arg_ty());
    }
    println!("):");

    for stmt in kernel_impl.block.stmts {
        match stmt {
            syn::Stmt::Local(x) => {
                // Let binding.
                assert!(x.attrs.is_empty());
                let var_name = match x.pat {
                    Pat::Ident(ident) => {
                        assert!(ident.attrs.is_empty());
                        assert!(ident.by_ref.is_none());
                        assert!(ident.mutability.is_none());
                        assert!(ident.subpat.is_none());
                        ident.ident.to_string()
                    }
                    Pat::Type(ty) => {
                        assert!(ty.attrs.is_empty());
                        match *ty.pat {
                            Pat::Ident(ident) => {
                                assert!(ident.attrs.is_empty());
                                assert!(ident.by_ref.is_none());
                                assert!(ident.mutability.is_none());
                                assert!(ident.subpat.is_none());
                                ident.ident.to_string()
                            },
                            _ => panic!(),
                        }
                    },
                    _ => panic!(),
                };

                if let Some((_, expr)) = x.init {
                    println!("    {} = {}", var_name, rs2py_expr(&*expr));
                } else {
                    panic!();
                }
            },
            syn::Stmt::Item(_) => todo!(),
            syn::Stmt::Expr(expr) => println!("    {}", rs2py_expr(&expr)),
            syn::Stmt::Semi(expr, _) => println!("    {}", rs2py_expr(&expr)),
        }
    }

    TokenStream::new()
}
