use core::panic;
use std::fmt::format;

use syn::{ItemFn, Pat, Type, Expr};
use proc_macro::{self, TokenStream};

fn rs2py_expr(expr: &Expr) -> String {
    match expr {
        Expr::Assign(assign) => {
            assert!(assign.attrs.is_empty());
            let left = rs2py_expr(&*assign.left);
            let right = rs2py_expr(&*assign.right);
            format!("{} = {}", left, right)
        },
        Expr::Binary(binary) => {
            let left = rs2py_expr(&binary.left);
            let right = rs2py_expr(&binary.right);
            let op = match binary.op {
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
                _ => unimplemented!(),
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
                _ => unimplemented!(),
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
        Expr::Return(ret) => {
            assert!(ret.attrs.is_empty());
            let value = ret.expr.as_ref()
                .map(|x| rs2py_expr(&*x))
                .unwrap_or_default();
            format!("return {}", value)
        },
        _ => unimplemented!(),
    }
}

fn rs2py_ty(ty_name: &str) -> &str {
    match ty_name {
        "i32" => "ti.i32",
        "f32" => "ti.f32",
        _ => unimplemented!(),
    }
}

#[proc_macro_attribute]
pub fn kernel(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let kernel_impl: ItemFn = syn::parse(item).unwrap();
    // println!("attrs = {:?}", &kernel_impl.attrs);
    // println!("vis = {:?}", &kernel_impl.vis);
    // println!("sig = {:?}", &kernel_impl.sig);
    // println!("block = {:?}", &kernel_impl.block);

    let name = kernel_impl.sig.ident.to_string();
    println!("@ti.kernel");
    print!("def {}(", name);
    for arg in kernel_impl.sig.inputs.into_iter() {
        let (arg_name, ty_name) = match arg {
            syn::FnArg::Receiver(_) => {
                // Argument name without a type, a.k.a. `self`.
                panic!();
            },
            syn::FnArg::Typed(x) => {
                assert!(x.attrs.is_empty());
                let arg_name = match *x.pat {
                    Pat::Ident(ident) => {
                        assert!(ident.attrs.is_empty());
                        assert!(ident.by_ref.is_none());
                        assert!(ident.mutability.is_none());
                        assert!(ident.subpat.is_none());
                        ident.ident.to_string()
                    },
                    _ => unimplemented!(),
                };
                let ty_name = match *x.ty {
                    Type::Path(path) => {
                        assert!(path.qself.is_none());
                        assert!(path.path.segments.len() == 1);
                        rs2py_ty(&path.path.segments.last().unwrap().ident.to_string()).to_owned()
                    },
                    _ => unimplemented!(),
                };
                (arg_name, ty_name)
            },
        };

        print!("{}: {}, ", arg_name, ty_name);
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
            syn::Stmt::Expr(expr) => println!("{}", rs2py_expr(&expr)),
            syn::Stmt::Semi(expr, _) => println!("{}", rs2py_expr(&expr)),
        }
    }

    TokenStream::new()
}
