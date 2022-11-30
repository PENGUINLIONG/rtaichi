mod expr_utils;

pub mod error;
pub mod type_hint;
pub mod arg_ty;
pub mod arg;
pub mod instr;
pub mod kernel;

pub mod print;

pub use error::Result;

//use crate::arg::parse_arg;




#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}


/*
fn rs2py_expr(expr: &Expr) -> Result<String> {
    match expr {
        Expr::Assign(assign) => {
            assert!(assign.attrs.is_empty());
            let left = rs2py_expr(&*assign.left)?;
            let right = rs2py_expr(&*assign.right)?;
            Ok(format!("{} = {}", left, right))
        },
        Expr::AssignOp(assign_op) => {
            assert!(assign_op.attrs.is_empty());
            let left = rs2py_expr(&*assign_op.left)?;
            let right = rs2py_expr(&*assign_op.right)?;
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
                //_ => Err(syn::Error::new(assign_op.op.span(), "unknown assign op"))?,
            };
            Ok(format!("{} {} {}", left, op, right))
        }
        Expr::Binary(binary) => {
            let left = rs2py_expr(&binary.left)?;
            let right = rs2py_expr(&binary.right)?;
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
            Ok(format!("({}{}{})", left, op, right))
        },
        Expr::Lit(lit) => {
            assert!(lit.attrs.is_empty());
            let lit_str = match &lit.lit {
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
            };
            Ok(lit_str)
        },
        Expr::Paren(paren) => {
            assert!(paren.attrs.is_empty());
            // Any inner expression is forced to be parenthesized.
            rs2py_expr(&*paren.expr)
        },
        Expr::Path(path) => {
            assert!(path.attrs.is_empty());
            let path_str = path.path.segments.iter()
                .map(|x| {
                    x.ident.to_string()
                })
                .collect::<Vec<_>>()
                .join(".");
            Ok(path_str)
        },
        Expr::Index(index) => {
            assert!(index.attrs.is_empty());
            let expr = rs2py_expr(&index.expr)?;
            let idx = rs2py_expr(&index.index)?;
            Ok(format!("{}[{}]", expr, idx))
        }
        Expr::Return(ret) => {
            assert!(ret.attrs.is_empty());
            if let Some(value) = ret.expr.as_ref()
                .map(|x| rs2py_expr(&*x)) {
                Ok(format!("return {}", value?))
            } else {
                Ok(format!("return"))
            }
        },
        _ => abort!(expr, "unknown expression"),
    }
}
struct Id {
    id: usize,
    name: Option<String>,
}
enum Value {
    Literal(Literal),
    Variable(Id),
}


fn generate_taichi_script_impl(item: TokenStream) -> std::result::Result<String, fmt::Error> {
    let mut out = String::new();

    let kernel_impl: ItemFn = syn::parse2(item).unwrap();

    let tmpdir = tempdir::TempDir::new("rtaichi")
        .expect("cannot create tmp for aot module");
    let module_path = tmpdir.path().to_string_lossy();

    let name = kernel_impl.sig.ident.to_string();

    writeln!(out, r#"
import taichi as ti

ti.init(ti.vulkan)
"#)?;

    writeln!(out, "@ti.kernel")?;
    write!(out, "def {}(", name)?;
    for arg in kernel_impl.sig.inputs.into_iter() {
        let kernel_arg = parse_arg(&arg);
        write!(out, "{}, ", kernel_arg.pytaichi_kernel_arg_ty())?;
    }
    writeln!(out, "):")?;

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
                    writeln!(out, "    {} = {}", var_name, rs2py_expr(&*expr).unwrap())?;
                } else {
                    panic!();
                }
            },
            syn::Stmt::Item(_) => todo!(),
            syn::Stmt::Expr(expr) => writeln!(out, "    {}", rs2py_expr(&expr).unwrap())?,
            syn::Stmt::Semi(expr, _) => writeln!(out, "    {}", rs2py_expr(&expr).unwrap())?,
        }
    }

    writeln!(out, r#"
m = ti.aot.Module(ti.vulkan)
m.add_kernel({name})
m.archive("{module_path}")
"#)?;

    Ok(out)
}

pub fn generate_taichi_script(item: TokenStream) -> String {
    generate_taichi_script_impl(item).unwrap()
}


//#[proc_macro_error]
//#[proc_macro_attribute]
//pub fn kernel(_attr: TokenStream, item: TokenStream) -> TokenStream {
//    let py_script = kernel_impl(item).unwrap();
//    println!("{py_script}");
//    TokenStream::new()
//}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use proc_macro2::TokenStream;

    use crate::generate_taichi_script;

    fn compile(src: &str) -> String {
        let tt = TokenStream::from_str(src)
            .expect("cannot parse token tree for source code");
        generate_taichi_script(tt)
    }

    #[test]
    fn test_empty() {
        let src = "fn f() {}";
        println!("{}", compile(src));
    }
    #[test]
    fn test_i32_arg() {
        let src = "fn f(a: i32) {}";
        println!("{}", compile(src));
    }
    #[test]
    fn test_f32_arg() {
        let src = "fn f(a: f32) {}";
        println!("{}", compile(src));
    }
    #[test]
    fn test_ndarray_arg() {
        let src = "fn f(#[ti(ndim=2)] a: NdArray<i32>) {}";
        println!("{}", compile(src));
    }
}
*/
