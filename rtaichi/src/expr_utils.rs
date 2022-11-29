use proc_macro_error::abort;
use syn::{Expr, Ident, Path, Lit};

use crate::Literal;

pub fn get_path_ident<'a>(path: &'a Path) -> &'a Ident {
    if let Some(ident) = path.get_ident() {
        ident
    } else {
        abort!(path, "expected an identifier");
    }
}

pub fn get_expr_ident<'a>(expr: &'a Expr) -> &'a Ident {
    if let Expr::Path(path) = expr {
        if !path.attrs.is_empty() {
            abort!(path, "expr path must not have any attribute");
        }
        if path.qself.is_some() {
            abort!(path, "`self` is not available in taichi scope");
        }
        get_path_ident(&path.path)
    } else {
        abort!(expr, "expected an identifier");
    }
}

pub(crate) fn get_lit_lit(lit: &Lit) -> Literal {
    match lit {
        syn::Lit::Str(x) => {
            return Literal::String(x.value());
        },
        syn::Lit::Int(x) => {
            return Literal::Int(x.base10_parse::<i64>().unwrap());
        },
        syn::Lit::Float(x) => {
            return Literal::Float(x.base10_parse::<f64>().unwrap());
        },
        syn::Lit::Bool(x) => {
            return Literal::Bool(x.value());
        },
        _ => {},
    }
    abort!(lit, "expected a boolean, int, float or string literal");
}

pub(crate) fn get_expr_lit(expr: &Expr) -> Literal {
    if let Expr::Lit(lit) = expr {
        assert!(lit.attrs.is_empty());
        get_lit_lit(&lit.lit)
    } else {
        abort!(expr, "expected a boolean, int, float or string literal");
    }
}
