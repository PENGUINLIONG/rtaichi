use crate::{abort, Result, instr::Literal};
use quote::ToTokens;
use syn::{Expr, Ident, Path, Lit, Pat};

pub fn get_path_ident<'a>(path: &'a Path) -> Result<&'a Ident> {
    if let Some(ident) = path.get_ident() {
        Ok(ident)
    } else {
        abort!(path, "path '{}' an identifier", path.to_token_stream().to_string());
    }
}

pub fn get_expr_ident<'a>(expr: &'a Expr) -> Result<&'a Ident> {
    if let Expr::Path(path) = expr {
        if !path.attrs.is_empty() {
            abort!(path, "expr path must not have any attribute");
        }
        if path.qself.is_some() {
            abort!(path, "`self` is not available in taichi scope");
        }
        get_path_ident(&path.path)
    } else {
        abort!(expr, "expr '{}' expected an identifier", expr.to_token_stream().to_string());
    }
}

pub fn get_pat_ident<'a>(pat: &'a Pat) -> Result<&'a Ident> {
    if let Pat::Ident(x) = pat {
        if !x.attrs.is_empty() {
            abort!(x, "pat path must not have any attribute");
        }
        if x.by_ref.is_some() {
            abort!(x, "`ref` is not allowed");
        }
        if x.mutability.is_some() {
            abort!(x, "`mut` is not allowed");
        }
        if x.subpat.is_some() {
            abort!(x, "subpattern is not allowed");
        }
        Ok(&x.ident)
    } else {
        abort!(pat, "pat '{}' expected an identifier", pat.to_token_stream().to_string());
    }
}

pub(crate) fn get_lit_lit(lit: &Lit) -> Result<Literal> {
    let out = match lit {
        syn::Lit::Str(x) => {
            Literal::String(x.value())
        },
        syn::Lit::Int(x) => {
            Literal::Int(x.base10_parse::<i64>().unwrap())
        },
        syn::Lit::Float(x) => {
            Literal::Float(x.base10_parse::<f64>().unwrap())
        },
        syn::Lit::Bool(x) => {
            Literal::Bool(x.value())
        },
        _ => abort!(lit, "expected a boolean, int, float or string literal"),
    };
    Ok(out)
}

pub(crate) fn get_expr_lit(expr: &Expr) -> Result<Literal> {
    if let Expr::Lit(lit) = expr {
        assert!(lit.attrs.is_empty());
        get_lit_lit(&lit.lit)
    } else {
        abort!(expr, "expected a boolean, int, float or string literal");
    }
}
