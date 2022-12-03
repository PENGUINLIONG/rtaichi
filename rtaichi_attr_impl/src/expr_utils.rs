use crate::{abort, Result, instr::Literal};
use quote::ToTokens;
use syn::{Ident, Path, Lit, Expr};

pub fn get_path_ident<'a>(path: &'a Path) -> Result<&'a Ident> {
    if let Some(ident) = path.get_ident() {
        Ok(ident)
    } else {
        abort!(path, "path '{}' is not an identifier", path.to_token_stream().to_string());
    }
}
pub fn get_fn_name<'a>(expr: &'a Expr) -> Result<Vec<&'a Ident>> {
    if let Expr::Path(path) = expr {
        if !path.attrs.is_empty() {
            abort!(path, "expr identifier '{}' cannot have any attribute", expr.to_token_stream().to_string());
        }
        if path.qself.is_some() {
            abort!(path, "expr identifier '{}' cannot be 'self'", expr.to_token_stream().to_string());
        }
        let mut out = Vec::new();
        for seg in path.path.segments.iter() {
            if !seg.arguments.is_empty() {
                abort!(seg, "function call cannot have generic args in taichi scope");
            }
            out.push(&seg.ident);
        }
        Ok(out)
    } else {
        abort!(expr, "expr '{}' is not an identifier", expr.to_token_stream().to_string());
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

