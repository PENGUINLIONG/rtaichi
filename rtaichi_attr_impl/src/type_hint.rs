use syn::{MetaNameValue, visit::Visit, Attribute};

use crate::{expr_utils::{get_path_ident, get_lit_lit}, error::ErrorStore, abort, instr::Literal};

#[non_exhaustive]
#[derive(Debug)]
pub enum TypeHint {
    NDim(u32),
}

struct TypeHintParser<'ast> {
    es: &'ast mut ErrorStore,
    hints: Vec<TypeHint>,
}
impl<'ast> Visit<'ast> for TypeHintParser<'ast> {
    fn visit_meta_name_value(&mut self, i: &'ast MetaNameValue) {
        let ident = abort!(self.es => { get_path_ident(&i.path) });
        let key = ident.to_string();
        let value = abort!(self.es => { get_lit_lit(&i.lit) });
        let hint = match key.as_str() {
            "ndim" => match value {
                Literal::Int(x) => {
                    if x < 0 {
                        abort!(self.es => (&i.lit, "`ndim` cannot be negative"));
                    }
                    TypeHint::NDim(x as u32)
                },
                _ => abort!(self.es => (i, "`ndim` only accepts an integer")),
            },
            _ => abort!(self.es => (i, "unsupported type hint '{}'", key.as_str())),
        };
        self.hints.push(hint);
    }
    fn visit_meta_list(&mut self, i: &'ast syn::MetaList) {
        if abort!(self.es => { get_path_ident(&i.path) }) == "ti" {
            for x in i.nested.iter() {
                self.visit_nested_meta(x);
            }
        } else {
            abort!(self.es => (i, "unexpected argument attribute"));
        }
    }
}

pub fn parse_type_hint<'ast>(es: &'ast mut ErrorStore, i: &'ast [Attribute]) -> Vec<TypeHint> {
    let mut out = Vec::new();
    for x in i {
        if let Some(ident) = x.path.get_ident() {
            if ident == "ti" {
                let mut parser = TypeHintParser {
                    es,
                    hints: Vec::new(),
                };
                let x: syn::Meta = x.parse_meta().unwrap();
                parser.visit_meta(&x);
                out.extend(parser.hints);
                continue;
            }
        }
        abort!(es => (x, "unrecognized attribute"));
    }
    return out;
}
