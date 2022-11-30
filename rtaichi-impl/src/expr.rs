use std::{collections::{HashMap}, hash::Hash};

use proc_macro2::Ident;
use syn::{visit::Visit, Expr, Pat, Attribute, Path, Stmt};

use crate::{error::{ErrorStore, Result}, Literal, abort, expr_utils::{get_expr_ident, get_pat_ident, get_lit_lit, get_path_ident}, abort_scope};

#[derive(Clone, Copy, Default, Eq, PartialEq, PartialOrd, Ord)]
pub struct InstrId(pub usize);

pub enum Operand {
    Nop {},
    Lit {
        lit: Literal,
    },
    Binary {
        op: &'static str,
        a: InstrId,
        b: InstrId,
    }
}
impl Default for Operand {
    fn default() -> Self {
        Operand::Nop {}
    }
}
pub struct Instr {
    pub id: InstrId,
    pub operand: Operand,
}
impl Instr {
    pub fn new(id: InstrId, operand: Operand) -> Self {
        Self { id, operand }
    }
    pub fn set_operand(&mut self, operand: Operand) -> &mut Self {
        self.operand = operand;
        self
    } 
}


fn ensure_empty_attr_impl(i: &[Attribute]) -> Result<()> {
    if let Some(x) = i.first() {
        abort!(x, "attribute is not allowed here");
    }
    Ok(())
}

macro_rules! ensure_empty_attr {
    ($i:expr) => {
        ensure_empty_attr_impl(&$i.attrs)?;
    };
}

struct InstrParser<'ast> {
    es: &'ast mut ErrorStore,
    bindings: HashMap<String, InstrId>,
    stack: Vec<Vec<InstrId>>,
    instrs: Vec<Instr>,
}
impl<'ast> InstrParser<'ast> {
    fn new(es: &'ast mut ErrorStore) -> Self {
        Self {
            es,
            bindings: HashMap::new(),
            stack: Vec::new(),
            // Instruction #0 is reserved. For simplicity it points to a nop.
            instrs: vec![Instr::new(InstrId(0), Operand::Nop{})],
        }
    }

    fn create_instr<'a>(&'a mut self, operand: Operand) -> InstrId {
        let id = InstrId(self.instrs.len());
        let instr = Instr { id, operand };
        self.instrs.push(instr);
        id
    }

    fn push_stack(&mut self) {
        self.stack.push(Vec::new());
    }
    fn pop_stack(&mut self) -> Option<Vec<InstrId>> {
        self.stack.pop()
    }
    fn push_arg(&mut self, id: InstrId) {
        if let Some(last_stack) = self.stack.last_mut() {
            last_stack.push(id);
        }
    }

    fn declare_var(&mut self, i: &Ident) -> Result<()> {
        use std::collections::hash_map::Entry;
        match self.bindings.entry(i.to_string()) {
            Entry::Occupied(_) => abort!(i, "variable is already declared"),
            Entry::Vacant(entry) => {
                entry.insert(InstrId(0));
                Ok(())
            },
        }
    }
    fn declare_var_by_pat(&mut self, i: &Pat) -> Result<()> {
        let ident = get_pat_ident(i)?;
        self.declare_var(ident)
    }

    fn get_var(&mut self, i: &Ident) -> Result<InstrId> {
        if let Some(x) = self.bindings.get(&i.to_string()) {
            Ok(*x)
        } else {
            abort!(i, "variable is used before declaration");
        }
    }
    fn get_var_by_path(&mut self, i: &Path) -> Result<InstrId> {
        let ident = get_path_ident(i)?;
        self.get_var(ident)
    }
    // Right values.
    fn get_var_by_expr(&mut self, i: &Expr) -> Result<InstrId> {
        let ident = get_expr_ident(i)?;
        self.get_var(ident)
    }

    fn set_var(&mut self, i: &Ident, id: InstrId) -> Result<()> {
        use std::collections::hash_map::Entry;
        match self.bindings.entry(i.to_string()) {
            Entry::Occupied(mut entry) => {
                entry.insert(id);
                Ok(())
            },
            Entry::Vacant(_) => {
                abort!(i, "variable is assgned before declaration");
            },
        }
    }
    fn set_var_by_expr(&mut self, i: &Expr, id: InstrId) -> Result<()> {
        let ident = get_expr_ident(i)?;
        self.set_var(ident, id)
    }
    fn set_var_by_pat(&mut self, i: &Pat, id: InstrId) -> Result<()> {
        let ident = get_pat_ident(i)?;
        self.set_var(ident, id)
    }
}
impl<'ast> Visit<'ast> for InstrParser<'ast> {
    fn visit_expr_lit(&mut self, i: &'ast syn::ExprLit) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let lit = get_lit_lit(&i.lit)?;
                let id = self.create_instr(Operand::Lit { lit });
                self.push_arg(id);
            }
        )
    }
    fn visit_expr_path(&mut self, i: &'ast syn::ExprPath) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let id = self.get_var_by_path(&i.path)?;
                self.push_arg(id);
            }
        )
    }
    fn visit_expr_assign(&mut self, i: &'ast syn::ExprAssign) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let right = self.get_var_by_expr(&i.right)?;
                self.set_var_by_expr(&i.left, right)?;
            }
        )
    }
    fn visit_expr_binary(&mut self, i: &'ast syn::ExprBinary) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                self.push_stack();
                self.visit_expr(&i.left);
                self.visit_expr(&i.right);
                let op = match i.op {
                    syn::BinOp::Add(_) => "+",
                    syn::BinOp::Sub(_) => "-",
                    syn::BinOp::Mul(_) => "*",
                    syn::BinOp::Div(_) => "/",
                    syn::BinOp::Rem(_) => " mod ",
                    syn::BinOp::And(_) => "&&",
                    syn::BinOp::Or(_) => "||",
                    syn::BinOp::BitXor(_) => "^",
                    syn::BinOp::BitAnd(_) => "&",
                    syn::BinOp::BitOr(_) => "|",
                    syn::BinOp::Shl(_) => "<<",
                    syn::BinOp::Shr(_) => ">>",
                    syn::BinOp::Eq(_) => "==",
                    syn::BinOp::Lt(_) => "<",
                    syn::BinOp::Le(_) => "<=",
                    syn::BinOp::Ne(_) => "!=",
                    syn::BinOp::Ge(_) => ">=",
                    syn::BinOp::Gt(_) => ">",
                    _ => abort!(&i.op, "invalid binary op"),
                };
                let args = self.pop_stack().unwrap();
                let a = *args.get(0).unwrap();
                let b = *args.get(1).unwrap();
                self.create_instr(Operand::Binary { op, a, b });
            }
        )
    }
    fn visit_local(&mut self, i: &'ast syn::Local) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                self.declare_var_by_pat(&i.pat)?;

                if let Some(init) = &i.init {
                    let right = self.get_var_by_expr(&init.1)?;
                    self.set_var_by_pat(&i.pat, right)?;
                }
            }
        )
    }
}

pub fn parse_instrs<'ast>(
    es: &'ast mut ErrorStore,
    stmts: &'ast [Stmt],
) -> Vec<Instr> {
    let mut parser = InstrParser::new(es);
    for stmt in stmts {
        parser.visit_stmt(stmt);
    }
    parser.instrs
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;
    use syn::Stmt;

    #[test]
    fn test_int_lit() {
        let mut es = ErrorStore::new();
        let i: Stmt = syn::parse2(quote!(123;)).unwrap();
        let instrs = parse_instrs(&mut es, &[i]);

        assert_eq!(instrs.len(), 2);

        match instrs.get(1).unwrap() {
            Instr {
                id: InstrId(1),
                operand: Operand::Lit {
                    lit: Literal::Int(123),
                }
            } => {},
            _ => panic!(),
        }
    }
    #[test]
    fn test_float_lit() {
        let mut es = ErrorStore::new();
        let i: Stmt = syn::parse2(quote!(123.0;)).unwrap();
        let instrs = parse_instrs(&mut es, &[i]);

        assert_eq!(instrs.len(), 2);

        match instrs.get(1).unwrap() {
            Instr {
                id: InstrId(1),
                operand: Operand::Lit {
                    lit: Literal::Float(value),
                }
            } => {
                assert!((value - 123.0).abs() < 1e-4);
            },
            _ => panic!(),
        }
    }
    #[test]
    fn test_binary_lit() {
        let mut es = ErrorStore::new();
        let i: Stmt = syn::parse2(quote!(123 + 124;)).unwrap();
        let instrs = parse_instrs(&mut es, &[i]);

        assert_eq!(instrs.len(), 4);

        match instrs.get(1).unwrap() {
            Instr {
                id: InstrId(1),
                operand: Operand::Lit {
                    lit: Literal::Int(123),
                }
            } => {},
            _ => panic!(),
        }
        match instrs.get(2).unwrap() {
            Instr {
                id: InstrId(2),
                operand: Operand::Lit {
                    lit: Literal::Int(124),
                }
            } => {},
            _ => panic!(),
        }
        match instrs.get(3).unwrap() {
            Instr {
                id: InstrId(3),
                operand: Operand::Binary {
                    op: "+",
                    a: InstrId(1),
                    b: InstrId(2),
                }
            } => {},
            _ => panic!(),
        }
    }
}
