use std::{collections::HashMap};
use syn::{visit::Visit, Expr, Pat, Attribute, Path, Block, Ident};

use crate::{error::{ErrorStore, Result}, Literal, abort, abort_scope, arg_ty::KernelArgType, kernel::Kernel};
use crate::expr_utils::{get_expr_ident, get_pat_ident, get_lit_lit, get_path_ident};

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd, Ord)]
pub struct InstrId(pub usize);
impl std::fmt::Display for InstrId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.to_string())
    }
}

#[derive(Clone, Debug)]
pub enum Operand {
    Nop {},
    Arg {
        name: String,
        ty: KernelArgType,
    },
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
#[derive(Debug)]
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
    f: &'ast mut Kernel,
    bindings: HashMap<String, InstrId>,
    stack: Vec<Vec<InstrId>>,
}
impl<'ast> InstrParser<'ast> {
    fn new(
        es: &'ast mut ErrorStore,
        f: &'ast mut Kernel,
        bindings: &HashMap<String, InstrId>
    ) -> Self {
        Self { es, f, bindings: bindings.clone(), stack: Vec::new() }
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
        // Doesn't care whether it's been declared before because Rust does
        // support variable shadowing.
        self.bindings.insert(i.to_string(), InstrId(0));
        Ok(())
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
                let id = self.f.create_instr(Operand::Lit { lit });
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
                self.push_stack();
                self.visit_expr(&i.right);
                let stack = self.pop_stack().unwrap();
                let right = *stack.get(0).unwrap();
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
                    syn::BinOp::Rem(_) => "%",
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
                let id = self.f.create_instr(Operand::Binary { op, a, b });
                self.push_arg(id);
            }
        )
    }
    fn visit_expr_assign_op(&mut self, i: &'ast syn::ExprAssignOp) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                self.push_stack();
                self.visit_expr(&i.left);
                self.visit_expr(&i.right);
                let op = match i.op {
                    syn::BinOp::AddEq(_) => "+",
                    syn::BinOp::SubEq(_) => "-",
                    syn::BinOp::MulEq(_) => "*",
                    syn::BinOp::DivEq(_) => "/",
                    syn::BinOp::RemEq(_) => "%",
                    syn::BinOp::BitXorEq(_) => "^",
                    syn::BinOp::BitAndEq(_) => "&",
                    syn::BinOp::BitOrEq(_) => "|",
                    syn::BinOp::ShlEq(_) => "<<",
                    syn::BinOp::ShrEq(_) => ">>",
                    _ => abort!(&i.op, "invalid assign op"),
                };
                let args = self.pop_stack().unwrap();
                let a = *args.get(0).unwrap();
                let b = *args.get(1).unwrap();
                let id = self.f.create_instr(Operand::Binary { op, a, b });
                self.push_arg(id);
                self.set_var_by_expr(&i.left, id)?;
            }
        )
    }
    fn visit_local(&mut self, i: &'ast syn::Local) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                self.declare_var_by_pat(&i.pat)?;

                if let Some(init) = &i.init {
                    self.push_stack();
                    self.visit_expr(&init.1);
                    let stack = self.pop_stack().unwrap();
                    let right = *stack.get(0).unwrap();
                    self.set_var_by_pat(&i.pat, right)?;
                }
            }
        )
    }
}

pub fn parse_instrs<'ast>(
    es: &'ast mut ErrorStore,
    f: &'ast mut Kernel,
    bindings: &HashMap<String, InstrId>,
    block: &'ast Block,
) {
    let mut parser = InstrParser::new(es, f, bindings);
    parser.visit_block(block);
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    fn parse_instrs2<'ast>(
        es: &'ast mut ErrorStore,
        block: &'ast Block,
    ) -> Vec<Instr> {
        let mut f = Kernel::new();
        let mut bindings = HashMap::new();
        parse_instrs(es, &mut f, &mut bindings, block);
        f.instrs
    }

    #[test]
    fn test_int_lit() {
        let mut es = ErrorStore::new();
        let block: Block = syn::parse2(quote!({
            123
        })).unwrap();
        let instrs = parse_instrs2(&mut es, &block);
        es.panic();

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
        let block: Block = syn::parse2(quote!({
            123.0
        })).unwrap();
        let instrs = parse_instrs2(&mut es, &block);
        es.panic();

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
        let block: Block = syn::parse2(quote!({
            123 + 124
        })).unwrap();
        let instrs = parse_instrs2(&mut es, &block);
        es.panic();

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
    #[test]
    fn test_var_binding() {
        let mut es = ErrorStore::new();
        let block: Block = syn::parse2(quote!({
            let abc = 123;
            abc
        })).unwrap();
        let instrs = parse_instrs2(&mut es, &block);
        es.panic();

        assert_eq!(instrs.len(), 2);

        match instrs.get(1).unwrap() {
            Instr {
                id: InstrId(1),
                operand: Operand::Lit { lit: Literal::Int(123) },
            } => {},
            _ => panic!(),
        }
    }
    #[test]
    fn test_var_assign_op() {
        let mut es = ErrorStore::new();
        let block: Block = syn::parse2(quote!({
            let abc = 123;
            abc += 1;
            abc
        })).unwrap();
        let instrs = parse_instrs2(&mut es, &block);
        es.panic();

        for instr in instrs {
            match instr {
                Instr {
                    id: InstrId(0),
                    operand: Operand::Nop {},
                } => {},
                Instr {
                    id: InstrId(1),
                    operand: Operand::Lit { lit: Literal::Int(123) },
                } => {},
                Instr {
                    id: InstrId(2),
                    operand: Operand::Lit { lit: Literal::Int(1) },
                } => {},
                Instr {
                    id: InstrId(3),
                    operand: Operand::Binary {
                        op: "+",
                        a: InstrId(1),
                        b: InstrId(2)
                    },
                } => {},
                _ => panic!("{:?}", instr),
            }
        }
    }
}
