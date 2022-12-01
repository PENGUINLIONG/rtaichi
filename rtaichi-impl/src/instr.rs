use std::{collections::HashMap};
use syn::{visit::Visit, Attribute, Path, Block, Ident};

use crate::{error::{ErrorStore, Result}, abort, abort_scope, arg_ty::KernelArgType, kernel::Kernel};
use crate::expr_utils::{get_pat_ident, get_lit_lit, get_path_ident};

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd, Ord)]
pub struct InstrId(pub usize);
impl std::fmt::Display for InstrId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.to_string())
    }
}

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub enum Operand {
    Nop {},
    Arg {
        name: String,
        ty: KernelArgType,
    },
    Var {
        name: String,
    },
    Lit {
        lit: Literal,
    },
    Binary {
        op: &'static str,
        a: InstrId,
        b: InstrId,
    },
    Accessor {
        base: InstrId,
        index: InstrId,
    },
    Tuple {
        elems: Vec<InstrId>,
    },
    Assign {
        dst: InstrId,
        src: InstrId,
    },
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
    fn push_id(&mut self, id: InstrId) {
        if let Some(last_stack) = self.stack.last_mut() {
            last_stack.push(id);
        }
    }

    fn set_alias(&mut self, i: &Ident, id: InstrId) -> Result<()> {
        self.bindings.insert(i.to_string(), id);
        Ok(())
    }
    fn resolve_alias(&mut self, i: &Ident) -> Result<InstrId> {
        if let Some(x) = self.bindings.get(&i.to_string()) {
            Ok(*x)
        } else {
            abort!(i, "ident {i} doesn't resolve to any instr");
        }
    }
    fn resolve_alias_by_path(&mut self, i: &Path) -> Result<InstrId> {
        let ident = get_path_ident(i)?;
        self.resolve_alias(ident)
    }
}
impl<'ast> Visit<'ast> for InstrParser<'ast> {
    fn visit_expr_lit(&mut self, i: &'ast syn::ExprLit) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let lit = get_lit_lit(&i.lit)?;
                let id = self.f.create_instr(Operand::Lit { lit });
                self.push_id(id);
            }
        )
    }
    fn visit_expr_path(&mut self, i: &'ast syn::ExprPath) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let id = self.resolve_alias_by_path(&i.path)?;
                self.push_id(id);
            }
        )
    }
    fn visit_expr_index(&mut self, i: &'ast syn::ExprIndex) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                self.push_stack();
                self.visit_expr(&i.expr);
                self.visit_expr(&i.index);
                let stack = self.pop_stack().unwrap();
                let base = stack[0];
                let index = stack[1];
                let id = self.f.create_instr(Operand::Accessor { base, index });
                self.push_id(id);
            }
        )
    }
    fn visit_expr_tuple(&mut self, i: &'ast syn::ExprTuple) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                self.push_stack();
                for elem in i.elems.iter() {
                    self.visit_expr(&elem);
                }
                let stack = self.pop_stack().unwrap();
                let id = self.f.create_instr(Operand::Tuple { elems: stack });
                self.push_id(id);
            }
        )
    }
    fn visit_expr_assign(&mut self, i: &'ast syn::ExprAssign) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                self.push_stack();
                self.visit_expr(&i.left);
                self.visit_expr(&i.right);
                let stack = self.pop_stack().unwrap();
                let src = stack[1];
                let dst = stack[0];
                let id = self.f.create_instr(Operand::Assign { dst, src });
                self.push_id(id);
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
                let stack = self.pop_stack().unwrap();
                let a = stack[0];
                let b = stack[1];
                let id = self.f.create_instr(Operand::Binary { op, a, b });
                self.push_id(id);
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
                let a = args[0];
                let b = args[1];
                let id = self.f.create_instr(Operand::Binary { op, a, b });
                self.push_id(id);
                let id = self.f.create_instr(Operand::Assign { dst: a, src: id });
                self.push_id(id);
            }
        )
    }
    fn visit_local(&mut self, i: &'ast syn::Local) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let name = get_pat_ident(&i.pat)?;
                let id = self.f.create_instr(Operand::Var { name: name.to_string() });
                self.push_id(id);
                self.set_alias(&name, id)?;

                if let Some(init) = &i.init {
                    self.push_stack();
                    self.visit_expr(&init.1);
                    let stack = self.pop_stack().unwrap();
                    let right = stack[0];
                    let id = self.f.create_instr(Operand::Assign { dst: id, src: right });
                    self.push_id(id);
                }
            }
        )
    }
    fn visit_stmt(&mut self, i: &'ast syn::Stmt) {
        syn::visit::visit_stmt(self, i);
        if let Some(root) = self.f.instrs.last() {
            self.f.reg_root_instr(root.id);
        }
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

        for instr in instrs {
            match instr {
                Instr {
                    id: InstrId(0),
                    operand: Operand::Nop {},
                } => {},
                Instr {
                    id: InstrId(1),
                    operand: Operand::Var { name },
                } => {
                    assert_eq!(name, "abc");
                },
                Instr {
                    id: InstrId(2),
                    operand: Operand::Lit { lit: Literal::Int(123) },
                } => {},
                Instr {
                    id: InstrId(3),
                    operand: Operand::Assign { src: InstrId(2), dst: InstrId(1) },
                } => {},
                _ => panic!("{:?}", instr),
            }
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
                    operand: Operand::Var { name }
                } => {
                    assert_eq!(name, "abc");
                },
                Instr {
                    id: InstrId(2),
                    operand: Operand::Lit { lit: Literal::Int(123) },
                } => {},
                Instr {
                    id: InstrId(3),
                    operand: Operand::Assign { dst: InstrId(1), src: InstrId(2) }
                } => {},
                Instr {
                    id: InstrId(4),
                    operand: Operand::Lit { lit: Literal::Int(1) },
                } => {},
                Instr {
                    id: InstrId(5),
                    operand: Operand::Binary {
                        op: "+",
                        a: InstrId(1),
                        b: InstrId(4)
                    },
                } => {},
                Instr {
                    id: InstrId(6),
                    operand: Operand::Assign { dst: InstrId(1), src: InstrId(5) }
                } => {},
                _ => panic!("{:?}", instr),
            }
        }
    }
}
