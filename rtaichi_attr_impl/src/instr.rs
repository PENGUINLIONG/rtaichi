use std::{collections::HashMap};
use syn::{visit::Visit, Attribute, Path, Block, Ident, Expr};

use crate::{error::{ErrorStore, Result}, abort, abort_scope, arg_ty::{KernelArgType, parse_arg_ty}, kernel::Kernel, expr_utils::{get_fn_name}};
use crate::expr_utils::{get_lit_lit, get_path_ident};

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
    Unary {
        op: &'static str,
        a: InstrId,
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
    Vector {
        elems: Vec<InstrId>,
    },
    Block {
        instrs: Vec<InstrId>,
    },
    IfThenElse {
        cond: InstrId,
        then_block: InstrId,
        else_block: Option<InstrId>,
    },
    While {
        cond: InstrId,
        body_block: InstrId,
    },
    For {
        itervar: InstrId,
        range: InstrId,
        body_block: InstrId,
    },
    Call {
        fn_name: String,
        args: Vec<InstrId>,
    },
    MethodCall {
        target: InstrId,
        fn_name: String,
        args: Vec<InstrId>,
    },
    Cast {
        value: InstrId,
        ty: KernelArgType,
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

#[derive(Debug)]
pub struct BasicBlock {
    pub instrs: Vec<Instr>,
}
impl BasicBlock {
    pub fn new() -> Self {
        Self { instrs: Vec::new() }
    }
    pub fn push_instr(&mut self, instr: Instr) -> &mut Self {
        self.instrs.push(instr);
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

#[derive(Debug)]
struct ScopeStackFrame {
    bindings: HashMap<String, InstrId>,
    id_buf: Vec<InstrId>,
}
impl ScopeStackFrame {
    fn new(bindings: HashMap<String, InstrId>) -> Self {
        Self {
            bindings,
            id_buf: Vec::new(),
        }
    }
}
#[derive(Debug)]
struct ScopeStack {
    stack: Vec<ScopeStackFrame>,
}
impl ScopeStack {
    fn new(bindings: HashMap<String, InstrId>) -> Self {
        Self { stack: vec![ScopeStackFrame::new(bindings)] }
    }

    fn push_stack(&mut self) {
        let bindings = self.stack.last()
            .map(|x| x.bindings.clone())
            .unwrap_or_default();
        self.stack.push(ScopeStackFrame::new(bindings));
    }
    fn pop_stack(&mut self) -> Option<Vec<InstrId>> {
        self.stack.pop()
            .map(|x| x.id_buf)
    }
    fn push_id(&mut self, id: InstrId) {
        if let Some(last_stack) = self.stack.last_mut() {
            last_stack.id_buf.push(id);
        }
    }
    fn get_frame_id_count(&self) -> Option<usize> {
        if let Some(last_stack) = self.stack.last() {
            return Some(last_stack.id_buf.len());
        }
        None
    }
    fn clip_frame_trailing_id(&mut self, i: usize) -> Option<Vec<InstrId>> {
        if let Some(last_stack) = self.stack.last_mut() {
            return Some(last_stack.id_buf.drain(i..).collect());
        }
        None
    }

    fn set_alias(&mut self, i: &Ident, id: InstrId) -> Result<()> {
        if let Some(top) = self.stack.last_mut() {
            top.bindings.insert(i.to_string(), id);
        } else {
            abort!(i, "attempt to set alias while the stack is empty");
        }
        Ok(())
    }
    fn resolve_alias(&mut self, i: &Ident) -> Result<InstrId> {
        let first_match = self.stack.iter()
            .rev()
            .filter_map(|x| x.bindings.get(&i.to_string()))
            .next();
        if let Some(x) = first_match {
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

macro_rules! stack_local_call {
    ($stack:expr => $inner:block) => {
        {
            let n = $stack.get_frame_id_count().unwrap();
            $inner
            let out = $stack.clip_frame_trailing_id(n).unwrap();
            debug_assert_eq!(n, $stack.get_frame_id_count().unwrap());
            out
        }
    };
}

struct InstrParser<'ast> {
    es: &'ast mut ErrorStore,
    f: &'ast mut Kernel,
    stack: ScopeStack,
}
impl<'ast> InstrParser<'ast> {
    fn new(
        es: &'ast mut ErrorStore,
        f: &'ast mut Kernel,
        bindings: HashMap<String, InstrId>
    ) -> Self {
        Self { es, f, stack: ScopeStack::new(bindings) }
    }
}
impl<'ast> Visit<'ast> for InstrParser<'ast> {
    fn visit_expr_lit(&mut self, i: &'ast syn::ExprLit) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let lit = get_lit_lit(&i.lit)?;
                let id = self.f.create_instr(Operand::Lit { lit });
                self.stack.push_id(id);
            }
        )
    }
    fn visit_expr_path(&mut self, i: &'ast syn::ExprPath) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let id = self.stack.resolve_alias_by_path(&i.path)?;
                self.stack.push_id(id);
            }
        )
    }
    fn visit_expr_index(&mut self, i: &'ast syn::ExprIndex) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_expr(&i.expr);
                        self.visit_expr(&i.index);
                    }
                };
                let base = stack[0];
                let index = stack[1];
                let id = self.f.create_instr(Operand::Accessor { base, index });
                self.stack.push_id(id);
            }
        )
    }
    fn visit_expr_tuple(&mut self, i: &'ast syn::ExprTuple) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        for elem in i.elems.iter() {
                            self.visit_expr(&elem);
                        }
                    }
                };
                let id = self.f.create_instr(Operand::Tuple { elems: stack });
                self.stack.push_id(id);
            }
        )
    }
    fn visit_expr_assign(&mut self, i: &'ast syn::ExprAssign) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_expr(&i.left);
                        self.visit_expr(&i.right);
                    }
                };
                let src = stack[1];
                let dst = stack[0];
                let id = self.f.create_instr(Operand::Assign { dst, src });
                self.stack.push_id(id);
            }
        )
    }
    fn visit_expr_unary(&mut self, i: &'ast syn::ExprUnary) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let op = match i.op {
                    syn::UnOp::Not(_) => " not ",
                    syn::UnOp::Neg(_) => "-",
                    _ => abort!(&i.op, "invalid binary op"),
                };
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_expr(&i.expr);
                    }
                };
                let a = stack[0];
                let id = self.f.create_instr(Operand::Unary { op, a });
                self.stack.push_id(id);
            }
        )
    }
    fn visit_expr_binary(&mut self, i: &'ast syn::ExprBinary) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let op = match i.op {
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
                    syn::BinOp::Lt(_) => "<",
                    syn::BinOp::Le(_) => "<=",
                    syn::BinOp::Ne(_) => "!=",
                    syn::BinOp::Ge(_) => ">=",
                    syn::BinOp::Gt(_) => ">",
                    _ => abort!(&i.op, "invalid binary op"),
                };
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_expr(&i.left);
                        self.visit_expr(&i.right);
                    }
                };
                let a = stack[0];
                let b = stack[1];
                let id = self.f.create_instr(Operand::Binary { op, a, b });
                self.stack.push_id(id);
            }
        )
    }
    fn visit_expr_assign_op(&mut self, i: &'ast syn::ExprAssignOp) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
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
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_expr(&i.left);
                        self.visit_expr(&i.right);
                    }
                };
                let a = stack[0];
                let b = stack[1];
                let id = self.f.create_instr(Operand::Binary { op, a, b });
                let id = self.f.create_instr(Operand::Assign { dst: a, src: id });
                self.stack.push_id(id);
            }
        )
    }
    fn visit_expr_array(&mut self, i: &'ast syn::ExprArray) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        for elem in i.elems.iter() {
                            self.visit_expr(elem);
                        }
                    }
                };
                let id = self.f.create_instr(Operand::Vector { elems: stack });
                self.stack.push_id(id);
            }
        )
    }
    fn visit_local(&mut self, i: &'ast syn::Local) {
        abort_scope!(
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_pat(&i.pat);
                    }
                };
                let var = stack[0];

                if let Some(init) = &i.init {
                    let stack = stack_local_call! {
                        self.stack => {
                            self.visit_expr(&init.1);
                        }
                    };
                    let right = stack[0];
                    let id = self.f.create_instr(Operand::Assign { dst: var, src: right });
                    self.stack.push_id(id);
                }
            }
        )
    }

    // Control flow.
    fn visit_block(&mut self, i: &'ast Block) {
        abort_scope! {
            self.es => {
                self.stack.push_stack();
                for x in i.stmts.iter() {
                    self.visit_stmt(x);
                }
                let stack = self.stack.pop_stack().unwrap();
                let id = self.f.create_instr(Operand::Block { instrs: stack });
                self.stack.push_id(id);
            }
        }
    }
    fn visit_expr_if(&mut self, i: &'ast syn::ExprIf) {
        abort_scope! {
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_expr(&i.cond);
                        self.visit_block(&i.then_branch);
                        if let Some(x) = &i.else_branch {
                            self.visit_expr(&x.1);
                        }
                    }
                };
                let cond = stack[0];
                let then_block = stack[1];
                let else_block = stack.get(2).cloned();
                let id = self.f.create_instr(Operand::IfThenElse { cond, then_block, else_block });
                self.stack.push_id(id);
            }
        }
    }
    fn visit_expr_while(&mut self, i: &'ast syn::ExprWhile) {
        abort_scope! {
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_expr(&i.cond);
                        self.visit_block(&i.body);
                    }
                };
                let cond = stack[0];
                let body_block = stack[1];
                let id = self.f.create_instr(Operand::While { cond, body_block, });
                self.stack.push_id(id);
            }
        }
    }
    fn visit_pat_ident(&mut self, i: &'ast syn::PatIdent) {
        abort_scope! {
            self.es => {
                ensure_empty_attr!(i);
                let name = &i.ident;
                let id = self.f.create_instr(Operand::Var { name: name.to_string() });
                self.stack.set_alias(&name, id)?;
                self.stack.push_id(id);
            }
        }
    }
    fn visit_pat_tuple(&mut self, i: &'ast syn::PatTuple) {
        abort_scope! {
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        for x in i.elems.iter() {
                            self.visit_pat(x);
                        }
                    }
                };
                let id = self.f.create_instr(Operand::Tuple { elems: stack });
                self.stack.push_id(id);
            }
        }
    }
    fn visit_expr_for_loop(&mut self, i: &'ast syn::ExprForLoop) {
        abort_scope! {
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_pat(&i.pat);
                        self.visit_expr(&i.expr);
                        self.visit_block(&i.body);
                    }
                };
                let itervar = stack[0];
                let range = stack[1];
                let body_block = stack[2];
                let id = self.f.create_instr(Operand::For { itervar, range, body_block });
                self.stack.push_id(id);
            }
        }
    }
    fn visit_expr_call(&mut self, i: &'ast syn::ExprCall) {
        abort_scope! {
            self.es => {
                ensure_empty_attr!(i);
                let fn_name = get_fn_name(&i.func)?
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>();
                let fn_name = fn_name.join(".");

                let stack = stack_local_call! {
                    self.stack => {
                        for x in i.args.iter() {
                            self.visit_expr(x);
                        }
                    }
                };
                let id = self.f.create_instr(Operand::Call { fn_name, args: stack });
                self.stack.push_id(id);
            }
        }
    }
    fn visit_expr_method_call(&mut self, i: &'ast syn::ExprMethodCall) {
        abort_scope! {
            self.es => {
                ensure_empty_attr!(i);
                let fn_name = i.method.to_string();
                if let Some(x) = &i.turbofish {
                    abort!(&x, "method generic argument is not allowed in taichi scope");
                }
                let mut stack = stack_local_call! {
                    self.stack => {
                        for arg in i.args.iter() {
                            self.visit_expr(arg);
                        }
                        self.visit_expr(&i.receiver);
                    }
                };
                let target = stack.pop().unwrap();
                let args = stack;
                let id = self.f.create_instr(Operand::MethodCall { target, fn_name, args });
                self.stack.push_id(id);
            }
        }
    }
    fn visit_expr_cast(&mut self, i: &'ast syn::ExprCast) {
        abort_scope! {
            self.es => {
                ensure_empty_attr!(i);
                let stack = stack_local_call! {
                    self.stack => {
                        self.visit_expr(&i.expr);
                    }
                };
                let value = stack[0];
                let ty = if let Some(x) = parse_arg_ty(self.es, &i.ty) {
                    x
                } else {
                    abort!(&i.ty, "unsupported type");
                };
                let id = self.f.create_instr(Operand::Cast { value, ty });
                self.stack.push_id(id);
            }
        }
    }



    // We only support a subset of Rust syntax.
    fn visit_expr(&mut self, i: &'ast Expr) {
        match i {
            Expr::Array(x) => self.visit_expr_array(x),
            Expr::Assign(x) => self.visit_expr_assign(x),
            Expr::AssignOp(x) => self.visit_expr_assign_op(x),
            Expr::Binary(x) => self.visit_expr_binary(x),
            Expr::Block(x) => self.visit_expr_block(x),
            Expr::Call(x) => self.visit_expr_call(x),
            Expr::Cast(x) => self.visit_expr_cast(x),
            Expr::ForLoop(x) => self.visit_expr_for_loop(x),
            Expr::If(x) => self.visit_expr_if(x),
            Expr::Index(x) => self.visit_expr_index(x),
            Expr::Lit(x) => self.visit_expr_lit(x),
            Expr::MethodCall(x) => self.visit_expr_method_call(x),
            Expr::Path(x) => self.visit_expr_path(x),
            Expr::Tuple(x) => self.visit_expr_tuple(x),
            Expr::Unary(x) => self.visit_expr_unary(x),
            Expr::While(x) => self.visit_expr_while(x),
            _ => abort!(self.es => (i, "expr type not supported")),
        }
    }
    fn visit_pat(&mut self, i: &'ast syn::Pat) {
        match i {
            syn::Pat::Ident(x) => self.visit_pat_ident(x),
            syn::Pat::Tuple(x) => self.visit_pat_tuple(x),
            _ => abort!(self.es => (i, "pat type not supported")),
        }
    }
}

pub fn parse_instrs<'ast>(
    es: &'ast mut ErrorStore,
    f: &'ast mut Kernel,
    bindings: HashMap<String, InstrId>,
    block: &'ast Block,
) {
    let mut parser = InstrParser::new(es, f, bindings);
    parser.visit_block(block);
    assert_eq!(parser.stack.stack.len(), 1);
    assert_eq!(parser.stack.stack[0].id_buf.len(), 1);
    assert_eq!(parser.stack.stack[0].id_buf[0], f.instrs.last().unwrap().id);
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    fn parse_instrs2<'ast>(
        es: &'ast mut ErrorStore,
        block: &'ast Block,
    ) -> Vec<Instr> {
        let mut f = Kernel::new("f".to_string());
        parse_instrs(es, &mut f, HashMap::new(), block);
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

        assert_eq!(instrs.len(), 3);

        for instr in instrs {
            match instr {
                Instr {
                    id: InstrId(0),
                    operand: Operand::Nop {},
                } => {},
                Instr {
                    id: InstrId(1),
                    operand: Operand::Lit {
                        lit: Literal::Int(123),
                    }
                } => {},
                Instr {
                    id: InstrId(2),
                    operand: Operand::Block { instrs },
                } => {
                    assert_eq!(instrs, vec![InstrId(1)]);
                },
                _ => panic!("{instr:?}"),
            }
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

        assert_eq!(instrs.len(), 3);

        for instr in instrs {
            match instr {
                Instr {
                    id: InstrId(0),
                    operand: Operand::Nop {},
                } => {},
                Instr {
                    id: InstrId(1),
                    operand: Operand::Lit {
                        lit: Literal::Float(value),
                    }
                } => {
                    assert!((value - 123.0).abs() < 1e-4);
                },
                Instr {
                    id: InstrId(2),
                    operand: Operand::Block { instrs },
                } => {
                    assert_eq!(instrs, vec![InstrId(1)]);
                },
                _ => panic!(),
            }
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

        assert_eq!(instrs.len(), 5);

        for instr in instrs {
            match instr {
                Instr {
                    id: InstrId(0),
                    operand: Operand::Nop {},
                } => {},
                Instr {
                    id: InstrId(1),
                    operand: Operand::Lit {
                        lit: Literal::Int(123),
                    }
                } => {},
                Instr {
                    id: InstrId(2),
                    operand: Operand::Lit {
                        lit: Literal::Int(124),
                    }
                } => {},
                Instr {
                    id: InstrId(3),
                    operand: Operand::Binary {
                        op: "+",
                        a: InstrId(1),
                        b: InstrId(2),
                    }
                } => {},
                Instr {
                    id: InstrId(4),
                    operand: Operand::Block { instrs },
                } => {
                    assert_eq!(instrs, vec![InstrId(3)]);
                },
                _ => panic!(),
            }
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
                Instr {
                    id: InstrId(4),
                    operand: Operand::Block { instrs },
                } => {
                    assert_eq!(instrs, vec![InstrId(3), InstrId(1)]);
                },
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
                Instr {
                    id: InstrId(7),
                    operand: Operand::Block { instrs },
                } => {
                    assert_eq!(instrs, vec![InstrId(3), InstrId(6), InstrId(1)]);
                },
                _ => panic!("{:?}", instr),
            }
        }
    }
    #[test]
    fn test_if_then_else() {
        let mut es = ErrorStore::new();
        let block: Block = syn::parse2(quote!({
            if 1 == 2 { 3 } else { 4 }
        })).unwrap();
        let _instrs = parse_instrs2(&mut es, &block);
        es.panic();

        // TODO: (penguinliong) Check instructions.
    }
    #[test]
    fn test_while_loop() {
        let mut es = ErrorStore::new();
        let block: Block = syn::parse2(quote!({
            while 1 == 2 { 3 }
        })).unwrap();
        let _instrs = parse_instrs2(&mut es, &block);
        es.panic();

        // TODO: (penguinliong) Check instructions.
    }
    #[test]
    fn test_ranged_loop() {
        let mut es = ErrorStore::new();
        let block: Block = syn::parse2(quote!({
            for i in range(4) { 3 }
        })).unwrap();
        let _instrs = parse_instrs2(&mut es, &block);
        es.panic();

        // TODO: (penguinliong) Check instructions.
    }
    #[test]
    fn test_cast() {
        let mut es = ErrorStore::new();
        let block: Block = syn::parse2(quote!({
            1 as f32
        })).unwrap();
        let _instrs = parse_instrs2(&mut es, &block);
        es.panic();

        // TODO: (penguinliong) Check instructions.
    }
}
