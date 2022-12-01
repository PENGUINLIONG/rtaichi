use std::collections::HashMap;

use syn::{visit::Visit, ItemFn, Block};

use crate::{error::ErrorStore, instr::{Instr, Operand, InstrId, parse_instrs}, arg::parse_arg, abort};

pub struct Kernel {
    pub instrs: Vec<Instr>,
    pub roots: Vec<InstrId>,
}
impl Kernel {
    pub fn new() -> Self {
        Self {
            // Instruction #0 is reserved. For simplicity it points to a nop.
            instrs: vec![Instr::new(InstrId(0), Operand::Nop{})],
            roots: Vec::new(),
        }
    }

    pub fn create_instr(&mut self, operand: Operand) -> InstrId {
        let id = InstrId(self.instrs.len());
        let instr = Instr { id, operand };
        self.instrs.push(instr);
        id
    }
    pub fn reg_root_instr(&mut self, id: InstrId) {
        // Don't repeatly register a root.
        if let Some(x) = self.roots.last() {
            if *x == id {
                return;
            }
        }
        self.roots.push(id);
    }
}

struct KernelParser<'ast> {
    es: &'ast mut ErrorStore,
    f: Kernel,
    bindings: HashMap<String, InstrId>,
}
impl<'ast> Visit<'ast> for  KernelParser<'ast> {
    fn visit_fn_arg(&mut self, i: &'ast syn::FnArg) {
        if let Some(arg) = parse_arg(self.es, i) {
            let id = self.f.create_instr(Operand::Arg {
                name: arg.name.clone(),
                ty: arg.ty
            });
            self.bindings.insert(arg.name, id);
        } else {
            abort!(self.es => (i, "failed to parse function arg"));
        }
    }
    fn visit_block(&mut self, i: &'ast Block) {
        parse_instrs(self.es, &mut self.f, &self.bindings, i);
    }
}

pub fn parse_kernel<'ast>(es: &'ast mut ErrorStore, i: &'ast ItemFn) -> Kernel {
    let mut parser = KernelParser {
        es,
        f: Kernel::new(),
        bindings: HashMap::new(),
    };
    parser.visit_item_fn(i);
    parser.f
}

#[cfg(test)]
mod tests {
    use proc_macro2::TokenStream;
    use quote::quote;
    use taichi_sys::TiDataType;
    use crate::arg_ty::KernelArgType;

    use super::*;

    fn parse_test_kernel(tt: TokenStream) -> Kernel {
        let i: ItemFn = syn::parse2(tt).unwrap();

        let mut es = ErrorStore::new();
        let kernel = parse_kernel(&mut es, &i);
        kernel
    }

    #[test]
    fn test_empty_fn() {
        let kernel = parse_test_kernel(quote!(
            fn f() {}
        ));

        for instr in kernel.instrs {
            match instr {
                Instr {
                    id: InstrId(0),
                    operand: Operand::Nop {},
                } => {},
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_arg_fn() {
        let kernel = parse_test_kernel(quote!(
            fn f(a: i32, b: f32, #[ti(ndim=2)] c: NdArray<i32>) {}
        ));

        for instr in kernel.instrs {
            match instr {
                Instr {
                    id: InstrId(0),
                    operand: Operand::Nop {},
                } => {},
                Instr {
                    id: InstrId(1),
                    operand: Operand::Arg {
                        name,
                        ty: KernelArgType::Scalar {
                            dtype: TiDataType::I32,
                        },
                    },
                } if name == "a" => {},
                Instr {
                    id: InstrId(2),
                    operand: Operand::Arg {
                        name,
                        ty: KernelArgType::Scalar {
                            dtype: TiDataType::F32,
                        },
                    },
                } if name == "b" => {},
                Instr {
                    id: InstrId(3),
                    operand: Operand::Arg {
                        name,
                        ty: KernelArgType::NdArray {
                            dtype: TiDataType::I32,
                            ndim: Some(2),
                        },
                    },
                } if name == "c" => {},
                _ => panic!(),
            }
        }
    }
}
