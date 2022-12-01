use taichi_sys::TiDataType;

use crate::{instr::{Instr, Operand, InstrId, Literal}, arg_ty::KernelArgType, kernel::Kernel};

fn get_dtype_name(dtype: &TiDataType) -> &'static str {
    match dtype {
        TiDataType::F16 => "ti.f16",
        TiDataType::F32 => "ti.f32",
        TiDataType::F64 => "ti.f64",
        TiDataType::I8 => "ti.i8",
        TiDataType::I16 => "ti.i16",
        TiDataType::I32 => "ti.i32",
        TiDataType::I64 => "ti.i64",
        TiDataType::U8 => "ti.u8",
        TiDataType::U16 => "ti.u16",
        TiDataType::U32 => "ti.u32",
        TiDataType::U64 => "ti.u64",
        _ => panic!("'{dtype:?}' is not a valid data type"),
    }
}

struct Block {
    lines: Vec<String>,
    indent: String,
}
impl Block {
    pub fn new(depth: usize) -> Self {
        let indent = std::iter::repeat(' ')
            .take(4 * depth + 4)
            .collect();
        Self {
            lines: Vec::new(),
            indent,
        }
    }

    fn commit_line(&mut self, line: String) {
        self.lines.push(self.indent.clone() + &line);
    }
}

struct Printer {
    syms: Vec<String>,
    params: Vec<String>,
    args: Vec<String>,
    block: Block,
}
impl Printer {
    fn print_arg_declr(&mut self, instr: &Instr) {
        match &instr.operand {
            Operand::Arg { name, ty } => {
                let sym_declr = match ty {
                    KernelArgType::Scalar { dtype } => {
                        let dtype_name = get_dtype_name(dtype);
                        format!("sym_{name} = ti.graph.Arg(ti.graph.ArgKind.SCALAR, '{name}', {dtype_name})")
                    },
                    KernelArgType::NdArray { dtype, ndim } => {
                        let dtype_name = get_dtype_name(dtype);
                        let ndim = ndim.unwrap();
                        format!("sym_{name} = ti.graph.Arg(ti.graph.ArgKind.NDARRAY, '{name}', {dtype_name}, field_dim={ndim})")
                    }
                    _ => panic!("unsupported type {ty:?}"),
                };

                let param_ty = match ty {
                    KernelArgType::Scalar { dtype } => {
                        get_dtype_name(dtype).to_string()
                    },
                    KernelArgType::NdArray { dtype, ndim } => {
                        let dtype_name = get_dtype_name(dtype);
                        let ndim = ndim.unwrap();
                        format!("ti.types.ndarray({dtype_name}, field_dim={ndim})")
                    },
                    _ => panic!("unsupported type {ty:?}"),
                };
                let param_declr = format!("_{name}: {param_ty}");

                let arg_declr = format!("sym_{name}");

                self.syms.push(sym_declr);
                self.params.push(param_declr);
                self.args.push(arg_declr);
            },
            _ => {},
        }
    }
    pub fn print_arg_declrs(&mut self, kernel: &Kernel) {
        for instr in kernel.instrs.iter() {
            self.print_arg_declr(&instr);
        }
    }
    fn print_instr(&mut self, id: &InstrId, kernel: &Kernel) -> Option<String> {
        let instr = &kernel.instrs[id.0];
        match &instr.operand {
            Operand::Nop {} => None,
            Operand::Arg { name, .. } => {
                Some(format!("_{name}"))
            },
            Operand::Var { name } => {
                Some(name.clone())
            },
            Operand::Lit { lit } => {
                let x = match lit {
                    Literal::String(x) => x.to_string(),
                    Literal::Int(x) => x.to_string(),
                    Literal::Float(x) => x.to_string(),
                    Literal::Bool(x) => x.to_string(),
                };
                Some(x)
            },
            Operand::Binary { op, a, b } => {
                let a = self.print_instr(a, kernel)?;
                let b = self.print_instr(b, kernel)?;
                Some(format!("({a} {op} {b})"))
            },
            Operand::Accessor { base, index } => {
                let base = self.print_instr(base, kernel)?;
                let index = self.print_instr(index, kernel)?;
                Some(format!("{base}[{index}]"))
            },
            Operand::Tuple { elems } => {
                let mut inner = Vec::with_capacity(elems.len());
                for elem in elems.iter() {
                    inner.push(self.print_instr(elem, kernel)?);
                }

                Some(format!("({})", inner.join(", ")))
            },
            Operand::Assign { src, dst } => {
                let src = self.print_instr(src, kernel)?;
                let dst = self.print_instr(dst, kernel)?;
                Some(format!("{dst} = {src}"))
            },
            Operand::Vector { elems } => {
                let mut elems2 = Vec::new();
                for elem in elems {
                    let elem = self.print_instr(elem, kernel)?;
                    elems2.push(elem);
                }
                Some(format!("ti.Vector({})", elems2.join(", ")))
            },
        }
    }
    pub fn print_instrs(&mut self, kernel: &Kernel) {
        for id in kernel.roots.iter() {
            if let Some(line) = self.print_instr(id, kernel) {
                self.block.commit_line(line);
            } else {
                panic!("instr {id} cannot be parsed: {:?}", kernel.instrs[id.0]);
            }
        }
    }
}

pub fn print_kernel(kernel: Kernel, base_dir: &str) -> String {
    let mut printer = Printer {
        syms: Vec::new(),
        params: Vec::new(),
        args: Vec::new(),
        block: Block::new(0),
    };
    printer.print_arg_declrs(&kernel);
    printer.print_instrs(&kernel);

    let sym_list = printer.syms.join("\n");
    let param_list = printer.params.join(", ");
    let arg_list = printer.args.join(", ");
    let body = printer.block.lines.join("\n");

    let python_script = format!(r#"
import taichi as ti
ti.init(ti.vulkan)

{sym_list}

@ti.kernel
def f({param_list}):
{body}

gb = ti.graph.GraphBuilder()
gb.dispatch(f, {arg_list})
g = gb.compile()

m = ti.aot.Module(ti.vulkan)
m.add_graph('g', g)
m.archive("{base_dir}/module.tcm")
"#);

    python_script
}

#[cfg(test)]
mod tests {
    use quote::quote;
    use syn::ItemFn;
    use crate::{kernel::parse_kernel, error::ErrorStore, print::print_kernel};

    #[test]
    fn test_print() {
        let mut es = ErrorStore::new();
        let tt = quote!(
            fn f(a: i32, b: f32, #[ti(ndim=2)] c: NdArray<i32>) {
                let a = 1;
                let b = 2;
                c[(a, a)] = a + b;
            }
        );
        let i: ItemFn = syn::parse2(tt).unwrap();
        let kernel = parse_kernel(&mut es, &i);
        es.panic();

        let python_script = print_kernel(kernel, "tmp");
        println!("{}", python_script);
        let expected = r#"
import taichi as ti
ti.init(ti.vulkan)

sym_a = ti.graph.Arg(ti.graph.ArgKind.SCALAR, 'a', ti.i32)
sym_b = ti.graph.Arg(ti.graph.ArgKind.SCALAR, 'b', ti.f32)
sym_c = ti.graph.Arg(ti.graph.ArgKind.NDARRAY, 'c', ti.i32, field_dim=2)

@ti.kernel
def f(_a: ti.i32, _b: ti.f32, _c: ti.types.ndarray(ti.i32, field_dim=2)):
    a = 1
    b = 2
    _c[(a, a)] = (a + b)

gb = ti.graph.GraphBuilder()
gb.dispatch(f, sym_a, sym_b, sym_c)
g = gb.compile()

m = ti.aot.Module(ti.vulkan)
m.add_graph('g', g)
m.archive("tmp/module.tcm")
"#;
        assert_eq!(expected, python_script);
    }
}
