use taichi_runtime::sys::TiDataType;

use crate::{instr::Instr, arg_ty::KernelArgType, kernel::Kernel};

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
    args: Vec<String>,
    block: Block,
}
impl Printer {
    fn print_instr(&mut self, instr: &Instr) {
        match &instr.operand {
            crate::instr::Operand::Nop {} => {
                let code = "None".to_string();
                self.block.commit_line(code);
            },
            crate::instr::Operand::Arg { name, ty } => {
                let arg_ty = match ty {
                    KernelArgType::Void{} => "None".to_string(),
                    KernelArgType::Scalar { dtype } => {
                        get_dtype_name(dtype).to_string()
                    },
                    KernelArgType::NdArray { dtype, ndim } => {
                        let dtype_name = get_dtype_name(dtype);
                        let ndim = ndim.unwrap();
                        format!("ti.types.ndarray(dtype={dtype_name}, ndim={ndim})")
                    },
                };
                let arg_declr = format!("{name}: {arg_ty}");
                self.args.push(arg_declr);

                let code = format!("_{} = {}", instr.id, name);
                self.block.commit_line(code);
            },
            crate::instr::Operand::Lit { lit } => {
                let x = match lit {
                    crate::Literal::String(x) => x.to_string(),
                    crate::Literal::Int(x) => x.to_string(),
                    crate::Literal::Float(x) => x.to_string(),
                    crate::Literal::Bool(x) => x.to_string(),
                };
                let code = format!("_{} = {}", instr.id, x);
                self.block.commit_line(code);
            },
            crate::instr::Operand::Binary { op, a, b } => {
                let code = format!("_{} = (_{} {} _{})", instr.id, a, op, b);
                self.block.commit_line(code);
            },
        }
    }
    pub fn print_instrs(&mut self, instrs: &[Instr]) {
        for instr in instrs {
            self.print_instr(instr);
        }
    }
}

pub fn print_kernel(kernel: Kernel) -> String {
    let mut printer = Printer {
        args: Vec::new(),
        block: Block::new(0),
    };
    printer.print_instrs(&kernel.instrs);

    let arg_list = printer.args.join(", ");
    let body = printer.block.lines.join("\n");

    let python_script = format!(r#"
import taichi as ti
ti.init(ti.vulkan)

@ti.kernel
def f({arg_list}):
{body}

m = ti.aot.Module(ti.vulkan)
m.add_kernel(f)
m.archive("tmp/module.tcm")
"#);

    println!("{}", python_script);

    String::new()
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
                a + b;
            }
        );
        let i: ItemFn = syn::parse2(tt).unwrap();
        let kernel = parse_kernel(&mut es, &i);
        es.panic();

        print_kernel(kernel);
        panic!();
    }
}
