use taichi_runtime as ti;

use crate::{instr::{Instr, Operand, InstrId, Literal}, arg_ty::KernelArgType, kernel::Kernel};

fn get_dtype_name(dtype: &ti::DataType) -> &'static str {
    match dtype {
        ti::DataType::F16 => "ti.f16",
        ti::DataType::F32 => "ti.f32",
        ti::DataType::F64 => "ti.f64",
        ti::DataType::I8 => "ti.i8",
        ti::DataType::I16 => "ti.i16",
        ti::DataType::I32 => "ti.i32",
        ti::DataType::I64 => "ti.i64",
        ti::DataType::U8 => "ti.u8",
        ti::DataType::U16 => "ti.u16",
        ti::DataType::U32 => "ti.u32",
        ti::DataType::U64 => "ti.u64",
        _ => panic!("'{dtype:?}' is not a valid data type"),
    }
}

#[derive(Debug)]
struct PythonScriptStackFrame {
    lines: Vec<String>,
    indent: String,
}
impl PythonScriptStackFrame {
    fn new(depth: usize) -> Self {
        let indent = std::iter::repeat(' ')
            .take(4 * depth)
            .collect();
        Self {
            lines: Vec::new(),
            indent,
        }
    }
    fn commit_line(&mut self, line: String) {
        self.lines.push(self.indent.clone() + &line);
    }
    fn commit_line_no_indent(&mut self, line: String) {
        self.lines.push(line);
    }
}
#[derive(Debug)]
struct PythonScriptStack {
    stack: Vec<PythonScriptStackFrame>,
}
impl PythonScriptStack {
    fn new() -> Self {
        let mut out = PythonScriptStack {
            stack: Vec::new(),
        };
        out.push_stack();
        out
    }
    fn commit_line(&mut self, line: String) {
        let frame = self.stack.last_mut().unwrap();
        frame.commit_line(line);
    }
    fn commit_line_no_indent(&mut self, line: String) {
        let frame = self.stack.last_mut().unwrap();
        frame.commit_line_no_indent(line);
    }
    fn push_stack(&mut self) {
        self.stack.push(PythonScriptStackFrame::new(self.stack.len()));
    }
    fn pop_stack(&mut self) {
        let frame = self.stack.pop().unwrap();
        let merged_lines = frame.lines.join("\n");
        self.commit_line_no_indent(merged_lines);
    }
}

struct Printer {
    syms: Vec<String>,
    params: Vec<String>,
    args: Vec<String>,
    stack: PythonScriptStack,
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
            Operand::Unary { op, a } => {
                let a = self.print_instr(a, kernel)?;
                Some(format!("({op} {a})"))
            }
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
                Some(format!("ti.Vector([{}])", elems2.join(", ")))
            },
            Operand::Block { instrs } => {
                if instrs.is_empty() {
                    Some("pass".to_string())
                } else {
                    self.stack.push_stack();
                    for instr in instrs.iter() {
                        let line = self.print_instr(instr, kernel)?;
                        self.stack.commit_line(line);
                    }
                    self.stack.pop_stack();
                    Some(String::new())
                }
            },
            Operand::IfThenElse { cond, then_block, else_block } => {
                let cond = self.print_instr(cond, kernel)?;
                self.stack.commit_line(format!("if {cond}:"));
                let then_block = self.print_instr(then_block, kernel)?;
                self.stack.commit_line(then_block); // Empty line.
                if let Some(else_block) = else_block {
                    self.stack.commit_line(format!("else:"));
                    let else_block = self.print_instr(else_block, kernel)?;
                    self.stack.commit_line(else_block); // Empty line.
                }
                Some(String::new())
            },
            Operand::While { cond, body_block } => {
                let cond = self.print_instr(cond, kernel)?;
                self.stack.commit_line(format!("while {cond}:"));
                let body_block = self.print_instr(body_block, kernel)?;
                self.stack.commit_line(body_block);
                Some(String::new())
            },
            Operand::For { itervar, range, body_block } => {
                let itervar = self.print_instr(itervar, kernel)?;
                let range = self.print_instr(range, kernel)?;
                self.stack.commit_line(format!("for {itervar} in {range}:"));
                let body_block = self.print_instr(body_block, kernel)?;
                self.stack.commit_line(body_block);
                Some(String::new())
            },
            Operand::Call { fn_name, args } => {
                let mut args2 = Vec::new();
                for arg in args {
                    let arg = self.print_instr(arg, kernel)?;
                    args2.push(arg);
                }
                let arg_list = args2.join(", ");
                Some(format!("{fn_name}({arg_list})"))
            },
            Operand::MethodCall { target, fn_name, args } => {
                let target = self.print_instr(target, kernel)?;
                let mut args2 = Vec::new();
                for arg in args {
                    let arg = self.print_instr(arg, kernel)?;
                    args2.push(arg);
                }
                let arg_list = args2.join(", ");
                Some(format!("{target}.{fn_name}({arg_list})"))
            },
            Operand::Cast { value, ty } => {
                let value = self.print_instr(value, kernel)?;
                let ty_name = match ty {
                    KernelArgType::Scalar { dtype } => get_dtype_name(&dtype),
                    _ => panic!("type cast is only supported for scalar types"),
                };
                Some(format!("{ty_name}({value})"))
            },
        }
    }
    pub fn print_instrs(&mut self, kernel: &Kernel) -> String {
        assert!(self.stack.stack.len() == 1, "this can be called only for once");
        // The last instr is always the outermost block.
        let root_block_id = kernel.instrs.last().unwrap().id;
        if let Some(last_line) = self.print_instr(&root_block_id, kernel) {
            assert_eq!(last_line, "",
                "the last line must be empty (from output of a block instr)");

            debug_assert_eq!(self.stack.stack.len(), 1);
            let frame = &self.stack.stack[0];

            debug_assert_eq!(self.stack.stack[0].lines.len(), 1);
            let line = &frame.lines[0];

            line.clone()
        } else {
            panic!("cannot parse kernel instrs");
        }
    }
}

pub fn print_kernel(kernel: &Kernel, base_dir: &str) -> String {
    let mut printer = Printer {
        syms: Vec::new(),
        params: Vec::new(),
        args: Vec::new(),
        stack: PythonScriptStack::new(),
    };
    printer.print_arg_declrs(kernel);
    let sym_list = printer.syms.join("\n");
    let param_list = printer.params.join(", ");
    let arg_list = printer.args.join(", ");

    let body = printer.print_instrs(kernel);
    let base_dir = base_dir.replace("\\", "/");

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

        let python_script = print_kernel(&kernel, "tmp");
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

    #[test]
    fn test_if_then_else() {
        let mut es = ErrorStore::new();
        let tt = quote!(
            fn f(a: i32, b: f32, #[ti(ndim=2)] c: NdArray<i32>) {
                let a = 1;
                let b = 2;
                if a == 0 {
                    c[(3, 3)] = a + b;
                } else {
                    c[(a, a)] = a + b;
                }
            }
        );
        let i: ItemFn = syn::parse2(tt).unwrap();
        let kernel = parse_kernel(&mut es, &i);
        es.panic();

        let python_script = print_kernel(&kernel, "tmp");
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
    if (a == 0):
        _c[(3, 3)] = (a + b)
    
    else:
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
