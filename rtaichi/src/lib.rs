use std::collections::HashMap;

use ref_thread_local::{ref_thread_local, RefThreadLocal};

pub use rtaichi_attr::kernel;
use taichi_runtime as ti;
pub use ti::TaichiResult as Result;
pub use ti::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct ModuleKey(usize);

struct Instance {
    runtime: ti::Runtime,
    loaded_modules: Vec<ti::AotModule>,
    /// TCM byte code base address to module key mapping.
    module_keys: HashMap<*const u8, ModuleKey>,
}
impl Instance {
    pub fn new(runtime: ti::Runtime) -> Result<Self> {
        let out = Self {
            runtime,
            loaded_modules: Vec::new(),
            module_keys: HashMap::new(),
        };
        Ok(out)
    }
    pub fn reg_module(&mut self, tcm: &[u8]) -> Result<ModuleKey> {
        let key = self.loaded_modules.len();
        let module = self.runtime.create_aot_module(tcm)?;
        self.loaded_modules.push(module);
        Ok(ModuleKey(key))
    }
    pub fn get_cgraph(&mut self, tcm: &[u8]) -> Result<ti::ComputeGraph> {
        let key = if let Some(key) = self.module_keys.get(&tcm.as_ptr()) {
            *key
        } else {
            // The module used for its first time. Register the module to the
            // instance. 
            self.reg_module(tcm)?
        };
        self.loaded_modules.get(key.0)
            .ok_or(ti::Error::InvalidState)
            .and_then(|x| x.get_compute_graph("g"))
    }
}



ref_thread_local! {
    static managed INSTANCE: Option<Instance> = None;
}
macro_rules! get_instance {
    () => {
        {
            if !INSTANCE.is_initialized() {
                panic!("rtaichi is not initialized");
            }
            INSTANCE.borrow_mut().as_mut().ok_or(ti::Error::InvalidState)
        }
    };
}



pub fn init(arch: ti::Arch) -> Result<ti::Runtime> {
    if !INSTANCE.is_initialized() {
        let runtime = ti::Runtime::new(arch)?;
        *INSTANCE.borrow_mut() = Some(Instance::new(runtime.clone())?);
        Ok(runtime)
    } else {
        let runtime = get_instance!()?.runtime.clone();
        Ok(runtime)
    }
}
pub fn init_with_runtime(runtime: ti::Runtime) -> Result<ti::Runtime> {
    if !INSTANCE.is_initialized() {
        *INSTANCE.borrow_mut() = Some(Instance::new(runtime.clone())?);
        Ok(runtime)
    } else {
        Ok(get_instance!()?.runtime.clone())
    }
}

pub fn get_cgraph(tcm: &[u8]) -> Result<ti::ComputeGraph> {
    get_instance!()?.get_cgraph(tcm)
}

pub fn sync() -> Result<()> {
    get_instance!()?.runtime.wait()
}
