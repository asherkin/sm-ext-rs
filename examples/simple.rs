use c_str_macro::c_str;
use sm_ext::native;
use sm_ext::types::{cell_t, IPluginContextPtr};
use sm_ext::{declare_native, register_natives, IExtension, IExtensionInterface, IPluginContext, IShareSys, SMExtension};
use std::error::Error;
use std::ffi::{CStr, CString};

#[derive(SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension();

unsafe extern "C" fn test_native(ctx: IPluginContextPtr, args: *const cell_t) -> cell_t {
    println!(">>> {:?} {:?}", ctx, args);

    47.into()
}

declare_native!(
    fn test_native2(ctx: &IPluginContext, args: &[cell_t]) -> cell_t {
        println!(">>> {:?} {:?}", ctx, args);

        0.into()
    }
);

#[native]
fn test_native3(ctx: &IPluginContext, a: i32, b: i32, c: f32, d: &CStr, e: &mut i32, f: &mut f32) -> Result<f32, String> {
    println!(">>> {:?} {:?} {:?} {:?} {:?} {:?} {:?}", ctx, a, b, c, d, e, f);

    *e = 47;
    *f = 1.5;

    Ok(5.0)
}

#[native]
fn test_native4(ctx: &IPluginContext) -> Result<i32, Box<dyn Error>> {
    println!(">>> {:?}", ctx);

    Err("This is an error...".into())
}

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let smutils = sys.request_interface(&myself, "ISourceMod", 14).map_err(|_| c_str!("Failed to get ISourceMod"))?;

        println!(">>> Got interface: {:?} v{:?}", smutils.get_interface_name().unwrap(), smutils.get_interface_version());

        register_natives!(&sys, &myself, [("Rust_Test", test_native), ("Rust_Test2", test_native2), ("Rust_Test3", __test_native3_adapter), ("Rust_Test4", __test_native4_adapter),]);

        Ok(())
    }
}
