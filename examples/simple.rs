use sm_ext::{c_str, cell_t, native, register_natives, IExtension, IExtensionInterface, IPluginContext, IShareSys, SMExtension};
use std::ffi::{CStr, CString};

#[native]
fn test_native(ctx: &IPluginContext, a: i32, b: i32, c: f32, d: &CStr, e: &mut i32, f: &mut f32, g: Option<i32>, h: Option<f32>, i: Option<&str>) -> Result<f32, String> {
    println!(">>> {:?} {:?} {:?} {:?} {:?} {:?} {:?} {:?} {:?} {:?}", ctx, a, b, c, d, e, f, g, h, i);

    *e = 47;
    *f = 1.5;

    Ok(5.0)
}

#[native]
fn test_native_error(ctx: &IPluginContext) -> Result<i32, Box<dyn std::error::Error>> {
    println!(">>> {:?}", ctx);

    Err("This is an error...".into())
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension;

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let smutils = sys.request_interface(&myself, "ISourceMod", 14).map_err(|_| c_str!("Failed to get ISourceMod"))?;

        println!(">>> Got interface: {:?} v{:?}", smutils.get_interface_name(), smutils.get_interface_version());

        register_natives!(&sys, &myself, [("Rust_Test", test_native), ("Rust_TestError", test_native_error)]);

        Ok(())
    }
}
