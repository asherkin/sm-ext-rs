//! A basic SourceMod extension providing natives and forwards to SourcePawn.

use sm_ext::{c_str, native, register_natives, IExtension, IExtensionInterface, IForwardManager, IPluginContext, IShareSys, SMExtension, SMInterfaceApi};
use std::ffi::CString;

/// A native that adds two integers.
///
/// ```sourcepawn
/// native int Rust_Add(int a, int b);
/// ```
#[native]
fn test_native_add(_ctx: &IPluginContext, a: i32, b: i32) -> i32 {
    a + b
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension;

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let forward_manager: IForwardManager = sys.request_interface(&myself).map_err(|_| c_str!("Failed to get IForwardManager interface"))?;
        println!(">>> Got interface: {:?} v{:?}", forward_manager.get_interface_name(), forward_manager.get_interface_version());

        register_natives!(
            &sys,
            &myself,
            [
                ("Rust_Add", test_native_add), //
            ]
        );

        Ok(())
    }
}
