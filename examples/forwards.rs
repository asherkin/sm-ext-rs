//! A basic SourceMod extension providing natives and forwards to SourcePawn.
//!
//! ```sourcepawn
//! native int Rust_Call(int a, int b);
//! forward int OnRustCall(int a, int b, const char[] c);
//!
//! public void OnPluginStart()
//! {
//!     int result = Rust_Call(5, 4);
//!
//!     PrintToServer(">>> Rust_Call(5, 4) = %d", result);
//! }
//!
//! public int OnRustCall(int a, int b, const char[] c)
//! {
//!     PrintToServer(">>> OnRustCall(%d, %d, %s)", a, b, c);
//!
//!     return a + b;
//! }
//! ```

use sm_ext::{c_str, forwards, native, register_natives, ExecType, IExtension, IExtensionInterface, IForwardManager, IPluginContext, IShareSys, SMExtension, SMInterfaceApi};
use std::ffi::CStr;

#[forwards]
struct MyForwards {
    /// ```sourcepawn
    /// forward int OnRustCall(int a, int b, const char[] c);
    /// ```
    #[global_forward("OnRustCall", ExecType::Single)]
    on_rust_call: fn(a: i32, b: i32, c: &CStr) -> i32,
}

/// A native that triggers the OnRustCall forward and returns the result.
///
/// ```sourcepawn
/// native int Rust_Call(int a, int b);
/// ```
#[native]
fn test_native_call(_ctx: &IPluginContext, a: i32, b: i32) -> Result<i32, sm_ext::SPError> {
    let result = MyForwards::on_rust_call(|fwd| fwd.execute(a, b, c_str!("Hello")))?;

    Ok(result)
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension;

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), Box<dyn std::error::Error>> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let forward_manager: IForwardManager = sys.request_interface(&myself)?;
        println!(">>> Got interface: {:?} v{:?}", forward_manager.get_interface_name(), forward_manager.get_interface_version());

        MyForwards::register(&forward_manager)?;

        register_natives!(
            &sys,
            &myself,
            [
                ("Rust_Call", test_native_call), //
            ]
        );

        Ok(())
    }

    fn on_extension_unload(&mut self) {
        MyForwards::unregister();
    }
}
