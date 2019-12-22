//! A basic SourceMod extension providing natives and forwards to SourcePawn.
//!
//! ```sourcepawn
//! native int Rust_Call(int a, int b);
//! forward int OnRustCall(int a, int b);
//!
//! public void OnPluginStart()
//! {
//!     int result = Rust_Call(5, 4);
//!
//!     PrintToServer(">>> Rust_Call(5, 4) = %d", result);
//! }
//!
//! public int OnRustCall(int a, int b)
//! {
//!     PrintToServer(">>> OnRustCall(%d, %d)", a, b);
//!
//!     return a + b;
//! }
//! ```

use sm_ext::{c_str, native, register_natives, ExecType, ICallableApi, IExtension, IExtensionInterface, IForward, IForwardManager, IPluginContext, IShareSys, ParamType, SMExtension, SMInterfaceApi};
use std::cell::RefCell;
use std::error::Error;
use std::ffi::CString;

struct MyForwards {
    /// ```sourcepawn
    /// forward int OnRustCall(int a, int b);
    /// ```
    test: IForward,
}

thread_local! {
    static FORWARDS: RefCell<Option<MyForwards>> = RefCell::new(None);
}

/// A native that triggers the OnRustCall forward and returns the result.
///
/// ```sourcepawn
/// native int Rust_Call(int a, int b);
/// ```
#[native]
fn test_native_call(_ctx: &IPluginContext, a: i32, b: i32) -> Result<i32, Box<dyn Error>> {
    FORWARDS.with(|forwards| -> Result<i32, Box<dyn Error>> {
        let forwards = forwards.borrow();
        let test_forward = &forwards.as_ref().unwrap().test;
        test_forward.push_int(a)?;
        test_forward.push_int(b)?;
        let result: i32 = test_forward.execute()?.into();

        Ok(result)
    })
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension;

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let forward_manager: IForwardManager = sys.request_interface(&myself).map_err(|_| c_str!("Failed to get IForwardManager interface"))?;
        println!(">>> Got interface: {:?} v{:?}", forward_manager.get_interface_name(), forward_manager.get_interface_version());

        FORWARDS.with(|forwards| -> Result<(), CString> {
            let test_forward = forward_manager.create_raw_forward("OnRustCall", ExecType::Single, &[ParamType::Cell, ParamType::Cell]).map_err(|_| c_str!("Failed to create OnRustCall forward"))?;

            *forwards.borrow_mut() = Some(MyForwards { test: test_forward });

            Ok(())
        })?;

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
        FORWARDS.with(|forwards| {
            *forwards.borrow_mut() = None;
        });
    }
}
