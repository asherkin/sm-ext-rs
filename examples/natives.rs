//! A basic SourceMod extension providing natives to SourcePawn.
//!
//! ```sourcepawn
//! native int Rust_Add(int a, int b);
//!
//! public void OnPluginStart()
//! {
//!     PrintToServer("Rust_Add(5, 4) = %d", Rust_Add(5, 4));
//! }
//! ```

use sm_ext::{native, register_natives, IExtension, IExtensionInterface, IPluginContext, IShareSys, SMExtension};
use std::ffi::{CStr, CString};

/// A native that adds two integers.
///
/// ```sourcepawn
/// native int Rust_Add(int a, int b);
/// ```
#[native]
fn test_native_add(_ctx: &IPluginContext, a: i32, b: i32) -> i32 {
    a + b
}

/// A native that has no return type.
///
/// ```sourcepawn
/// native void Rust_Void();
/// ```
#[native]
fn test_native_void(_ctx: &IPluginContext) {}

/// A native that returns an error to SourcePawn.
///
/// ```sourcepawn
/// native void Rust_Error();
/// ```
#[native]
fn test_native_error(_ctx: &IPluginContext) -> Result<(), &'static str> {
    Err("This is an error...")
}

/// A native that uses a number of different argument types.
///
/// The last 3 arguments are optional to support older plugins that were compiled before they were added.
///
/// All of these signatures are valid for this native:
/// ```sourcepawn
/// native float Rust_Test(int a, float b, const char[] c, int &d, float &e);
/// native float Rust_Test(int a, float b, const char[] c, int &d, float &e, int f);
/// native float Rust_Test(int a, float b, const char[] c, int &d, float &e, int f, float g);
/// native float Rust_Test(int a, float b, const char[] c, int &d, float &e, int f, float g, const char[] h);
/// ```
#[native]
#[allow(clippy::too_many_arguments, clippy::many_single_char_names)]
fn test_native(ctx: &IPluginContext, a: i32, b: f32, c: &CStr, d: &mut i32, e: &mut f32, f: Option<i32>, g: Option<f32>, h: Option<&str>) -> f32 {
    println!(">>> {:?} {:?} {:?} {:?} {:?} {:?} {:?} {:?} {:?}", ctx, a, b, c, d, e, f, g, h);

    *d = 47;
    *e = 1.5;

    5.0
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension;

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        register_natives!(
            &sys,
            &myself,
            [
                ("Rust_Add", test_native_add),     //
                ("Rust_Void", test_native_void),   //
                ("Rust_Error", test_native_error), //
                ("Rust_Test", test_native),        //
            ]
        );

        Ok(())
    }
}
