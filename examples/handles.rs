//! A basic SourceMod extension using a custom Handle type.
//!
//! ```sourcepawn
//! methodmap Rust < Handle {
//!     public native Rust();
//!     public native void Add(int number);
//!     property int Result {
//!         public native get();
//!     }
//! }
//!
//! public void OnPluginStart()
//! {
//!     Rust test = new Rust();
//!     test.Add(5);
//!     test.Add(4);
//!     int result = test.Result;
//!
//!     PrintToServer(">>> 5 + 4 = %d", result);
//! }
//! ```

use sm_ext::{c_str, native, register_natives, IExtension, IExtensionInterface, IHandleSys, IPluginContext, IShareSys, SMExtension, SMInterfaceApi};
use std::ffi::CString;

#[native]
fn native_obj_new(_ctx: &IPluginContext) -> i32 {
    println!(">>> Rust.Rust()");

    0
}

#[native]
fn native_obj_add(_ctx: &IPluginContext, this: i32, number: i32) {
    println!(">>> Rust.Add({:?}, {:?})", this, number);
}

#[native]
fn native_obj_result(_ctx: &IPluginContext, this: i32) -> i32 {
    println!(">>> Rust.Result({:?})", this);

    0
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension;

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let handlesys: IHandleSys = sys.request_interface(&myself).map_err(|_| c_str!("Failed to get IHandleSys interface"))?;
        println!(">>> Got interface: {:?} v{:?}", handlesys.get_interface_name(), handlesys.get_interface_version());

        register_natives!(
            &sys,
            &myself,
            [
                ("Rust.Rust", native_obj_new),          //
                ("Rust.Add", native_obj_add),           //
                ("Rust.Result.get", native_obj_result), //
            ]
        );

        Ok(())
    }
}
