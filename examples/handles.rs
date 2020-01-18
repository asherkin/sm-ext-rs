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

use sm_ext::{c_str, cell_t, native, register_natives, AssociatedHandleType, HandleError, HandleRef, HandleType, IExtension, IExtensionInterface, IHandleSys, IPluginContext, IShareSys, SMExtension, SMInterfaceApi, TryIntoPlugin};
use std::ffi::CString;

#[derive(Debug)]
struct RustContext(i32);

impl AssociatedHandleType for RustContext {
    fn handle_type<'ty>() -> &'ty HandleType<Self> {
        MyExtension::handle_type()
    }
}

impl<'ctx> TryIntoPlugin<'ctx> for RustContext {
    type Error = HandleError;

    fn try_into_plugin(self, ctx: &'ctx IPluginContext) -> Result<cell_t, Self::Error> {
        let handle = MyExtension::handle_type().create(self, ctx.get_identity())?;

        Ok(handle.into())
    }
}

#[native]
fn native_obj_new(_ctx: &IPluginContext) -> RustContext {
    println!(">>> Rust.Rust()");

    RustContext(0)
}

#[native]
fn native_obj_add(_ctx: &IPluginContext, mut this: HandleRef<RustContext>, number: i32) {
    println!(">>> Rust.Add({:?}, {:?})", this, number);

    this.0 += number;
}

#[native]
fn native_obj_result(_ctx: &IPluginContext, this: HandleRef<RustContext>) -> i32 {
    println!(">>> Rust.Result({:?})", this);

    this.0
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension {
    handle_type: Option<HandleType<RustContext>>,
}

impl MyExtension {
    /// Helper to get the extension singleton from the global provided by sm-ext.
    /// This is implemented here rather than by the SMExtension derive to aid code completion.
    fn get() -> &'static Self {
        EXTENSION_GLOBAL.with(|ext| unsafe { &(*ext.borrow().unwrap()).delegate })
    }

    fn handle_type() -> &'static HandleType<RustContext> {
        Self::get().handle_type.as_ref().unwrap()
    }
}

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let handlesys: IHandleSys = sys.request_interface(&myself).map_err(|_| c_str!("Failed to get IHandleSys interface"))?;
        println!(">>> Got interface: {:?} v{:?}", handlesys.get_interface_name(), handlesys.get_interface_version());

        let handle_type: HandleType<RustContext> = handlesys.create_type("RustContext", myself.get_identity()).map_err(|_| c_str!("Failed to create RustContext Handle type"))?;
        self.handle_type = Some(handle_type);

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

    fn on_extension_unload(&mut self) {
        self.handle_type = None;
    }
}
