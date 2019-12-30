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

use sm_ext::{c_str, cell_t, native, register_natives, HandleError, HandleType, IExtension, IExtensionInterface, IHandleSys, IPluginContext, IShareSys, SMExtension, SMInterfaceApi, TryFromPlugin, TryIntoPlugin};
use std::cell::RefCell;
use std::ffi::CString;

#[derive(Debug)]
struct RustContext(i32);

thread_local! {
    static HANDLE_TYPE: RefCell<Option<HandleType<RustContext>>> = RefCell::new(None);
}

impl<'a> TryFromPlugin<'a, cell_t> for &'a mut RustContext {
    type Error = HandleError;

    fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        HANDLE_TYPE.with(|ty| -> Result<&'a mut RustContext, HandleError> {
            let ty = ty.borrow();
            let ty = ty.as_ref().unwrap();
            ty.read(value.into(), ctx.get_identity())
        })
    }
}

impl<'a> TryIntoPlugin<'a, cell_t> for RustContext {
    type Error = HandleError;

    fn try_into_plugin(self, ctx: &'a IPluginContext) -> Result<cell_t, Self::Error> {
        HANDLE_TYPE.with(|ty| {
            let ty = ty.borrow();
            let ty = ty.as_ref().unwrap();
            let handle = ty.create(self, ctx.get_identity())?;
            Ok(handle.into())
        })
    }
}

#[native]
fn native_obj_new(_ctx: &IPluginContext) -> RustContext {
    println!(">>> Rust.Rust()");

    RustContext(0)
}

#[native]
fn native_obj_add(_ctx: &IPluginContext, this: &mut RustContext, number: i32) {
    println!(">>> Rust.Add({:?}, {:?})", this, number);

    this.0 += number;
}

#[native]
fn native_obj_result(_ctx: &IPluginContext, this: &mut RustContext) -> i32 {
    println!(">>> Rust.Result({:?})", this);

    this.0
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension;

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let handlesys: IHandleSys = sys.request_interface(&myself).map_err(|_| c_str!("Failed to get IHandleSys interface"))?;
        println!(">>> Got interface: {:?} v{:?}", handlesys.get_interface_name(), handlesys.get_interface_version());

        let handle_type: HandleType<RustContext> = handlesys.create_type("RustContext", myself.get_identity()).map_err(|_| c_str!("Failed to create RustContext Handle type"))?;
        HANDLE_TYPE.with(|ty| {
            *ty.borrow_mut() = Some(handle_type);
        });

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
        HANDLE_TYPE.with(|ty| {
            *ty.borrow_mut() = None;
        });
    }
}
