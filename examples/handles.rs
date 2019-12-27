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

use sm_ext::{c_str, native, register_natives, HandleError, HandleId, HandleType, IExtension, IExtensionInterface, IHandleSys, IPluginContext, IShareSys, SMExtension, SMInterfaceApi};
use std::cell::RefCell;
use std::ffi::CString;

struct RustContext(i32);

thread_local! {
    static HANDLE_TYPE: RefCell<Option<HandleType<RustContext>>> = RefCell::new(None);
}

#[native]
fn native_obj_new(ctx: &IPluginContext) -> Result<HandleId, HandleError> {
    println!(">>> Rust.Rust()");

    HANDLE_TYPE.with(|ty| {
        let ty = ty.borrow();
        let ty = ty.as_ref().unwrap();
        ty.create(RustContext(0), ctx.get_identity())
    })
}

#[native]
fn native_obj_add(ctx: &IPluginContext, this: HandleId, number: i32) -> Result<(), HandleError> {
    println!(">>> Rust.Add({:?}, {:?})", this, number);

    HANDLE_TYPE.with(|ty| -> Result<(), HandleError> {
        let ty = ty.borrow();
        let ty = ty.as_ref().unwrap();
        let this = ty.read(this, ctx.get_identity())?;

        this.0 += number;

        Ok(())
    })
}

#[native]
fn native_obj_result(ctx: &IPluginContext, this: HandleId) -> Result<i32, HandleError> {
    println!(">>> Rust.Result({:?})", this);

    HANDLE_TYPE.with(|ty| -> Result<i32, HandleError> {
        let ty = ty.borrow();
        let ty = ty.as_ref().unwrap();
        let this = ty.read(this, ctx.get_identity())?;

        Ok(this.0)
    })
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
