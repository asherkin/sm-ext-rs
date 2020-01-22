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

use sm_ext::{cell_t, native, register_natives, HandleError, HandleId, HandleType, IExtension, IExtensionInterface, IHandleSys, IPluginContext, IShareSys, SMExtension, SMInterfaceApi, TryIntoPlugin};
use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;

#[derive(Debug)]
struct RustContext(i32);

impl<'ctx> TryIntoPlugin<'ctx> for RustContext {
    type Error = HandleError;

    fn try_into_plugin(self, ctx: &'ctx IPluginContext) -> Result<cell_t, Self::Error> {
        let object = Rc::new(RefCell::new(self));
        let handle = MyExtension::handle_type().create_handle(object, ctx.get_identity())?;

        Ok(handle.into())
    }
}

#[native]
fn native_obj_new(_ctx: &IPluginContext) -> RustContext {
    println!(">>> Rust.Rust()");

    RustContext(0)
}

#[native]
fn native_obj_add(ctx: &IPluginContext, this: HandleId, number: i32) -> Result<(), Box<dyn Error>> {
    println!(">>> Rust.Add({:?}, {:?})", this, number);

    let this = MyExtension::handle_type().read_handle(this, ctx.get_identity())?;
    let mut this = this.try_borrow_mut()?;

    this.0 += number;

    Ok(())
}

#[native]
fn native_obj_result(ctx: &IPluginContext, this: HandleId) -> Result<i32, Box<dyn Error>> {
    println!(">>> Rust.Result({:?})", this);

    let this = MyExtension::handle_type().read_handle(this, ctx.get_identity())?;
    let this = this.try_borrow()?;

    Ok(this.0)
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension {
    handle_type: Option<HandleType<RefCell<RustContext>>>,
}

impl MyExtension {
    /// Helper to get the extension singleton from the global provided by sm-ext.
    /// This is implemented here rather than by the SMExtension derive to aid code completion.
    fn get() -> &'static Self {
        EXTENSION_GLOBAL.with(|ext| unsafe { &(*ext.borrow().unwrap()).delegate })
    }

    fn handle_type() -> &'static HandleType<RefCell<RustContext>> {
        Self::get().handle_type.as_ref().unwrap()
    }
}

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), Box<dyn std::error::Error>> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let handlesys: IHandleSys = sys.request_interface(&myself)?;
        println!(">>> Got interface: {:?} v{:?}", handlesys.get_interface_name(), handlesys.get_interface_version());

        self.handle_type = Some(handlesys.create_type("RustContext", myself.get_identity())?);

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
