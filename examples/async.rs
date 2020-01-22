//! An example showing integration with asynchronous futures.
//!
//! ```sourcepawn
//! methodmap RustHandle < Handle {
//!     public native void Read();
//! }
//!
//! typedef RustCallback = function void(RustHandle a);
//!
//! native void Rust_Test(RustCallback func);
//!
//! public void Callback(RustHandle a)
//! {
//!     PrintToServer(">>> %d -> %d", a, a.Read());
//!
//!     delete a;
//! }
//!
//! public void OnPluginStart()
//! {
//!     Rust_Test(Callback);
//! }
//! ```

use async_std::task;
use futures::executor::{LocalPool, LocalSpawner};
use futures::task::LocalSpawnExt;
use sm_ext::{native, register_natives, CallableParam, ExecType, Executable, GameFrameHookId, HandleId, HandleType, IExtension, IExtensionInterface, IForwardManager, IHandleSys, IPluginContext, IPluginFunction, IShareSys, ISourceMod, SMExtension, SMInterfaceApi};
use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;
use std::time::Duration;

struct IntHandle(i32);

#[native]
fn native_handle_read(ctx: &IPluginContext, this: HandleId) -> Result<i32, Box<dyn Error>> {
    let this = MyExtension::handle_type().read_handle(this, ctx.get_identity())?;
    let this = this.try_borrow()?;

    Ok(this.0)
}

#[native]
fn test_native(ctx: &IPluginContext, mut func: IPluginFunction) -> Result<(), Box<dyn Error>> {
    let mut forward = MyExtension::forwardsys().create_private_forward(None, ExecType::Ignore, &[HandleId::param_type()])?;
    forward.add_function(&mut func);

    let this = Rc::new(RefCell::new(IntHandle(0)));
    let handle = MyExtension::handle_type().create_handle(this.clone(), ctx.get_identity())?;

    MyExtension::spawner().spawn_local(async move {
        let future = async move {
            task::sleep(Duration::from_secs(5)).await;

            {
                let mut this = this.try_borrow_mut()?;
                this.0 = 42;
            }

            forward.push(handle)?;
            forward.execute()?;

            Ok(())
        };

        if let Result::<(), Box<dyn Error>>::Err(e) = future.await {
            // Call error callback.
            println!(">>> Async error: {}", e);
        }
    })?;

    Ok(())
}

extern "C" fn on_game_frame(_simulating: bool) {
    // TODO: See if we need to catch panics.
    MyExtension::get().pool.borrow_mut().run_until_stalled()
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension {
    myself: Option<IExtension>,
    sharesys: Option<IShareSys>,
    forwardsys: Option<IForwardManager>,
    sourcemod: Option<ISourceMod>,
    frame_hook: Option<GameFrameHookId>,
    handle_type: Option<HandleType<RefCell<IntHandle>>>,
    pool: RefCell<LocalPool>,
}

impl MyExtension {
    /// Helper to get the extension singleton from the global provided by sm-ext.
    /// This is implemented here rather than by the SMExtension derive to aid code completion.
    fn get() -> &'static Self {
        EXTENSION_GLOBAL.with(|ext| unsafe { &(*ext.borrow().unwrap()).delegate })
    }

    fn forwardsys() -> &'static IForwardManager {
        Self::get().forwardsys.as_ref().unwrap()
    }

    fn handle_type() -> &'static HandleType<RefCell<IntHandle>> {
        Self::get().handle_type.as_ref().unwrap()
    }

    fn spawner() -> LocalSpawner {
        Self::get().pool.borrow().spawner()
    }
}

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), Box<dyn std::error::Error>> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let forward_manager: IForwardManager = sys.request_interface(&myself)?;
        println!(">>> Got interface: {:?} v{:?}", forward_manager.get_interface_name(), forward_manager.get_interface_version());

        let sourcemod: ISourceMod = sys.request_interface(&myself)?;
        println!(">>> Got interface: {:?} v{:?}", sourcemod.get_interface_name(), sourcemod.get_interface_version());

        let handlesys: IHandleSys = sys.request_interface(&myself)?;
        println!(">>> Got interface: {:?} v{:?}", handlesys.get_interface_name(), handlesys.get_interface_version());

        self.frame_hook = Some(sourcemod.add_game_frame_hook(on_game_frame));

        self.handle_type = Some(handlesys.create_type("IntHandle", myself.get_identity())?);

        register_natives!(
            &sys,
            &myself,
            [
                ("Rust_Test", test_native),              //
                ("RustHandle.Read", native_handle_read), //
            ]
        );

        self.myself = Some(myself);
        self.sharesys = Some(sys);
        self.forwardsys = Some(forward_manager);
        self.sourcemod = Some(sourcemod);

        Ok(())
    }

    fn on_extension_unload(&mut self) {
        self.frame_hook = None;
        self.handle_type = None;
    }
}
