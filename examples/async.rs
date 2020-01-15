//! An example showing integration with asynchronous futures.
//!
//! ```sourcepawn
//! typedef RustCallback = function void(int a);
//!
//! native void Rust_Test(RustCallback func);
//!
//! public void Callback(int a)
//! {
//!     PrintToServer(">>> %d", a);
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
use sm_ext::{c_str, native, register_natives, ExecType, ExecutableApi, GameFrameHookId, IExtension, IExtensionInterface, IForwardManager, IPluginContext, IPluginFunction, IShareSys, ISourceMod, ParamType, SMExtension, SMInterfaceApi};
use std::cell::RefCell;
use std::error::Error;
use std::ffi::CString;
use std::time::Duration;

#[native]
fn test_native(_ctx: &IPluginContext, mut func: IPluginFunction) -> Result<(), Box<dyn Error>> {
    let mut forward = MyExtension::forwardsys().create_private_forward(None, ExecType::Single, &[ParamType::Cell])?;
    forward.add_function(&mut func);

    MyExtension::spawner().spawn_local(async move {
        task::sleep(Duration::from_secs(5)).await;

        let future = async move {
            forward.push(0)?;
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

    fn spawner() -> LocalSpawner {
        Self::get().pool.borrow().spawner()
    }
}

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let forward_manager: IForwardManager = sys.request_interface(&myself).map_err(|_| c_str!("Failed to get IForwardManager interface"))?;
        println!(">>> Got interface: {:?} v{:?}", forward_manager.get_interface_name(), forward_manager.get_interface_version());

        let sourcemod: ISourceMod = sys.request_interface(&myself).map_err(|_| c_str!("Failed to get ISourceMod interface"))?;
        println!(">>> Got interface: {:?} v{:?}", sourcemod.get_interface_name(), sourcemod.get_interface_version());

        self.frame_hook = Some(sourcemod.add_game_frame_hook(on_game_frame));

        register_natives!(
            &sys,
            &myself,
            [
                ("Rust_Test", test_native), //
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
    }
}
