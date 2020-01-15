//! An example showing data storage on the extension singleton.
//!
//! ```sourcepawn
//! typedef RustCallback = function int(int a, const char[] b, float c);
//!
//! native int Rust_Test(RustCallback func);
//!
//! public int Callback(int a, const char[] b, float c)
//! {
//!     PrintToServer(">>> %d, \"%s\", %.2f", a, b, c);
//!
//!     return 42;
//! }
//!
//! public void OnPluginStart()
//! {
//!     int result = Rust_Test(Callback);
//!
//!     PrintToServer(">>> Rust_Test() = %d", result);
//! }
//! ```

use sm_ext::{c_str, cell_t, native, register_natives, ExecType, ExecutableApi, GameFrameHookId, IExtension, IExtensionInterface, IForwardManager, IPluginContext, IPluginFunction, IShareSys, ISourceMod, ParamType, SMExtension, SMInterfaceApi};
use std::error::Error;
use std::ffi::CString;

#[native]
fn test_native(ctx: &IPluginContext, mut func: IPluginFunction) -> Result<cell_t, Box<dyn Error>> {
    MyExtension::log_message(format!("Log message from Rust native! {:?} {:?}", ctx, func));

    println!(">>> test_native, func = {:?}", func);

    func.push(0)?;
    func.push(c_str!("Hello"))?;
    func.push(5.0)?;
    let result = func.execute()?;
    println!(">>> func() = {:?}", result);

    // Admittedly, this is a little gross.
    let mut forward = MyExtension::get().forwardsys.as_ref().unwrap().create_private_forward(None, ExecType::Single, &[ParamType::Cell, ParamType::String, ParamType::Float])?;
    assert_eq!(forward.get_function_count(), 0);

    forward.add_function(&mut func);
    assert_eq!(forward.get_function_count(), 1);

    forward.push(0)?;
    forward.push(c_str!("Hello"))?;
    forward.push(5.0)?;
    let result = forward.execute()?;
    println!(">>> forward() = {:?}", result);

    Ok(result)
}

// Just used to avoid spamming stdout.
static mut UNSAFE_COUNTER: i32 = 0;

unsafe extern "C" fn on_game_frame(_simulating: bool) {
    if UNSAFE_COUNTER > 5 {
        return;
    }

    println!(">>> on_game_frame fired!");

    UNSAFE_COUNTER += 1;
}

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension {
    myself: Option<IExtension>,
    sharesys: Option<IShareSys>,
    forwardsys: Option<IForwardManager>,
    sourcemod: Option<ISourceMod>,
    frame_hook: Option<GameFrameHookId>,
}

impl MyExtension {
    /// Helper to get the extension singleton from the global provided by sm-ext.
    /// This is implemented here rather than by the SMExtension derive to aid code completion.
    fn get() -> &'static Self {
        EXTENSION_GLOBAL.with(|ext| unsafe { &(*ext.borrow().unwrap()).delegate })
    }

    fn log_message(msg: String) {
        Self::get().sourcemod.as_ref().unwrap().log_message(Self::get().myself.as_ref().unwrap(), msg);
    }
}

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        let forward_manager: IForwardManager = sys.request_interface(&myself).map_err(|_| c_str!("Failed to get IForwardManager interface"))?;
        println!(">>> Got interface: {:?} v{:?}", forward_manager.get_interface_name(), forward_manager.get_interface_version());

        let sourcemod: ISourceMod = sys.request_interface(&myself).map_err(|_| c_str!("Failed to get ISourceMod interface"))?;
        println!(">>> Got interface: {:?} v{:?}", sourcemod.get_interface_name(), sourcemod.get_interface_version());

        sourcemod.add_frame_action(|| {
            println!(">>> add_frame_action callback fired!");
        });

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
