//! Minimal example of a valid SourceMod extension.

use sm_ext::{IExtension, IExtensionInterface, IShareSys, SMExtension};
use std::ffi::CString;

#[derive(Default, SMExtension)]
#[extension(name = "Rusty", description = "Sample extension written in Rust")]
pub struct MyExtension;

impl IExtensionInterface for MyExtension {
    fn on_extension_load(&mut self, myself: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        println!(">>> Rusty extension loaded! me = {:?}, sys = {:?}, late = {:?}", myself, sys, late);

        Ok(())
    }
}
