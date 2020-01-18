#![cfg_attr(feature = "abi_thiscall", feature(abi_thiscall))]
#![allow(non_snake_case, non_camel_case_types, unused_variables)]
//! This interface is extremely unstable, everything just lives in a soup at the top level for now.

use std::convert::TryFrom;
use std::ffi::{CStr, CString, NulError};
use std::os::raw::{c_char, c_int, c_uint, c_void};
use std::ptr::{null, null_mut};
use std::str::Utf8Error;

pub use c_str_macro::c_str;
pub use libc::size_t;

pub use sm_ext_derive::{forwards, native, vtable, vtable_override, ICallableApi, SMExtension, SMInterfaceApi};
use std::ops::{Deref, DerefMut};

#[repr(transparent)]
pub struct IdentityType(c_uint);

#[repr(C)]
pub enum FeatureType {
    Native = 0,
    Capability = 1,
}

#[repr(C)]
pub enum FeatureStatus {
    Available = 0,
    Unavailable = 1,
    Unknown = 2,
}

// TODO: Investigate using a `union` for this instead.
/// Wrapper type that represents a value from SourcePawn.
///
/// Could be a [`i32`], [`f32`], `&i32`, `&f32`, or `&i8` (for character strings).
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct cell_t(i32);

impl std::fmt::Display for cell_t {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.0.fmt(f)
    }
}

/// Trait to support conversions to/from [`cell_t`] that require an [`IPluginContext`] for access to plugin memory.
pub trait TryFromPlugin<'ctx, T = cell_t>: Sized {
    type Error;

    fn try_from_plugin(ctx: &'ctx crate::IPluginContext, value: T) -> Result<Self, Self::Error>;
}

impl<T, U> TryFromPlugin<'_, T> for U
where
    U: TryFrom<T>,
{
    type Error = U::Error;

    fn try_from_plugin(ctx: &IPluginContext, value: T) -> Result<Self, Self::Error> {
        TryFrom::try_from(value)
    }
}

/// Trait to support conversions to/from [`cell_t`] that require an [`IPluginContext`] for access to plugin memory.
///
/// As with Rust's [`TryInto`](std::convert::TryInto) and [`TryFrom`](std::convert::TryFrom), this is implemented automatically
/// for types that implement [`TryFromPlugin`] which you should prefer to implement instead.
pub trait TryIntoPlugin<'ctx, T = cell_t>: Sized {
    type Error;

    fn try_into_plugin(self, ctx: &'ctx IPluginContext) -> Result<T, Self::Error>;
}

impl<'ctx, T, U> TryIntoPlugin<'ctx, U> for T
where
    U: TryFromPlugin<'ctx, T>,
{
    type Error = U::Error;

    fn try_into_plugin(self, ctx: &'ctx IPluginContext) -> Result<U, U::Error> {
        U::try_from_plugin(ctx, self)
    }
}

impl From<i32> for cell_t {
    fn from(x: i32) -> Self {
        cell_t(x)
    }
}

impl From<cell_t> for i32 {
    fn from(x: cell_t) -> Self {
        x.0
    }
}

impl From<f32> for cell_t {
    fn from(x: f32) -> Self {
        cell_t(x.to_bits() as i32)
    }
}

impl From<cell_t> for f32 {
    fn from(x: cell_t) -> Self {
        f32::from_bits(x.0 as u32)
    }
}

impl<'ctx> TryFromPlugin<'ctx> for &'ctx CStr {
    type Error = SPError;

    fn try_from_plugin(ctx: &'ctx IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        Ok(ctx.local_to_string(value)?)
    }
}

impl<'ctx> TryFromPlugin<'ctx> for &'ctx str {
    type Error = Box<dyn std::error::Error>;

    fn try_from_plugin(ctx: &'ctx IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        Ok(ctx.local_to_string(value)?.to_str()?)
    }
}

// TODO: These &mut implementations seem risky, maybe a SPRef/SPString/SPArray wrapper object would be a better way to go...

impl<'ctx> TryFromPlugin<'ctx> for &'ctx mut cell_t {
    type Error = SPError;

    fn try_from_plugin(ctx: &'ctx IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        Ok(ctx.local_to_phys_addr(value)?)
    }
}

impl<'ctx> TryFromPlugin<'ctx> for &'ctx mut i32 {
    type Error = SPError;

    fn try_from_plugin(ctx: &'ctx IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        let cell: &mut cell_t = value.try_into_plugin(ctx)?;
        unsafe { Ok(&mut *(cell as *mut cell_t as *mut i32)) }
    }
}

impl<'ctx> TryFromPlugin<'ctx> for &'ctx mut f32 {
    type Error = SPError;

    fn try_from_plugin(ctx: &'ctx IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        let cell: &mut cell_t = value.try_into_plugin(ctx)?;
        unsafe { Ok(&mut *(cell as *mut cell_t as *mut f32)) }
    }
}

/// Struct to contain name/fnptr pairs for native registration.
///
/// SourceMod has very strict lifetime requirements for this data and you should not construct
/// instances of this type yourself - use the [`register_natives!`] macro instead.
#[repr(C)]
pub struct NativeInfo {
    pub name: *const c_char,
    pub func: Option<unsafe extern "C" fn(ctx: IPluginContextPtr, args: *const cell_t) -> cell_t>,
}

pub struct IdentityToken {
    _private: [u8; 0],
}

pub type IdentityTokenPtr = *mut IdentityToken;

pub type IExtensionInterfacePtr = *mut *mut IExtensionInterfaceVtable;

#[vtable(IExtensionInterfacePtr)]
pub struct IExtensionInterfaceVtable {
    pub GetExtensionVersion: fn() -> i32,
    pub OnExtensionLoad: fn(me: IExtensionPtr, sys: IShareSysPtr, error: *mut c_char, maxlength: size_t, late: bool) -> bool,
    pub OnExtensionUnload: fn() -> (),
    pub OnExtensionsAllLoaded: fn() -> (),
    pub OnExtensionPauseChange: fn(pause: bool) -> (),
    pub QueryInterfaceDrop: fn(interface: SMInterfacePtr) -> bool,
    pub NotifyInterfaceDrop: fn(interface: SMInterfacePtr) -> (),
    pub QueryRunning: fn(error: *mut c_char, maxlength: size_t) -> bool,
    pub IsMetamodExtension: fn() -> bool,
    pub GetExtensionName: fn() -> *const c_char,
    pub GetExtensionURL: fn() -> *const c_char,
    pub GetExtensionTag: fn() -> *const c_char,
    pub GetExtensionAuthor: fn() -> *const c_char,
    pub GetExtensionVerString: fn() -> *const c_char,
    pub GetExtensionDescription: fn() -> *const c_char,
    pub GetExtensionDateString: fn() -> *const c_char,
    pub OnCoreMapStart: fn(edict_list: *mut c_void, edict_count: c_int, client_max: c_int) -> (),
    pub OnDependenciesDropped: fn() -> (),
    pub OnCoreMapEnd: fn() -> (),
}

// There appears to be a bug with the MSVC linker in release mode dropping these symbols when threaded
// compilation is enabled - if you run into undefined symbol errors here try setting code-units to 1.
pub trait IExtensionInterface {
    fn on_extension_load(&mut self, me: IExtension, sys: IShareSys, late: bool) -> Result<(), CString> {
        Ok(())
    }
    fn on_extension_unload(&mut self) {}
    fn on_extensions_all_loaded(&mut self) {}
    fn on_extension_pause_change(&mut self, pause: bool) {}
    fn on_core_map_start(&mut self, edict_list: *mut c_void, edict_count: i32, client_max: i32) {}
    fn on_core_map_end(&mut self) {}
    fn query_interface_drop(&mut self, interface: SMInterface) -> bool {
        false
    }
    fn notify_interface_drop(&mut self, interface: SMInterface) {}
    fn query_running(&mut self) -> Result<(), CString> {
        Ok(())
    }
    fn on_dependencies_dropped(&mut self) {}
}

pub trait ExtensionMetadata {
    fn get_extension_name(&self) -> &'static CStr;
    fn get_extension_url(&self) -> &'static CStr;
    fn get_extension_tag(&self) -> &'static CStr;
    fn get_extension_author(&self) -> &'static CStr;
    fn get_extension_ver_string(&self) -> &'static CStr;
    fn get_extension_description(&self) -> &'static CStr;
    fn get_extension_date_string(&self) -> &'static CStr;
}

#[repr(C)]
pub struct IExtensionInterfaceAdapter<T: IExtensionInterface + ExtensionMetadata> {
    vtable: *mut IExtensionInterfaceVtable,
    pub delegate: T,
}

impl<T: IExtensionInterface + ExtensionMetadata> Drop for IExtensionInterfaceAdapter<T> {
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(self.vtable));
        }
    }
}

// TODO: The implementations in here need to catch panics in the delegate functions.
impl<T: IExtensionInterface + ExtensionMetadata> IExtensionInterfaceAdapter<T> {
    pub fn new(delegate: T) -> IExtensionInterfaceAdapter<T> {
        let vtable = IExtensionInterfaceVtable {
            GetExtensionVersion: IExtensionInterfaceAdapter::<T>::get_extension_version,
            OnExtensionLoad: IExtensionInterfaceAdapter::<T>::on_extension_load,
            OnExtensionUnload: IExtensionInterfaceAdapter::<T>::on_extension_unload,
            OnExtensionsAllLoaded: IExtensionInterfaceAdapter::<T>::on_extensions_all_loaded,
            OnExtensionPauseChange: IExtensionInterfaceAdapter::<T>::on_extension_pause_change,
            QueryInterfaceDrop: IExtensionInterfaceAdapter::<T>::query_interface_drop,
            NotifyInterfaceDrop: IExtensionInterfaceAdapter::<T>::notify_interface_drop,
            QueryRunning: IExtensionInterfaceAdapter::<T>::query_running,
            IsMetamodExtension: IExtensionInterfaceAdapter::<T>::is_metamod_extension,
            GetExtensionName: IExtensionInterfaceAdapter::<T>::get_extension_name,
            GetExtensionURL: IExtensionInterfaceAdapter::<T>::get_extension_url,
            GetExtensionTag: IExtensionInterfaceAdapter::<T>::get_extension_tag,
            GetExtensionAuthor: IExtensionInterfaceAdapter::<T>::get_extension_author,
            GetExtensionVerString: IExtensionInterfaceAdapter::<T>::get_extension_ver_string,
            GetExtensionDescription: IExtensionInterfaceAdapter::<T>::get_extension_description,
            GetExtensionDateString: IExtensionInterfaceAdapter::<T>::get_extension_date_string,
            OnCoreMapStart: IExtensionInterfaceAdapter::<T>::on_core_map_start,
            OnDependenciesDropped: IExtensionInterfaceAdapter::<T>::on_dependencies_dropped,
            OnCoreMapEnd: IExtensionInterfaceAdapter::<T>::on_core_map_end,
        };

        IExtensionInterfaceAdapter { vtable: Box::into_raw(Box::new(vtable)), delegate }
    }

    #[vtable_override]
    unsafe fn get_extension_version(this: IExtensionInterfacePtr) -> i32 {
        8
    }

    #[vtable_override]
    unsafe fn on_extension_load(this: IExtensionInterfacePtr, me: IExtensionPtr, sys: IShareSysPtr, error: *mut c_char, maxlength: size_t, late: bool) -> bool {
        match (*this.cast::<Self>()).delegate.on_extension_load(IExtension(me), IShareSys(sys), late) {
            Ok(_) => true,
            Err(str) => {
                libc::strncpy(error, str.as_ptr(), maxlength);
                false
            }
        }
    }

    #[vtable_override]
    unsafe fn on_extension_unload(this: IExtensionInterfacePtr) {
        (*this.cast::<Self>()).delegate.on_extension_unload()
    }

    #[vtable_override]
    unsafe fn on_extensions_all_loaded(this: IExtensionInterfacePtr) {
        (*this.cast::<Self>()).delegate.on_extensions_all_loaded()
    }

    #[vtable_override]
    unsafe fn on_extension_pause_change(this: IExtensionInterfacePtr, pause: bool) {
        (*this.cast::<Self>()).delegate.on_extension_pause_change(pause)
    }

    #[vtable_override]
    unsafe fn query_interface_drop(this: IExtensionInterfacePtr, interface: SMInterfacePtr) -> bool {
        (*this.cast::<Self>()).delegate.query_interface_drop(SMInterface(interface))
    }

    #[vtable_override]
    unsafe fn notify_interface_drop(this: IExtensionInterfacePtr, interface: SMInterfacePtr) {
        (*this.cast::<Self>()).delegate.notify_interface_drop(SMInterface(interface))
    }

    #[vtable_override]
    unsafe fn query_running(this: IExtensionInterfacePtr, error: *mut c_char, maxlength: size_t) -> bool {
        match (*this.cast::<Self>()).delegate.query_running() {
            Ok(_) => true,
            Err(str) => {
                libc::strncpy(error, str.as_ptr(), maxlength);
                false
            }
        }
    }

    #[vtable_override]
    unsafe fn is_metamod_extension(this: IExtensionInterfacePtr) -> bool {
        false
    }

    #[vtable_override]
    unsafe fn get_extension_name(this: IExtensionInterfacePtr) -> *const c_char {
        (*this.cast::<Self>()).delegate.get_extension_name().as_ptr()
    }

    #[vtable_override]
    unsafe fn get_extension_url(this: IExtensionInterfacePtr) -> *const c_char {
        (*this.cast::<Self>()).delegate.get_extension_url().as_ptr()
    }

    #[vtable_override]
    unsafe fn get_extension_tag(this: IExtensionInterfacePtr) -> *const c_char {
        (*this.cast::<Self>()).delegate.get_extension_tag().as_ptr()
    }

    #[vtable_override]
    unsafe fn get_extension_author(this: IExtensionInterfacePtr) -> *const c_char {
        (*this.cast::<Self>()).delegate.get_extension_author().as_ptr()
    }

    #[vtable_override]
    unsafe fn get_extension_ver_string(this: IExtensionInterfacePtr) -> *const c_char {
        (*this.cast::<Self>()).delegate.get_extension_ver_string().as_ptr()
    }

    #[vtable_override]
    unsafe fn get_extension_description(this: IExtensionInterfacePtr) -> *const c_char {
        (*this.cast::<Self>()).delegate.get_extension_description().as_ptr()
    }

    #[vtable_override]
    unsafe fn get_extension_date_string(this: IExtensionInterfacePtr) -> *const c_char {
        (*this.cast::<Self>()).delegate.get_extension_date_string().as_ptr()
    }

    #[vtable_override]
    unsafe fn on_core_map_start(this: IExtensionInterfacePtr, edict_list: *mut c_void, edict_count: c_int, client_max: c_int) {
        (*this.cast::<Self>()).delegate.on_core_map_start(edict_list, edict_count, client_max)
    }

    #[vtable_override]
    unsafe fn on_dependencies_dropped(this: IExtensionInterfacePtr) {
        (*this.cast::<Self>()).delegate.on_dependencies_dropped()
    }

    #[vtable_override]
    unsafe fn on_core_map_end(this: IExtensionInterfacePtr) {
        (*this.cast::<Self>()).delegate.on_core_map_end()
    }
}

pub type IExtensionPtr = *mut *mut IExtensionVtable;

#[vtable(IExtensionPtr)]
pub struct IExtensionVtable {
    pub IsLoaded: fn() -> bool,
    pub GetAPI: fn() -> IExtensionInterfacePtr,
    pub GetFilename: fn() -> *const c_char,
    pub GetIdentity: fn() -> IdentityTokenPtr,
    _FindFirstDependency: fn() -> *mut c_void,
    _FindNextDependency: fn() -> *mut c_void,
    _FreeDependencyIterator: fn() -> *mut c_void,
    pub IsRunning: fn(error: *mut c_char, maxlength: size_t) -> bool,
    pub IsExternal: fn() -> bool,
}

#[derive(Debug)]
pub enum IsRunningError<'str> {
    WithReason(&'str str),
    InvalidReason(Utf8Error),
}

impl std::fmt::Display for IsRunningError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Debug::fmt(self, f)
    }
}

impl std::error::Error for IsRunningError<'_> {}

#[derive(Debug)]
pub struct IExtension(IExtensionPtr);

impl IExtension {
    pub fn is_loaded(&self) -> bool {
        unsafe { virtual_call!(IsLoaded, self.0) }
    }

    pub fn get_api(&self) -> IExtensionInterfacePtr {
        unsafe { virtual_call!(GetAPI, self.0) }
    }

    pub fn get_filename(&self) -> Result<&str, Utf8Error> {
        unsafe {
            let c_name = virtual_call!(GetFilename, self.0);

            CStr::from_ptr(c_name).to_str()
        }
    }

    pub fn get_identity(&self) -> IdentityTokenPtr {
        unsafe { virtual_call!(GetIdentity, self.0) }
    }

    pub fn is_running(&self) -> Result<(), IsRunningError> {
        unsafe {
            let mut c_error = [0 as c_char; 256];
            let result = virtual_call!(IsRunning, self.0, c_error.as_mut_ptr(), c_error.len());

            if result {
                Ok(())
            } else {
                match CStr::from_ptr(c_error.as_ptr()).to_str() {
                    Ok(error) => Err(IsRunningError::WithReason(error)),
                    Err(e) => Err(IsRunningError::InvalidReason(e)),
                }
            }
        }
    }

    pub fn is_external(&self) -> bool {
        unsafe { virtual_call!(IsExternal, self.0) }
    }
}

pub type SMInterfacePtr = *mut *mut SMInterfaceVtable;

#[vtable(SMInterfacePtr)]
pub struct SMInterfaceVtable {
    pub GetInterfaceVersion: fn() -> c_uint,
    pub GetInterfaceName: fn() -> *const c_char,
    pub IsVersionCompatible: fn(version: c_uint) -> bool,
}

pub trait RequestableInterface {
    fn get_interface_name() -> &'static str;
    fn get_interface_version() -> u32;

    /// # Safety
    ///
    /// Only for use internally by [`IShareSys::request_interface`], which always knows the correct type.
    unsafe fn from_raw_interface(iface: SMInterface) -> Self;
}

pub trait SMInterfaceApi {
    fn get_interface_version(&self) -> u32;
    fn get_interface_name(&self) -> &str;
    fn is_version_compatible(&self, version: u32) -> bool;
}

#[derive(Debug, SMInterfaceApi)]
pub struct SMInterface(SMInterfacePtr);

pub type IFeatureProviderPtr = *mut *mut IFeatureProviderVtable;

#[vtable(IFeatureProviderPtr)]
pub struct IFeatureProviderVtable {}

pub type IPluginRuntimePtr = *mut *mut IPluginRuntimeVtable;

#[vtable(IPluginRuntimePtr)]
pub struct IPluginRuntimeVtable {}

pub type IShareSysPtr = *mut *mut IShareSysVtable;

#[vtable(IShareSysPtr)]
pub struct IShareSysVtable {
    pub AddInterface: fn(myself: IExtensionPtr, iface: SMInterfacePtr) -> bool,
    pub RequestInterface: fn(iface_name: *const c_char, iface_vers: c_uint, myself: IExtensionPtr, iface: *mut SMInterfacePtr) -> bool,
    pub AddNatives: fn(myself: IExtensionPtr, natives: *const NativeInfo) -> (),
    pub CreateIdentType: fn(name: *const c_char) -> IdentityType,
    pub FindIdentType: fn(name: *const c_char) -> IdentityType,
    pub CreateIdentity: fn(ident_type: IdentityType, ptr: *mut c_void) -> IdentityTokenPtr,
    pub DestroyIdentType: fn(ident_type: IdentityType) -> (),
    pub DestroyIdentity: fn(identity: IdentityTokenPtr) -> (),
    pub AddDependency: fn(myself: IExtensionPtr, filename: *const c_char, require: bool, autoload: bool) -> (),
    pub RegisterLibrary: fn(myself: IExtensionPtr, name: *const c_char) -> (),
    _OverrideNatives: fn(myself: IExtensionPtr, natives: *const NativeInfo) -> (),
    pub AddCapabilityProvider: fn(myself: IExtensionPtr, provider: IFeatureProviderPtr, name: *const c_char) -> (),
    pub DropCapabilityProvider: fn(myself: IExtensionPtr, provider: IFeatureProviderPtr, name: *const c_char) -> (),
    pub TestFeature: fn(rt: IPluginRuntimePtr, feature_type: FeatureType, name: *const c_char) -> FeatureStatus,
}

#[derive(Debug)]
pub enum RequestInterfaceError {
    InvalidName(NulError),
    InvalidInterface(),
}

impl std::fmt::Display for RequestInterfaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Debug::fmt(self, f)
    }
}

impl std::error::Error for RequestInterfaceError {}

#[derive(Debug)]
pub struct IShareSys(IShareSysPtr);

impl IShareSys {
    pub fn request_interface<I: RequestableInterface>(&self, myself: &IExtension) -> Result<I, RequestInterfaceError> {
        let iface = self.request_raw_interface(myself, I::get_interface_name(), I::get_interface_version())?;

        unsafe { Ok(I::from_raw_interface(iface)) }
    }

    pub fn request_raw_interface(&self, myself: &IExtension, name: &str, version: u32) -> Result<SMInterface, RequestInterfaceError> {
        let c_name = CString::new(name).map_err(RequestInterfaceError::InvalidName)?;

        unsafe {
            let mut iface: SMInterfacePtr = null_mut();
            let res = virtual_call!(RequestInterface, self.0, c_name.as_ptr(), version, myself.0, &mut iface);

            if res {
                Ok(SMInterface(iface))
            } else {
                Err(RequestInterfaceError::InvalidInterface())
            }
        }
    }

    /// # Safety
    ///
    /// This should be be used via the [`register_natives!`] macro only.
    pub unsafe fn add_natives(&self, myself: &IExtension, natives: *const NativeInfo) {
        virtual_call!(AddNatives, self.0, myself.0, natives)
    }
}

/// Error codes for SourcePawn routines.
#[repr(C)]
#[derive(Debug)]
pub enum SPError {
    /// No error occurred
    None = 0,
    /// File format unrecognized
    FileFormat = 1,
    /// A decompressor was not found
    Decompressor = 2,
    /// Not enough space left on the heap
    HeapLow = 3,
    /// Invalid parameter or parameter type
    Param = 4,
    /// A memory address was not valid
    InvalidAddress = 5,
    /// The object in question was not found
    NotFound = 6,
    /// Invalid index parameter
    Index = 7,
    /// Not enough space left on the stack
    StackLow = 8,
    /// Debug mode was not on or debug section not found
    NotDebugging = 9,
    /// Invalid instruction was encountered
    InvalidInstruction = 10,
    /// Invalid memory access
    MemAccess = 11,
    /// Stack went beyond its minimum value
    StackMin = 12,
    /// Heap went beyond its minimum value
    HeapMin = 13,
    /// Division by zero
    DivideByZero = 14,
    /// Array index is out of bounds
    ArrayBounds = 15,
    /// Instruction had an invalid parameter
    InstructionParam = 16,
    /// A native leaked an item on the stack
    StackLeak = 17,
    /// A native leaked an item on the heap
    HeapLeak = 18,
    /// A dynamic array is too big
    ArrayTooBig = 19,
    /// Tracker stack is out of bounds
    TrackerBounds = 20,
    /// Native was pending or invalid
    InvalidNative = 21,
    /// Maximum number of parameters reached
    ParamsMax = 22,
    /// Error originates from a native
    Native = 23,
    /// Function or plugin is not runnable
    NotRunnable = 24,
    /// Function call was aborted
    Aborted = 25,
    /// Code is too old for this VM
    CodeTooOld = 26,
    /// Code is too new for this VM
    CodeTooNew = 27,
    /// Out of memory
    OutOfMemory = 28,
    /// Integer overflow (-INT_MIN / -1)
    IntegerOverflow = 29,
    /// Timeout
    Timeout = 30,
    /// Custom message
    User = 31,
    /// Custom fatal message
    Fatal = 32,
}

impl std::fmt::Display for SPError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.pad(match self {
            SPError::None => "no error occurred",
            SPError::FileFormat => "unrecognizable file format",
            SPError::Decompressor => "decompressor was not found",
            SPError::HeapLow => "not enough space on the heap",
            SPError::Param => "invalid parameter or parameter type",
            SPError::InvalidAddress => "invalid plugin address",
            SPError::NotFound => "object or index not found",
            SPError::Index => "invalid index or index not found",
            SPError::StackLow => "not enough space on the stack",
            SPError::NotDebugging => "debug section not found or debug not enabled",
            SPError::InvalidInstruction => "invalid instruction",
            SPError::MemAccess => "invalid memory access",
            SPError::StackMin => "stack went below stack boundary",
            SPError::HeapMin => "heap went below heap boundary",
            SPError::DivideByZero => "divide by zero",
            SPError::ArrayBounds => "array index is out of bounds",
            SPError::InstructionParam => "instruction contained invalid parameter",
            SPError::StackLeak => "stack memory leaked by native",
            SPError::HeapLeak => "heap memory leaked by native",
            SPError::ArrayTooBig => "dynamic array is too big",
            SPError::TrackerBounds => "tracker stack is out of bounds",
            SPError::InvalidNative => "native is not bound",
            SPError::ParamsMax => "maximum number of parameters reached",
            SPError::Native => "native detected error",
            SPError::NotRunnable => "plugin not runnable",
            SPError::Aborted => "call was aborted",
            SPError::CodeTooOld => "plugin format is too old",
            SPError::CodeTooNew => "plugin format is too new",
            SPError::OutOfMemory => "out of memory",
            SPError::IntegerOverflow => "integer overflow",
            SPError::Timeout => "script execution timed out",
            SPError::User => "custom error",
            SPError::Fatal => "fatal error",
        })
    }
}

impl std::error::Error for SPError {}

pub type IPluginContextPtr = *mut *mut IPluginContextVtable;

#[vtable(IPluginContextPtr)]
pub struct IPluginContextVtable {
    _Destructor: fn() -> (),
    #[cfg(not(windows))]
    _Destructor2: fn() -> (),
    _GetVirtualMachine: fn(),
    _GetContext: fn(),
    _IsDebugging: fn(),
    _SetDebugBreak: fn(),
    _GetDebugInfo: fn(),
    _HeapAlloc: fn(),
    _HeapPop: fn(),
    _HeapRelease: fn(),
    _FindNativeByName: fn(),
    _GetNativeByIndex: fn(),
    _GetNativesNum: fn(),
    _FindPublicByName: fn(),
    _GetPublicByIndex: fn(),
    _GetPublicsNum: fn(),
    _GetPubvarByIndex: fn(),
    _FindPubvarByName: fn(),
    _GetPubvarAddrs: fn(),
    _GetPubVarsNum: fn(),
    pub LocalToPhysAddr: fn(local_addr: cell_t, phys_addr: *mut *mut cell_t) -> SPError,
    pub LocalToString: fn(local_addr: cell_t, addr: *mut *mut c_char) -> SPError,
    _StringToLocal: fn(),
    _StringToLocalUTF8: fn(),
    _PushCell: fn(),
    _PushCellArray: fn(),
    _PushString: fn(),
    _PushCellsFromArray: fn(),
    _BindNatives: fn(),
    _BindNative: fn(),
    _BindNativeToAny: fn(),
    _Execute: fn(),
    _ThrowNativeErrorEx: fn(),
    pub ThrowNativeError: fn(*const c_char, ...) -> cell_t,
    pub GetFunctionByName: fn(public_name: *const c_char) -> IPluginFunctionPtr,
    pub GetFunctionById: fn(func_id: u32) -> IPluginFunctionPtr,
    pub GetIdentity: fn() -> IdentityTokenPtr,
    _GetNullRef: fn(),
    _LocalToStringNULL: fn(),
    _BindNativeToIndex: fn(),
    _IsInExec: fn(),
    _GetRuntime: fn(),
    _Execute2: fn(),
    _GetLastNativeError: fn(),
    _GetLocalParams: fn(),
    _SetKey: fn(),
    _GetKey: fn(),
    _ClearLastNativeError: fn(),
    _APIv2: fn(),
    _ReportError: fn(),
    _ReportErrorVA: fn(),
    _ReportFatalError: fn(),
    _ReportFatalErrorVA: fn(),
    _ReportErrorNumber: fn(),
    _BlamePluginError: fn(),
    _CreateFrameIterator: fn(),
    _DestroyFrameIterator: fn(),
}

#[derive(Debug)]
pub struct IPluginContext(IPluginContextPtr);

#[derive(Debug)]
pub enum GetFunctionError {
    UnknownFunction,
}

impl std::fmt::Display for GetFunctionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Debug::fmt(self, f)
    }
}

impl std::error::Error for GetFunctionError {}

impl IPluginContext {
    pub fn local_to_phys_addr(&self, local: cell_t) -> Result<&mut cell_t, SPError> {
        unsafe {
            let mut addr: *mut cell_t = null_mut();
            let res = virtual_call!(LocalToPhysAddr, self.0, local, &mut addr);

            match res {
                SPError::None => Ok(&mut *addr),
                _ => Err(res),
            }
        }
    }

    pub fn local_to_string(&self, local: cell_t) -> Result<&CStr, SPError> {
        unsafe {
            let mut addr: *mut c_char = null_mut();
            let res = virtual_call!(LocalToString, self.0, local, &mut addr);

            match res {
                SPError::None => Ok(CStr::from_ptr(addr)),
                _ => Err(res),
            }
        }
    }

    pub fn throw_native_error(&self, err: String) -> cell_t {
        let fmt = c_str!("%s");
        let err = CString::new(err).unwrap_or_else(|_| c_str!("ThrowNativeError message contained NUL byte").into());
        unsafe { virtual_call_varargs!(ThrowNativeError, self.0, fmt.as_ptr(), err.as_ptr()) }
    }

    pub fn get_function_by_id(&self, func_id: u32) -> Result<IPluginFunction, GetFunctionError> {
        unsafe {
            let function = virtual_call!(GetFunctionById, self.0, func_id);
            if function.is_null() {
                Err(GetFunctionError::UnknownFunction)
            } else {
                Ok(IPluginFunction(function, self))
            }
        }
    }

    pub fn get_identity(&self) -> IdentityTokenPtr {
        unsafe { virtual_call!(GetIdentity, self.0) }
    }
}

pub type IPluginFunctionPtr = *mut *mut IPluginFunctionVtable;

#[vtable(IPluginFunctionPtr)]
pub struct IPluginFunctionVtable {
    // ICallable
    pub PushCell: fn(cell: cell_t) -> SPError,
    pub PushCellByRef: fn(cell: *mut cell_t, flags: c_int) -> SPError,
    pub PushFloat: fn(number: f32) -> SPError,
    pub PushFloatByRef: fn(number: *mut f32, flags: c_int) -> SPError,
    pub PushArray: fn(cell: *mut cell_t, cells: c_uint, flags: c_int) -> SPError,
    pub PushString: fn(string: *const c_char) -> SPError,
    pub PushStringEx: fn(string: *const c_char, length: size_t, sz_flags: c_int, cp_flags: c_int) -> SPError,
    pub Cancel: fn(),

    // IPluginFunction
    pub Execute: fn(result: *mut cell_t) -> SPError,
    _CallFunction: fn(),
    _GetParentContext: fn(),
    pub IsRunnable: fn() -> bool,
    pub GetFunctionID: fn() -> u32,
    _Execute2: fn(),
    _CallFunction2: fn(),
    _GetParentRuntime: fn(),
    pub Invoke: fn(rval: *mut cell_t) -> bool,
    pub DebugName: fn() -> *const c_char,
}

#[derive(Debug, ICallableApi)]
pub struct IPluginFunction<'ctx>(IPluginFunctionPtr, &'ctx IPluginContext);

impl Executable for IPluginFunction<'_> {
    fn execute(&mut self) -> Result<cell_t, SPError> {
        unsafe {
            let mut result: cell_t = 0.into();
            let res = virtual_call!(Execute, self.0, &mut result);
            match res {
                SPError::None => Ok(result),
                _ => Err(res),
            }
        }
    }
}

impl<'ctx> TryFromPlugin<'ctx> for IPluginFunction<'ctx> {
    type Error = GetFunctionError;

    fn try_from_plugin(ctx: &'ctx IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        ctx.get_function_by_id(value.0 as u32)
    }
}

/// Defines how a forward iterates through plugin functions.
#[repr(C)]
pub enum ExecType {
    /// Ignore all return values, return 0
    Ignore = 0,
    /// Only return the last exec, ignore all others
    Single = 1,
    /// Acts as an event with the ResultTypes above, no mid-Stops allowed, returns highest
    Event = 2,
    /// Acts as a hook with the ResultTypes above, mid-Stops allowed, returns highest
    Hook = 3,
    /// Same as Event except that it returns the lowest value
    LowEvent = 4,
}

/// Describes the various ways to pass parameters to plugins.
#[repr(C)]
pub enum ParamType {
    /// Any data type can be pushed
    Any = 0,
    /// Only basic cells can be pushed
    Cell = (1 << 1),
    /// Only floats can be pushed
    Float = (2 << 1),
    /// Only strings can be pushed
    String = (3 << 1) | 1,
    /// Only arrays can be pushed
    Array = (4 << 1) | 1,
    /// Same as "..." in plugins, anything can be pushed, but it will always be byref
    VarArgs = (5 << 1),
    /// Only a cell by reference can be pushed
    CellByRef = (1 << 1) | 1,
    /// Only a float by reference can be pushed
    FloatByRef = (2 << 1) | 1,
}

pub type IForwardPtr = *mut *mut IForwardVtable;

#[vtable(IForwardPtr)]
pub struct IForwardVtable {
    // ICallable
    pub PushCell: fn(cell: cell_t) -> SPError,
    pub PushCellByRef: fn(cell: *mut cell_t, flags: c_int) -> SPError,
    pub PushFloat: fn(number: f32) -> SPError,
    pub PushFloatByRef: fn(number: *mut f32, flags: c_int) -> SPError,
    pub PushArray: fn(cell: *mut cell_t, cells: c_uint, flags: c_int) -> SPError,
    pub PushString: fn(string: *const c_char) -> SPError,
    pub PushStringEx: fn(string: *const c_char, length: size_t, sz_flags: c_int, cp_flags: c_int) -> SPError,
    pub Cancel: fn(),

    // IForward
    _Destructor: fn() -> (),
    #[cfg(not(windows))]
    _Destructor2: fn() -> (),
    pub GetForwardName: fn() -> *const c_char,
    pub GetFunctionCount: fn() -> c_uint,
    pub GetExecType: fn() -> ExecType,
    pub Execute: fn(result: *mut cell_t, filter: *mut c_void) -> SPError,
}

pub type IChangeableForwardPtr = *mut *mut IChangeableForwardVtable;

#[vtable(IChangeableForwardPtr)]
pub struct IChangeableForwardVtable {
    // ICallable
    pub PushCell: fn(cell: cell_t) -> SPError,
    pub PushCellByRef: fn(cell: *mut cell_t, flags: c_int) -> SPError,
    pub PushFloat: fn(number: f32) -> SPError,
    pub PushFloatByRef: fn(number: *mut f32, flags: c_int) -> SPError,
    pub PushArray: fn(cell: *mut cell_t, cells: c_uint, flags: c_int) -> SPError,
    pub PushString: fn(string: *const c_char) -> SPError,
    pub PushStringEx: fn(string: *const c_char, length: size_t, sz_flags: c_int, cp_flags: c_int) -> SPError,
    pub Cancel: fn(),

    // IForward
    _Destructor: fn() -> (),
    #[cfg(not(windows))]
    _Destructor2: fn() -> (),
    pub GetForwardName: fn() -> *const c_char,
    pub GetFunctionCount: fn() -> c_uint,
    pub GetExecType: fn() -> ExecType,
    pub Execute: fn(result: *mut cell_t, filter: *mut c_void) -> SPError,

    // IChangeableForward
    #[cfg(windows)]
    pub RemoveFunctionById: fn(ctx: IPluginContextPtr, func: u32) -> bool,
    pub RemoveFunction: fn(func: IPluginFunctionPtr) -> bool,
    _RemoveFunctionsOfPlugin: fn(),
    #[cfg(windows)]
    pub AddFunctionById: fn(ctx: IPluginContextPtr, func: u32) -> bool,
    pub AddFunction: fn(func: IPluginFunctionPtr) -> bool,
    #[cfg(not(windows))]
    pub AddFunctionById: fn(ctx: IPluginContextPtr, func: u32) -> bool,
    #[cfg(not(windows))]
    pub RemoveFunctionById: fn(ctx: IPluginContextPtr, func: u32) -> bool,
}

pub trait CallableParam {
    fn push<T: ICallableApi>(&self, callable: &mut T) -> Result<(), SPError>;
    fn param_type() -> ParamType;
}

impl CallableParam for cell_t {
    fn push<T: ICallableApi>(&self, callable: &mut T) -> Result<(), SPError> {
        callable.push_int(self.0)
    }

    fn param_type() -> ParamType {
        ParamType::Cell
    }
}

impl CallableParam for i32 {
    fn push<T: ICallableApi>(&self, callable: &mut T) -> Result<(), SPError> {
        callable.push_int(*self)
    }

    fn param_type() -> ParamType {
        ParamType::Cell
    }
}

impl CallableParam for f32 {
    fn push<T: ICallableApi>(&self, callable: &mut T) -> Result<(), SPError> {
        callable.push_float(*self)
    }

    fn param_type() -> ParamType {
        ParamType::Float
    }
}

impl CallableParam for &CStr {
    fn push<T: ICallableApi>(&self, callable: &mut T) -> Result<(), SPError> {
        callable.push_string(self)
    }

    fn param_type() -> ParamType {
        ParamType::String
    }
}

// TODO: This interface is very, very rough.
pub trait ICallableApi {
    fn push_int(&mut self, cell: i32) -> Result<(), SPError>;
    fn push_float(&mut self, number: f32) -> Result<(), SPError>;
    fn push_string(&mut self, string: &CStr) -> Result<(), SPError>;
}

pub trait Executable: ICallableApi + Sized {
    fn execute(&mut self) -> Result<cell_t, SPError>;

    fn push<T: CallableParam>(&mut self, param: T) -> Result<(), SPError> {
        param.push(self)
    }
}

#[derive(Debug, ICallableApi)]
pub struct Forward(IForwardPtr, IForwardManagerPtr);

impl Drop for Forward {
    fn drop(&mut self) {
        IForwardManager(self.1).release_forward(&mut self.0);
    }
}

impl Executable for Forward {
    fn execute(&mut self) -> Result<cell_t, SPError> {
        unsafe {
            let mut result: cell_t = 0.into();
            let res = virtual_call!(Execute, self.0, &mut result, null_mut());
            match res {
                SPError::None => Ok(result),
                _ => Err(res),
            }
        }
    }
}

impl Forward {
    pub fn get_function_count(&self) -> u32 {
        unsafe { virtual_call!(GetFunctionCount, self.0) }
    }
}

#[derive(Debug, ICallableApi)]
pub struct ChangeableForward(IChangeableForwardPtr, IForwardManagerPtr);

impl Drop for ChangeableForward {
    fn drop(&mut self) {
        IForwardManager(self.1).release_forward(&mut (self.0 as IForwardPtr));
    }
}

impl Executable for ChangeableForward {
    fn execute(&mut self) -> Result<cell_t, SPError> {
        unsafe {
            let mut result: cell_t = 0.into();
            let res = virtual_call!(Execute, self.0, &mut result, null_mut());
            match res {
                SPError::None => Ok(result),
                _ => Err(res),
            }
        }
    }
}

impl ChangeableForward {
    pub fn get_function_count(&self) -> u32 {
        unsafe { virtual_call!(GetFunctionCount, self.0) }
    }

    pub fn add_function(&mut self, func: &mut IPluginFunction) {
        unsafe {
            virtual_call!(AddFunction, self.0, func.0);
        }
    }

    pub fn remove_function(&mut self, func: &mut IPluginFunction) {
        unsafe {
            virtual_call!(RemoveFunction, self.0, func.0);
        }
    }
}

pub type IForwardManagerPtr = *mut *mut IForwardManagerVtable;

#[vtable(IForwardManagerPtr)]
pub struct IForwardManagerVtable {
    // SMInterface
    pub GetInterfaceVersion: fn() -> c_uint,
    pub GetInterfaceName: fn() -> *const c_char,
    pub IsVersionCompatible: fn(version: c_uint) -> bool,

    // IForwardManager
    pub CreateForward: fn(name: *const c_char, et: ExecType, num_params: c_uint, types: *const ParamType, ...) -> IForwardPtr,
    pub CreateForwardEx: fn(name: *const c_char, et: ExecType, num_params: c_uint, types: *const ParamType, ...) -> IChangeableForwardPtr,
    pub FindForward: fn(name: *const c_char, *mut IChangeableForwardPtr) -> IForwardPtr,
    pub ReleaseForward: fn(forward: IForwardPtr) -> (),
}

#[derive(Debug)]
pub enum CreateForwardError {
    InvalidName(NulError),
    InvalidParams,
}

impl std::fmt::Display for CreateForwardError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Debug::fmt(self, f)
    }
}

impl std::error::Error for CreateForwardError {}

#[derive(Debug, SMInterfaceApi)]
#[interface("IForwardManager", 4)]
pub struct IForwardManager(IForwardManagerPtr);

impl IForwardManager {
    pub fn create_global_forward(&self, name: &str, et: ExecType, params: &[ParamType]) -> Result<Forward, CreateForwardError> {
        let c_name = CString::new(name).map_err(CreateForwardError::InvalidName)?;

        unsafe {
            let forward = virtual_call_varargs!(CreateForward, self.0, c_name.as_ptr(), et, params.len() as u32, params.as_ptr());

            if forward.is_null() {
                Err(CreateForwardError::InvalidParams)
            } else {
                Ok(Forward(forward, self.0))
            }
        }
    }

    pub fn create_private_forward(&self, name: Option<&str>, et: ExecType, params: &[ParamType]) -> Result<ChangeableForward, CreateForwardError> {
        let c_name = match name {
            Some(name) => Some(CString::new(name).map_err(CreateForwardError::InvalidName)?),
            None => None,
        };

        let c_name = match c_name {
            Some(c_name) => c_name.as_ptr(),
            None => null(),
        };

        unsafe {
            let forward = virtual_call_varargs!(CreateForwardEx, self.0, c_name, et, params.len() as u32, params.as_ptr());

            if forward.is_null() {
                Err(CreateForwardError::InvalidParams)
            } else {
                Ok(ChangeableForward(forward, self.0))
            }
        }
    }

    fn release_forward(&self, forward: &mut IForwardPtr) {
        if forward.is_null() {
            panic!("release_forward called on null forward ptr")
        }

        unsafe {
            virtual_call!(ReleaseForward, self.0, *forward);
            *forward = null_mut();
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct HandleTypeId(c_uint);

impl HandleTypeId {
    pub fn is_valid(self) -> bool {
        self.0 != 0
    }

    pub fn invalid() -> Self {
        Self(0)
    }
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct HandleId(c_uint);

impl HandleId {
    pub fn is_valid(self) -> bool {
        self.0 != 0
    }

    pub fn invalid() -> Self {
        Self(0)
    }
}

impl From<cell_t> for HandleId {
    fn from(x: cell_t) -> Self {
        Self(x.0 as u32)
    }
}

impl From<HandleId> for cell_t {
    fn from(x: HandleId) -> Self {
        Self(x.0 as i32)
    }
}

impl CallableParam for HandleId {
    fn push<T: ICallableApi>(&self, callable: &mut T) -> Result<(), SPError> {
        callable.push_int(self.0 as i32)
    }

    fn param_type() -> ParamType {
        ParamType::Cell
    }
}

/// Lists the possible handle error codes.
#[repr(C)]
#[derive(Debug)]
pub enum HandleError {
    /// No error
    None = 0,
    /// The handle has been freed and reassigned
    Changed = 1,
    /// The handle has a different type registered
    Type = 2,
    /// The handle has been freed
    Freed = 3,
    /// Generic internal indexing error
    Index = 4,
    /// No access permitted to free this handle
    Access = 5,
    /// The limited number of handles has been reached
    Limit = 6,
    /// The identity token was not usable
    Identity = 7,
    /// Owners do not match for this operation
    Owner = 8,
    /// Unrecognized security structure version
    Version = 9,
    /// An invalid parameter was passed
    Parameter = 10,
    /// This type cannot be inherited
    NoInherit = 11,
}

impl std::fmt::Display for HandleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.pad(match self {
            HandleError::None => "no error",
            HandleError::Changed => "the handle has been freed and reassigned",
            HandleError::Type => "the handle has a different type registered",
            HandleError::Freed => "the handle has been freed",
            HandleError::Index => "generic internal indexing error",
            HandleError::Access => "no access permitted to free this handle",
            HandleError::Limit => "the limited number of handles has been reached",
            HandleError::Identity => "the identity token was not usable",
            HandleError::Owner => "owners do not match for this operation",
            HandleError::Version => "unrecognized security structure version",
            HandleError::Parameter => "an invalid parameter was passed",
            HandleError::NoInherit => "this type cannot be inherited",
        })
    }
}

impl std::error::Error for HandleError {}

pub type IHandleTypeDispatchPtr = *mut *mut IHandleTypeDispatchVtable;

#[vtable(IHandleTypeDispatchPtr)]
pub struct IHandleTypeDispatchVtable {
    pub GetDispatchVersion: fn() -> c_uint,
    pub OnHandleDestroy: fn(ty: HandleTypeId, object: *mut c_void) -> (),
    pub GetHandleApproxSize: fn(ty: HandleTypeId, object: *mut c_void, size: *mut c_uint) -> bool,
}

#[repr(C)]
pub struct IHandleTypeDispatchAdapter<T> {
    vtable: *mut IHandleTypeDispatchVtable,
    phantom: std::marker::PhantomData<T>,
}

impl<T> Drop for IHandleTypeDispatchAdapter<T> {
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(self.vtable));
        }
    }
}

impl<T> Default for IHandleTypeDispatchAdapter<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IHandleTypeDispatchAdapter<T> {
    pub fn new() -> IHandleTypeDispatchAdapter<T> {
        let vtable = IHandleTypeDispatchVtable {
            GetDispatchVersion: IHandleTypeDispatchAdapter::<T>::get_dispatch_version,
            OnHandleDestroy: IHandleTypeDispatchAdapter::<T>::on_handle_destroy,
            GetHandleApproxSize: IHandleTypeDispatchAdapter::<T>::get_handle_approx_size,
        };

        IHandleTypeDispatchAdapter { vtable: Box::into_raw(Box::new(vtable)), phantom: std::marker::PhantomData }
    }

    #[vtable_override]
    unsafe fn get_dispatch_version(this: IHandleTypeDispatchPtr) -> u32 {
        <IHandleSys as RequestableInterface>::get_interface_version()
    }

    #[vtable_override]
    unsafe fn on_handle_destroy(this: IHandleTypeDispatchPtr, ty: HandleTypeId, object: *mut c_void) {
        drop(Box::from_raw(object as *mut T));
    }

    #[vtable_override]
    unsafe fn get_handle_approx_size(this: IHandleTypeDispatchPtr, ty: HandleTypeId, object: *mut c_void, size: *mut c_uint) -> bool {
        // This isn't ideal as it doesn't account for dynamic sizes, probably need to add a trait at some point
        // for people to implement this properly. See also: https://github.com/rust-lang/rust/issues/63073
        let object = object as *mut T;
        *size = std::mem::size_of_val(&*object) as u32;
        true
    }
}

/// This pair of tokens is used for identification.
#[repr(C)]
#[derive(Debug)]
pub struct HandleSecurity {
    /// Owner of the Handle
    pub owner: IdentityTokenPtr,
    /// Owner of the Type
    pub identity: IdentityTokenPtr,
}

impl HandleSecurity {
    pub fn new(owner: IdentityTokenPtr, identity: IdentityTokenPtr) -> Self {
        Self { owner, identity }
    }
}

pub type IHandleSysPtr = *mut *mut IHandleSysVtable;

#[vtable(IHandleSysPtr)]
pub struct IHandleSysVtable {
    // SMInterface
    pub GetInterfaceVersion: fn() -> c_uint,
    pub GetInterfaceName: fn() -> *const c_char,
    pub IsVersionCompatible: fn(version: c_uint) -> bool,

    // IHandleSys
    pub CreateType: fn(name: *const c_char, dispatch: IHandleTypeDispatchPtr, parent: HandleTypeId, typeAccess: *const c_void, handleAccess: *const c_void, ident: IdentityTokenPtr, err: *mut HandleError) -> HandleTypeId,
    pub RemoveType: fn(ty: HandleTypeId, ident: IdentityTokenPtr) -> bool,
    pub FindHandleType: fn(name: *const c_char, ty: *mut HandleTypeId) -> bool,
    pub CreateHandle: fn(ty: HandleTypeId, object: *mut c_void, owner: IdentityTokenPtr, ident: IdentityTokenPtr, err: *mut HandleError) -> HandleId,
    pub FreeHandle: fn(handle: HandleId, security: *const HandleSecurity) -> HandleError,
    pub CloneHandle: fn(handle: HandleId, newHandle: *mut HandleId, newOwner: IdentityTokenPtr, security: *const HandleSecurity) -> HandleError,
    pub ReadHandle: fn(handle: HandleId, ty: HandleTypeId, security: *const HandleSecurity, object: *mut *mut c_void) -> HandleError,
    pub InitAccessDefaults: fn(typeAccess: *mut c_void, handleAccess: *mut c_void) -> bool,
    pub CreateHandleEx: fn(ty: HandleTypeId, object: *mut c_void, security: *const HandleSecurity, access: *const c_void, err: *mut HandleError) -> HandleId,
    pub FastCloneHandle: fn(handle: HandleId) -> HandleId,
    pub TypeCheck: fn(given: HandleTypeId, actual: HandleTypeId) -> bool,
}

#[derive(Debug)]
pub struct HandleType<T> {
    iface: IHandleSysPtr,
    id: HandleTypeId,
    dispatch: *mut IHandleTypeDispatchAdapter<T>,
    ident: IdentityTokenPtr,
}

impl<T> Drop for HandleType<T> {
    fn drop(&mut self) {
        IHandleSys(self.iface).remove_type(self).unwrap();

        unsafe {
            drop(Box::from_raw(self.dispatch));
        }
    }
}

/// Implement this trait to allow automatic conversion to [`HandleRef`] from native arguments.
pub trait HasHandleType: Sized {
    fn handle_type<'ty>() -> &'ty HandleType<Self>;

    // TODO: Try and avoid having to read the ptr back from the handle.
    fn into_handle<'ty>(self) -> Result<HandleRef<'ty, Self>, HandleError> {
        let ty = Self::handle_type();
        let handle = ty.create_handle(self, ty.ident)?;
        let ptr = ty.read_handle(handle, ty.ident)?;

        Ok(HandleRef { ty, handle, ptr })
    }
}

impl<'ctx, 'ty, T: HasHandleType> TryFromPlugin<'ctx> for HandleRef<'ty, T> {
    type Error = HandleError;

    fn try_from_plugin(ctx: &'ctx IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        let ty = T::handle_type();
        let handle = HandleId::from(value);
        let owner = ctx.get_identity();

        HandleRef::new(ty, handle, owner)
    }
}

/// Wrapper for a [`HandleId`] that is [`Deref`] to the wrapping type.
pub struct HandleRef<'ty, T> {
    ty: &'ty HandleType<T>,
    handle: HandleId,
    ptr: *mut T,
}

impl<'ty, T> HandleRef<'ty, T> {
    pub fn new(ty: &'ty HandleType<T>, handle: HandleId, owner: IdentityTokenPtr) -> Result<Self, HandleError> {
        let ptr = ty.read_handle(handle, owner)?;
        let owned = ty.clone_handle(handle, owner, ty.ident)?;

        Ok(HandleRef { ty, handle: owned, ptr })
    }

    pub fn clone_handle(&self, new_owner: IdentityTokenPtr) -> Result<HandleId, HandleError> {
        self.ty.clone_handle(self.handle, self.ty.ident, new_owner)
    }
}

impl<T> Drop for HandleRef<'_, T> {
    fn drop(&mut self) {
        match self.ty.free_handle(self.handle, self.ty.ident) {
            Ok(_) => self.handle = HandleId::invalid(),
            Err(e) => panic!("invalid handle when dropping HandleRef: {}", e),
        }
    }
}

impl<T> Deref for HandleRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.ptr }
    }
}

/// This is unsound if multiple [`HandleRef`]s are created wrapping the same Handle data.
///
/// To guarantee safety, do not use mutable HandleRefs but instead use a [`std::cell::RefCell`] wrapper inside your [`HandleType`].
impl<T> DerefMut for HandleRef<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.ptr }
    }
}

impl<T> std::fmt::Debug for HandleRef<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "HandleRef({:08x}, {:?})", self.handle.0, self.ptr)
    }
}

impl<T> HandleType<T> {
    pub fn create_handle(&self, object: T, owner: IdentityTokenPtr) -> Result<HandleId, HandleError> {
        IHandleSys(self.iface).create_handle(self, object, owner)
    }

    pub fn clone_handle(&self, handle: HandleId, owner: IdentityTokenPtr, new_owner: IdentityTokenPtr) -> Result<HandleId, HandleError> {
        IHandleSys(self.iface).clone_handle(self, handle, owner, new_owner)
    }

    pub fn free_handle(&self, handle: HandleId, owner: IdentityTokenPtr) -> Result<(), HandleError> {
        IHandleSys(self.iface).free_handle(self, handle, owner)
    }

    pub fn read_handle(&self, handle: HandleId, owner: IdentityTokenPtr) -> Result<*mut T, HandleError> {
        IHandleSys(self.iface).read_handle(self, handle, owner)
    }
}

#[derive(Debug, SMInterfaceApi)]
#[interface("IHandleSys", 5)]
pub struct IHandleSys(IHandleSysPtr);

impl IHandleSys {
    pub fn create_type<T>(&self, name: &str, ident: IdentityTokenPtr) -> Result<HandleType<T>, HandleError> {
        unsafe {
            let c_name = CString::new(name).unwrap(); // TODO
            let dispatch = Box::into_raw(Box::new(IHandleTypeDispatchAdapter::<T>::new()));
            let mut err: HandleError = HandleError::None;
            let id = virtual_call!(CreateType, self.0, c_name.as_ptr(), dispatch as IHandleTypeDispatchPtr, HandleTypeId::invalid(), null(), null(), ident, &mut err);
            if id.is_valid() {
                Ok(HandleType { iface: self.0, id, dispatch, ident })
            } else {
                Err(err)
            }
        }
    }

    fn remove_type<T>(&self, ty: &mut HandleType<T>) -> Result<(), bool> {
        unsafe {
            if virtual_call!(RemoveType, self.0, ty.id, ty.ident) {
                Ok(())
            } else {
                Err(false)
            }
        }
    }

    fn create_handle<T>(&self, ty: &HandleType<T>, object: T, owner: IdentityTokenPtr) -> Result<HandleId, HandleError> {
        unsafe {
            let object = Box::into_raw(Box::new(object)) as *mut c_void;
            let security = HandleSecurity::new(owner, ty.ident);
            let mut err: HandleError = HandleError::None;
            let id = virtual_call!(CreateHandleEx, self.0, ty.id, object, &security, null(), &mut err);
            if id.is_valid() {
                Ok(id)
            } else {
                Err(err)
            }
        }
    }

    fn free_handle<T>(&self, ty: &HandleType<T>, handle: HandleId, owner: IdentityTokenPtr) -> Result<(), HandleError> {
        unsafe {
            let security = HandleSecurity::new(owner, ty.ident);
            let err = virtual_call!(FreeHandle, self.0, handle, &security);
            match err {
                HandleError::None => Ok(()),
                _ => Err(err),
            }
        }
    }

    fn clone_handle<T>(&self, ty: &HandleType<T>, handle: HandleId, owner: IdentityTokenPtr, new_owner: IdentityTokenPtr) -> Result<HandleId, HandleError> {
        unsafe {
            let security = HandleSecurity::new(owner, ty.ident);
            let mut new_handle = HandleId::invalid();
            let err = virtual_call!(CloneHandle, self.0, handle, &mut new_handle, new_owner, &security);
            match err {
                HandleError::None => Ok(new_handle),
                _ => Err(err),
            }
        }
    }

    fn read_handle<T>(&self, ty: &HandleType<T>, handle: HandleId, owner: IdentityTokenPtr) -> Result<*mut T, HandleError> {
        unsafe {
            let security = HandleSecurity::new(owner, ty.ident);
            let mut object: *mut c_void = null_mut();
            let err = virtual_call!(ReadHandle, self.0, handle, ty.id, &security, &mut object);
            match err {
                HandleError::None => Ok(object as *mut T),
                _ => Err(err),
            }
        }
    }
}

/// Describes various ways of formatting a base path.
#[repr(C)]
#[derive(Debug)]
pub enum PathType {
    /// No base path
    Path_None = 0,
    /// Base path is absolute mod folder
    Path_Game = 1,
    /// Base path is absolute to SourceMod
    Path_SM = 2,
    /// Base path is relative to SourceMod
    Path_SM_Rel = 3,
}

pub type GameFrameHookFunc = unsafe extern "C" fn(simulating: bool);

pub type ISourceModPtr = *mut *mut ISourceModVtable;

#[vtable(ISourceModPtr)]
pub struct ISourceModVtable {
    // SMInterface
    pub GetInterfaceVersion: fn() -> c_uint,
    pub GetInterfaceName: fn() -> *const c_char,
    pub IsVersionCompatible: fn(version: c_uint) -> bool,

    // ISourceMod
    pub GetGamePath: fn() -> *const c_char,
    pub GetSourceModPath: fn() -> *const c_char,
    pub BuildPath: fn(ty: PathType, buffer: *mut c_char, maxlength: size_t, format: *const c_char, ...) -> size_t,
    pub LogMessage: fn(ext: IExtensionPtr, format: *const c_char, ...) -> (),
    pub LogError: fn(ext: IExtensionPtr, format: *const c_char, ...) -> (),
    pub FormatString: fn(buffer: *mut c_char, maxlength: size_t, context: IPluginContextPtr, params: *const cell_t, param: c_uint) -> size_t,
    _CreateDataPack: fn(),
    _FreeDataPack: fn(),
    _GetDataPackHandleType: fn(),
    _ReadKeyValuesHandle: fn(),
    pub GetGameFolderName: fn() -> *const c_char,
    pub GetScriptingEngine: fn() -> *mut c_void,
    pub GetScriptingVM: fn() -> *mut c_void,
    _GetAdjustedTime: fn(),
    pub SetGlobalTarget: fn(index: c_uint) -> c_uint,
    pub GetGlobalTarget: fn() -> c_uint,
    pub AddGameFrameHook: fn(hook: GameFrameHookFunc) -> (),
    pub RemoveGameFrameHook: fn(hook: GameFrameHookFunc) -> (),
    pub Format: fn(buffer: *mut c_char, maxlength: size_t, format: *const c_char, ...) -> size_t,
    _FormatArgs: fn(),
    pub AddFrameAction: fn(func: unsafe extern "C" fn(*mut c_void), data: *mut c_void) -> (),
    pub GetCoreConfigValue: fn(key: *const c_char) -> *const c_char,
    pub GetPluginId: fn() -> c_int,
    pub GetShApiVersion: fn() -> c_int,
    pub IsMapRunning: fn() -> bool,
    pub FromPseudoAddress: fn(pseudo: u32) -> *mut c_void,
    pub ToPseudoAddress: fn(addr: *mut c_void) -> u32,
}

#[derive(Debug, SMInterfaceApi)]
#[interface("ISourceMod", 14)]
pub struct ISourceMod(ISourceModPtr);

pub struct GameFrameHookId(GameFrameHookFunc, ISourceModPtr);

impl Drop for GameFrameHookId {
    fn drop(&mut self) {
        ISourceMod(self.1).remove_game_frame_hook(self.0);
    }
}

unsafe extern "C" fn frame_action_trampoline<F: FnMut() + 'static>(func: *mut c_void) {
    let mut func = Box::<F>::from_raw(func as *mut _);
    (*func)()
}

impl ISourceMod {
    pub fn log_message(&self, myself: &IExtension, msg: String) {
        let fmt = c_str!("%s");
        let msg = CString::new(msg).expect("log message contained NUL byte");
        unsafe { virtual_call_varargs!(LogMessage, self.0, myself.0, fmt.as_ptr(), msg.as_ptr()) }
    }

    pub fn log_error(&self, myself: &IExtension, msg: String) {
        let fmt = c_str!("%s");
        let msg = CString::new(msg).expect("log message contained NUL byte");
        unsafe { virtual_call_varargs!(LogError, self.0, myself.0, fmt.as_ptr(), msg.as_ptr()) }
    }

    /// Add a function that will be called every game frame until the [`GameFrameHookId`] return value
    /// is dropped. This is a fairly low-level building block as the callback must be `extern "C"`.
    pub fn add_game_frame_hook(&self, hook: GameFrameHookFunc) -> GameFrameHookId {
        unsafe {
            virtual_call!(AddGameFrameHook, self.0, hook);
        }

        GameFrameHookId(hook, self.0)
    }

    fn remove_game_frame_hook(&self, hook: GameFrameHookFunc) {
        unsafe {
            virtual_call!(RemoveGameFrameHook, self.0, hook);
        }
    }

    // TODO: If we implement a [`Send`] subset of [`ISourceMod`] this function should be included but the closure must also be [`Send`].
    /// Add a function that will be called on the next game frame. This has a runtime cost as this API
    /// is thread-safe on the SM side, but it supports a Rust closure so is more flexible than [`ISourceMod::add_game_frame_hook`].
    pub fn add_frame_action<F>(&self, func: F)
    where
        F: FnMut() + 'static,
    {
        unsafe {
            let func = Box::into_raw(Box::new(func));
            virtual_call!(AddFrameAction, self.0, frame_action_trampoline::<F>, func as *mut c_void);
        }
    }
}

/// Helper for virtual function invocation that works with the `#[vtable]` attribute to support
/// virtual calls on Windows without compiler support for the `thiscall` calling convention.
#[macro_export]
macro_rules! virtual_call {
    ($name:ident, $this:expr, $($param:expr),* $(,)?) => {
        ((**$this).$name)(
            $this,
            #[cfg(all(windows, target_arch = "x86", not(feature = "abi_thiscall")))]
            std::ptr::null_mut(),
            $(
                $param,
            )*
        )
    };
    ($name:ident, $this:expr) => {
        virtual_call!($name, $this, )
    };
}

// TODO: Figure out a way to make this type-safe (and hopefully avoid the need for it completely.)
/// Helper for varargs-using virtual function invocation that works with the `#[vtable]` attribute to
/// support virtual calls on Windows without compiler support for the `thiscall` calling convention.
#[macro_export]
macro_rules! virtual_call_varargs {
    ($name:ident, $this:expr, $($param:expr),* $(,)?) => {
        ((**$this).$name)(
            $this,
            $(
                $param,
            )*
        )
    };
    ($name:ident, $this:expr) => {
        virtual_call!($name, $this, )
    };
}

#[macro_export]
macro_rules! register_natives {
    ($sys:expr, $myself:expr, [$(($name:expr, $func:expr)),* $(,)?]) => {
        unsafe {
            let mut vec = Vec::new();
            $(
                let name = concat!($name, "\0").as_ptr() as *const ::std::os::raw::c_char;
                vec.push($crate::NativeInfo {
                    name: name,
                    func: Some($func),
                });
            )*
            vec.push($crate::NativeInfo {
                name: ::std::ptr::null(),
                func: None,
            });

            // This leaks vec so that it remains valid.
            // TODO: Look into making it static somewhere, it only has to live as long as the extension is loaded.
            // Would probably need some of the nightly macro features, which tbh would help the native callbacks anyway.
            let boxed = vec.into_boxed_slice();
            $sys.add_natives($myself, Box::leak(boxed).as_ptr());
        }
    };
}

/// The return type for native callbacks.
pub trait NativeResult {
    type Ok;
    type Err;

    fn into_result(self) -> Result<Self::Ok, Self::Err>;
}

/// Dummy error used for [`NativeResult`] implementations that can never fail.
#[derive(Debug)]
pub struct DummyNativeError;

impl std::fmt::Display for DummyNativeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Debug::fmt(self, f)
    }
}

impl std::error::Error for DummyNativeError {}

impl NativeResult for () {
    type Ok = i32;
    type Err = DummyNativeError;

    fn into_result(self) -> Result<Self::Ok, Self::Err> {
        Ok(0)
    }
}

impl<'ctx, T> NativeResult for T
where
    T: TryIntoPlugin<'ctx, cell_t>,
{
    type Ok = T;
    type Err = DummyNativeError;

    fn into_result(self) -> Result<Self::Ok, Self::Err> {
        Ok(self)
    }
}

impl<E> NativeResult for Result<(), E> {
    type Ok = i32;
    type Err = E;

    #[allow(clippy::type_complexity)]
    fn into_result(self) -> Result<<Result<(), E> as NativeResult>::Ok, <Result<(), E> as NativeResult>::Err> {
        self.map(|_| 0)
    }
}

impl<'ctx, T, E> NativeResult for Result<T, E>
where
    T: TryIntoPlugin<'ctx, cell_t>,
{
    type Ok = T;
    type Err = E;

    #[allow(clippy::type_complexity)]
    fn into_result(self) -> Result<<Result<T, E> as NativeResult>::Ok, <Result<T, E> as NativeResult>::Err> {
        self
    }
}

/// Wrapper to invoke a native callback and translate a [`panic!`] or [`Err`](std::result::Result::Err)
/// return into a SourceMod error using [`IPluginContext::throw_native_error`].
///
/// This is used internally by the `#[native]` attribute.
pub fn safe_native_invoke<F>(ctx: IPluginContextPtr, f: F) -> cell_t
where
    F: FnOnce(&IPluginContext) -> Result<cell_t, Box<dyn std::error::Error>> + std::panic::UnwindSafe,
{
    let ctx = IPluginContext(ctx);
    let result = std::panic::catch_unwind(|| f(&ctx));

    match result {
        Ok(result) => match result {
            Ok(result) => result,
            Err(err) => ctx.throw_native_error(err.to_string()),
        },
        Err(err) => {
            let msg = format!(
                "native panicked: {}",
                if let Some(str_slice) = err.downcast_ref::<&'static str>() {
                    str_slice
                } else if let Some(string) = err.downcast_ref::<String>() {
                    string
                } else {
                    "unknown message"
                }
            );

            ctx.throw_native_error(msg)
        }
    }
}
