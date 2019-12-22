#![cfg_attr(feature = "thiscall", feature(abi_thiscall))]
#![allow(non_snake_case, non_camel_case_types, unused_variables)]
//! This interface is extremely unstable, everything just lives in a soup at the top level for now.

use std::convert::TryFrom;
use std::ffi::{CStr, CString, NulError};
use std::os::raw::{c_char, c_int, c_uchar, c_uint, c_void};
use std::ptr::null_mut;
use std::str::Utf8Error;

pub use c_str_macro::c_str;
pub use libc::size_t;

pub use sm_ext_derive::{native, vtable, vtable_override, SMExtension, SMInterfaceApi};

#[repr(transparent)]
pub struct IdentityType(c_uint);

// TODO: This should be a checked enum.
#[repr(transparent)]
pub struct FeatureType(c_uchar);

// TODO: This should be a checked enum.
#[repr(transparent)]
pub struct FeatureStatus(c_uchar);

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
pub trait TryFromPlugin<'a, T>: Sized {
    type Error;

    fn try_from_plugin(ctx: &'a crate::IPluginContext, value: T) -> Result<Self, Self::Error>;
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
pub trait TryIntoPlugin<'a, T>: Sized {
    type Error;

    fn try_into_plugin(self, ctx: &'a IPluginContext) -> Result<T, Self::Error>;
}

impl<'a, T, U> TryIntoPlugin<'a, U> for T
where
    U: TryFromPlugin<'a, T>,
{
    type Error = U::Error;

    fn try_into_plugin(self, ctx: &'a IPluginContext) -> Result<U, U::Error> {
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

impl<'a> TryFromPlugin<'a, cell_t> for &'a CStr {
    type Error = &'static str;

    fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        Ok(ctx.local_to_string(value).map_err(|_| "Invalid memory address")?)
    }
}

impl<'a> TryFromPlugin<'a, cell_t> for &'a str {
    type Error = Box<dyn std::error::Error>;

    fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        Ok(ctx.local_to_string(value).map_err(|_| "Invalid memory address")?.to_str()?)
    }
}

// TODO: These &mut implementations seem risky, maybe a SPRef/SPString/SPArray wrapper object would be a better way to go...

impl<'a> TryFromPlugin<'a, cell_t> for &'a mut cell_t {
    type Error = &'static str;

    fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        match ctx.local_to_phys_addr(value) {
            Ok(s) => Ok(s),
            Err(_) => Err("Invalid memory address"),
        }
    }
}

impl<'a> TryFromPlugin<'a, cell_t> for &'a mut i32 {
    type Error = &'static str;

    fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
        let cell: &mut cell_t = value.try_into_plugin(ctx)?;
        unsafe { Ok(&mut *(cell as *mut cell_t as *mut i32)) }
    }
}

impl<'a> TryFromPlugin<'a, cell_t> for &'a mut f32 {
    type Error = &'static str;

    fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
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

pub struct IdentityToken();

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

pub trait IExtensionMetadata {
    fn get_extension_name(&self) -> &'static CStr;
    fn get_extension_url(&self) -> &'static CStr;
    fn get_extension_tag(&self) -> &'static CStr;
    fn get_extension_author(&self) -> &'static CStr;
    fn get_extension_ver_string(&self) -> &'static CStr;
    fn get_extension_description(&self) -> &'static CStr;
    fn get_extension_date_string(&self) -> &'static CStr;
}

#[repr(C)]
pub struct IExtensionInterfaceAdapter<T: IExtensionInterface + IExtensionMetadata> {
    vtable: *mut IExtensionInterfaceVtable,
    delegate: T,
}

impl<T: IExtensionInterface + IExtensionMetadata> Drop for IExtensionInterfaceAdapter<T> {
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(self.vtable));
        }
    }
}

// TODO: The implementations in here need to catch panics in the delegate functions.
impl<T: IExtensionInterface + IExtensionMetadata> IExtensionInterfaceAdapter<T> {
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
pub enum IsRunningError<'a> {
    WithReason(&'a str),
    InvalidReason(Utf8Error),
}

#[derive(Debug)]
pub struct IExtension(pub IExtensionPtr);

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
    unsafe fn from_raw_interface(iface: SMInterface) -> Self;
}

pub trait SMInterfaceApi {
    fn get_interface_version(&self) -> u32;
    fn get_interface_name(&self) -> &str;
    fn is_version_compatible(&self, version: u32) -> bool;
}

#[derive(Debug, SMInterfaceApi)]
pub struct SMInterface(pub SMInterfacePtr);

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
    InvalidNameError(NulError),
    InterfaceError(),
}

#[derive(Debug)]
pub struct IShareSys(pub IShareSysPtr);

impl IShareSys {
    pub fn request_interface<I: RequestableInterface>(&self, myself: &IExtension) -> Result<I, RequestInterfaceError> {
        let iface = self.request_raw_interface(myself, I::get_interface_name(), I::get_interface_version())?;

        unsafe { Ok(I::from_raw_interface(iface)) }
    }

    pub fn request_raw_interface(&self, myself: &IExtension, name: &str, version: u32) -> Result<SMInterface, RequestInterfaceError> {
        let c_name = CString::new(name).map_err(RequestInterfaceError::InvalidNameError)?;

        unsafe {
            let mut iface: SMInterfacePtr = null_mut();
            let res = virtual_call!(RequestInterface, self.0, c_name.as_ptr(), version, myself.0, &mut iface);

            if res {
                Ok(SMInterface(iface))
            } else {
                Err(RequestInterfaceError::InterfaceError())
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
    pub LocalToPhysAddr: fn(local_addr: cell_t, phys_addr: *mut *mut cell_t) -> c_int,
    pub LocalToString: fn(local_addr: cell_t, addr: *mut *mut c_char) -> c_int,
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
    _GetFunctionByName: fn(),
    _GetFunctionById: fn(),
    _GetIdentity: fn(),
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
pub struct IPluginContext(pub IPluginContextPtr);

impl IPluginContext {
    pub fn throw_native_error(&self, err: String) -> cell_t {
        let fmt = c_str!("%s");
        let err = CString::new(err).unwrap_or_else(|_| c_str!("ThrowNativeError message contained NUL byte").into());
        unsafe { virtual_call_varargs!(ThrowNativeError, self.0, fmt.as_ptr(), err.as_ptr()) }
    }

    pub fn local_to_phys_addr(&self, local: cell_t) -> Result<&mut cell_t, i32> {
        unsafe {
            let mut addr: *mut cell_t = null_mut();
            let res = virtual_call!(LocalToPhysAddr, self.0, local, &mut addr);

            if res == 0 {
                Ok(&mut *addr)
            } else {
                Err(res)
            }
        }
    }

    pub fn local_to_string(&self, local: cell_t) -> Result<&CStr, i32> {
        unsafe {
            let mut addr: *mut c_char = null_mut();
            let res = virtual_call!(LocalToString, self.0, local, &mut addr);

            if res == 0 {
                Ok(CStr::from_ptr(addr))
            } else {
                Err(res)
            }
        }
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
    pub PushCell: fn(cell: cell_t) -> c_int,
    pub PushCellByRef: fn(cell: *mut cell_t, flags: c_int) -> c_int,
    pub PushFloat: fn(number: f32) -> c_int,
    pub PushFloatByRef: fn(number: *mut f32, flags: c_int) -> c_int,
    pub PushArray: fn(cell: *mut cell_t, cells: c_uint, flags: c_int) -> c_int,
    pub PushString: fn(string: *const c_char) -> c_int,
    pub PushStringEx: fn(string: *const c_char, length: size_t, sz_flags: c_int, cp_flags: c_int) -> c_int,
    pub Cancel: fn(),

    // IForward
    _Destructor: fn() -> (),
    #[cfg(not(windows))]
    _Destructor2: fn() -> (),
    pub GetForwardName: fn() -> *const c_char,
    pub GetFunctionCount: fn() -> c_uint,
    pub GetExecType: fn() -> ExecType,
    pub Execute: fn(result: *mut cell_t, filter: *mut c_void) -> i32,
}

#[derive(Debug)]
pub struct IForward(pub IForwardPtr);

impl IForward {}

// TODO: Type alias until it is properly implemented
pub type IChangeableForwardPtr = IForwardPtr;

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

#[derive(Debug, SMInterfaceApi)]
#[interface("IForwardManager", 4)]
pub struct IForwardManager(pub IForwardManagerPtr);

/// Helper for virtual function invocation that works with the `#[vtable]` attribute to support
/// virtual calls on Windows without compiler support for the `thiscall` calling convention.
#[macro_export]
#[cfg(all(windows, target_arch = "x86", not(feature = "thiscall")))]
macro_rules! virtual_call {
    ($name:ident, $this:expr, $($param:expr),* $(,)?) => {
        ((**$this).$name)(
            $this,
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

/// Helper for virtual function invocation that works with the `#[vtable]` attribute to support
/// virtual calls on Windows without compiler support for the `thiscall` calling convention.
#[macro_export]
#[cfg(not(all(windows, target_arch = "x86", not(feature = "thiscall"))))]
macro_rules! virtual_call {
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

/// Helper for virtual function invocation that works with the `#[vtable]` attribute to support
/// virtual calls on Windows without compiler support for the `thiscall` calling convention.
///
/// Identical to the non-fastcall-hack version above, but for calling functions that use varargs.
///
/// TODO: Figure out a way to make this type-safe (and hopefully avoid the need for it completely.)
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

impl<'a, T> NativeResult for T
where
    T: TryIntoPlugin<'a, cell_t>,
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

impl<'a, T, E> NativeResult for Result<T, E>
where
    T: TryIntoPlugin<'a, cell_t>,
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
pub fn safe_native_invoke<F: FnOnce() -> Result<cell_t, Box<dyn std::error::Error>> + std::panic::UnwindSafe>(ctx: &IPluginContext, f: F) -> cell_t {
    let result = std::panic::catch_unwind(f);

    match result {
        Ok(result) => match result {
            Ok(result) => result,
            Err(err) => ctx.throw_native_error(err.to_string()),
        },
        Err(err) => {
            let msg = format!(
                "Unexpected panic: {}",
                if let Some(str_slice) = err.downcast_ref::<&'static str>() {
                    str_slice
                } else if let Some(string) = err.downcast_ref::<String>() {
                    string
                } else {
                    "Unknown message"
                }
            );

            ctx.throw_native_error(msg)
        }
    }
}
