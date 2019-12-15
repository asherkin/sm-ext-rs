#![feature(abi_thiscall)]
#![allow(non_snake_case, non_camel_case_types, unused_variables)]

pub use sm_ext_derive::*;

pub mod types {
    use super::vtables::*;
    use crate::IPluginContext;
    use std::convert::TryFrom;
    use std::ffi::{CStr, CString};
    use std::fmt::{Error, Formatter};
    use std::os::raw::{c_char, c_uchar, c_uint};

    #[repr(transparent)]
    pub struct IdentityType(c_uint);

    // TODO: This should be a checked enum.
    #[repr(transparent)]
    pub struct FeatureType(c_uchar);

    // TODO: This should be a checked enum.
    #[repr(transparent)]
    pub struct FeatureStatus(c_uchar);

    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct cell_t(i32);

    impl std::fmt::Display for cell_t {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            self.0.fmt(f)
        }
    }

    pub trait TryFromWithContext<'a, T>: Sized {
        type Error;

        fn try_from_plugin(ctx: &'a crate::IPluginContext, value: T) -> Result<Self, Self::Error>;
    }

    impl<T, U> TryFromWithContext<'_, T> for U
    where
        U: TryFrom<T>,
    {
        type Error = U::Error;

        fn try_from_plugin(ctx: &IPluginContext, value: T) -> Result<Self, Self::Error> {
            TryFrom::try_from(value)
        }
    }

    pub trait TryIntoWithContext<'a, T>: Sized {
        type Error;

        fn try_into_plugin(self, ctx: &'a IPluginContext) -> Result<T, Self::Error>;
    }

    impl<'a, T, U> TryIntoWithContext<'a, U> for T
    where
        U: TryFromWithContext<'a, T>,
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

    impl TryFromWithContext<'_, cell_t> for CString {
        type Error = &'static str;

        fn try_from_plugin(ctx: &IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
            match ctx.local_to_string(value) {
                Ok(s) => Ok(s.into()),
                Err(_) => Err("Invalid memory address"),
            }
        }
    }

    impl<'a> TryFromWithContext<'a, cell_t> for &'a CStr {
        type Error = &'static str;

        fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
            match ctx.local_to_string(value) {
                Ok(s) => Ok(s),
                Err(_) => Err("Invalid memory address"),
            }
        }
    }

    // TODO: These &mut implementations seem risky, maybe a SPRef/SPString/SPArray wrapper object would be a better way to go...

    impl<'a> TryFromWithContext<'a, cell_t> for &'a mut cell_t {
        type Error = &'static str;

        fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
            match ctx.local_to_phys_addr(value) {
                Ok(s) => Ok(s),
                Err(_) => Err("Invalid memory address"),
            }
        }
    }

    impl<'a> TryFromWithContext<'a, cell_t> for &'a mut i32 {
        type Error = &'static str;

        fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
            let cell: &mut cell_t = value.try_into_plugin(ctx)?;
            unsafe { Ok(&mut *(cell as *mut cell_t as *mut i32)) }
        }
    }

    impl<'a> TryFromWithContext<'a, cell_t> for &'a mut f32 {
        type Error = &'static str;

        fn try_from_plugin(ctx: &'a IPluginContext, value: cell_t) -> Result<Self, Self::Error> {
            let cell: &mut cell_t = value.try_into_plugin(ctx)?;
            unsafe { Ok(&mut *(cell as *mut cell_t as *mut f32)) }
        }
    }

    #[repr(C)]
    pub struct NativeInfo {
        pub name: *const c_char,
        pub func: Option<unsafe extern "C" fn(ctx: IPluginContextPtr, args: *const cell_t) -> cell_t>,
    }

    pub struct IdentityToken();
    pub type IdentityTokenPtr = *mut IdentityToken;

    pub type IExtensionInterfacePtr = *mut *mut IExtensionInterfaceVtable;
    pub type IExtensionPtr = *mut *mut IExtensionVtable;
    pub type SMInterfacePtr = *mut *mut SMInterfaceVtable;
    pub type IShareSysPtr = *mut *mut IShareSysVtable;
    pub type IFeatureProviderPtr = *mut *mut IFeatureProviderVtable;
    pub type IPluginRuntimePtr = *mut *mut IPluginRuntimeVtable;
    pub type IPluginContextPtr = *mut *mut IPluginContextVtable;
}

pub(self) mod vtables {
    use super::types::*;

    use libc::size_t;
    use std::os::raw::{c_char, c_int, c_uint, c_void};

    #[repr(C)]
    pub struct IExtensionInterfaceVtable {
        pub GetExtensionVersion: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> i32,
        pub OnExtensionLoad: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr, me: IExtensionPtr, sys: IShareSysPtr, error: *mut c_char, maxlength: size_t, late: bool) -> bool,
        pub OnExtensionUnload: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> (),
        pub OnExtensionsAllLoaded: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> (),
        pub OnExtensionPauseChange: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr, pause: bool) -> (),
        pub QueryInterfaceDrop: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr, interface: SMInterfacePtr) -> bool,
        pub NotifyInterfaceDrop: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr, interface: SMInterfacePtr) -> (),
        pub QueryRunning: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr, error: *mut c_char, maxlength: size_t) -> bool,
        pub IsMetamodExtension: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> bool,
        pub GetExtensionName: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> *const c_char,
        pub GetExtensionURL: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> *const c_char,
        pub GetExtensionTag: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> *const c_char,
        pub GetExtensionAuthor: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> *const c_char,
        pub GetExtensionVerString: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> *const c_char,
        pub GetExtensionDescription: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> *const c_char,
        pub GetExtensionDateString: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> *const c_char,
        pub OnCoreMapStart: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr, edict_list: *mut c_void, edict_count: c_int, client_max: c_int) -> (),
        pub OnDependenciesDropped: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> (),
        pub OnCoreMapEnd: unsafe extern "thiscall" fn(this: IExtensionInterfacePtr) -> (),
    }

    #[repr(C)]
    pub struct IExtensionVtable {
        pub IsLoaded: unsafe extern "thiscall" fn(this: IExtensionPtr) -> bool,
        pub GetAPI: unsafe extern "thiscall" fn(this: IExtensionPtr) -> IExtensionInterfacePtr,
        pub GetFilename: unsafe extern "thiscall" fn(this: IExtensionPtr) -> *const c_char,
        pub GetIdentity: unsafe extern "thiscall" fn(this: IExtensionPtr) -> IdentityTokenPtr,
        _FindFirstDependency: unsafe extern "thiscall" fn(this: IExtensionPtr) -> *mut c_void,
        _FindNextDependency: unsafe extern "thiscall" fn(this: IExtensionPtr) -> *mut c_void,
        _FreeDependencyIterator: unsafe extern "thiscall" fn(this: IExtensionPtr) -> *mut c_void,
        pub IsRunning: unsafe extern "thiscall" fn(this: IExtensionPtr, error: *mut c_char, maxlength: size_t) -> bool,
        pub IsExternal: unsafe extern "thiscall" fn(this: IExtensionPtr) -> bool,
    }

    #[repr(C)]
    pub struct SMInterfaceVtable {
        pub GetInterfaceVersion: unsafe extern "thiscall" fn(this: SMInterfacePtr) -> c_uint,
        pub GetInterfaceName: unsafe extern "thiscall" fn(this: SMInterfacePtr) -> *const c_char,
        pub IsVersionCompatible: unsafe extern "thiscall" fn(this: SMInterfacePtr, version: c_uint) -> bool,
    }

    #[repr(C)]
    pub struct IShareSysVtable {
        pub AddInterface: unsafe extern "thiscall" fn(this: IShareSysPtr, myself: IExtensionPtr, iface: SMInterfacePtr) -> bool,
        pub RequestInterface: unsafe extern "thiscall" fn(this: IShareSysPtr, iface_name: *const c_char, iface_vers: c_uint, myself: IExtensionPtr, iface: *mut SMInterfacePtr) -> bool,
        pub AddNatives: unsafe extern "thiscall" fn(this: IShareSysPtr, myself: IExtensionPtr, natives: *const NativeInfo) -> (),
        pub CreateIdentType: unsafe extern "thiscall" fn(this: IShareSysPtr, name: *const c_char) -> IdentityType,
        pub FindIdentType: unsafe extern "thiscall" fn(this: IShareSysPtr, name: *const c_char) -> IdentityType,
        pub CreateIdentity: unsafe extern "thiscall" fn(this: IShareSysPtr, ident_type: IdentityType, ptr: *mut c_void) -> IdentityTokenPtr,
        pub DestroyIdentType: unsafe extern "thiscall" fn(this: IShareSysPtr, ident_type: IdentityType) -> (),
        pub DestroyIdentity: unsafe extern "thiscall" fn(this: IShareSysPtr, identity: IdentityTokenPtr) -> (),
        pub AddDependency: unsafe extern "thiscall" fn(this: IShareSysPtr, myself: IExtensionPtr, filename: *const c_char, require: bool, autoload: bool) -> (),
        pub RegisterLibrary: unsafe extern "thiscall" fn(this: IShareSysPtr, myself: IExtensionPtr, name: *const c_char) -> (),
        _OverrideNatives: unsafe extern "thiscall" fn(this: IShareSysPtr, myself: IExtensionPtr, natives: *const NativeInfo) -> (),
        pub AddCapabilityProvider: unsafe extern "thiscall" fn(this: IShareSysPtr, myself: IExtensionPtr, provider: IFeatureProviderPtr, name: *const c_char) -> (),
        pub DropCapabilityProvider: unsafe extern "thiscall" fn(this: IShareSysPtr, myself: IExtensionPtr, provider: IFeatureProviderPtr, name: *const c_char) -> (),
        pub TestFeature: unsafe extern "thiscall" fn(this: IShareSysPtr, rt: IPluginRuntimePtr, feature_type: FeatureType, name: *const c_char) -> FeatureStatus,
    }

    #[repr(C)]
    pub struct IFeatureProviderVtable {}

    #[repr(C)]
    pub struct IPluginRuntimeVtable {}

    #[repr(C)]
    pub struct IPluginContextVtable {
        _Destructor: unsafe extern "thiscall" fn(this: IPluginContextPtr) -> (),
        #[cfg(unix)]
        _Destructor2: unsafe extern "thiscall" fn(this: IPluginContextPtr) -> (),
        _GetVirtualMachine: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetContext: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _IsDebugging: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _SetDebugBreak: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetDebugInfo: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _HeapAlloc: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _HeapPop: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _HeapRelease: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _FindNativeByName: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetNativeByIndex: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetNativesNum: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _FindPublicByName: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetPublicByIndex: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetPublicsNum: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetPubvarByIndex: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _FindPubvarByName: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetPubvarAddrs: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetPubVarsNum: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        pub LocalToPhysAddr: unsafe extern "thiscall" fn(this: IPluginContextPtr, local_addr: cell_t, phys_addr: *mut *mut cell_t) -> c_int,
        pub LocalToString: unsafe extern "thiscall" fn(this: IPluginContextPtr, local_addr: cell_t, addr: *mut *mut c_char) -> c_int,
        _StringToLocal: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _StringToLocalUTF8: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _PushCell: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _PushCellArray: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _PushString: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _PushCellsFromArray: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _BindNatives: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _BindNative: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _BindNativeToAny: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _Execute: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _ThrowNativeErrorEx: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        pub ThrowNativeError: unsafe extern "cdecl" fn(this: IPluginContextPtr, *const c_char, ...) -> cell_t,
        _GetFunctionByName: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetFunctionById: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetIdentity: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetNullRef: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _LocalToStringNULL: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _BindNativeToIndex: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _IsInExec: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetRuntime: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _Execute2: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetLastNativeError: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetLocalParams: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _SetKey: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _GetKey: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _ClearLastNativeError: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _APIv2: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _ReportError: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _ReportErrorVA: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _ReportFatalError: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _ReportFatalErrorVA: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _ReportErrorNumber: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _BlamePluginError: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _CreateFrameIterator: unsafe extern "thiscall" fn(this: IPluginContextPtr),
        _DestroyFrameIterator: unsafe extern "thiscall" fn(this: IPluginContextPtr),
    }
}

pub use IExtensionInterfaceApi::*;
mod IExtensionInterfaceApi {
    use super::types::{IExtensionInterfacePtr, IExtensionPtr, IShareSysPtr, SMInterfacePtr};
    use super::vtables::IExtensionInterfaceVtable;
    use super::{IExtension, IShareSys, SMInterface};

    use libc::size_t;
    use std::ffi::{CStr, CString};
    use std::os::raw::{c_char, c_int, c_void};

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

        unsafe extern "thiscall" fn get_extension_version(this: IExtensionInterfacePtr) -> i32 {
            8
        }

        unsafe extern "thiscall" fn on_extension_load(this: IExtensionInterfacePtr, me: IExtensionPtr, sys: IShareSysPtr, error: *mut c_char, maxlength: size_t, late: bool) -> bool {
            match (*this.cast::<Self>()).delegate.on_extension_load(IExtension(me), IShareSys(sys), late) {
                Ok(_) => true,
                Err(str) => {
                    libc::strncpy(error, str.as_ptr(), maxlength);
                    false
                }
            }
        }

        unsafe extern "thiscall" fn on_extension_unload(this: IExtensionInterfacePtr) {
            (*this.cast::<Self>()).delegate.on_extension_unload()
        }

        unsafe extern "thiscall" fn on_extensions_all_loaded(this: IExtensionInterfacePtr) {
            (*this.cast::<Self>()).delegate.on_extensions_all_loaded()
        }

        unsafe extern "thiscall" fn on_extension_pause_change(this: IExtensionInterfacePtr, pause: bool) {
            (*this.cast::<Self>()).delegate.on_extension_pause_change(pause)
        }

        unsafe extern "thiscall" fn query_interface_drop(this: IExtensionInterfacePtr, interface: SMInterfacePtr) -> bool {
            (*this.cast::<Self>()).delegate.query_interface_drop(SMInterface(interface))
        }

        unsafe extern "thiscall" fn notify_interface_drop(this: IExtensionInterfacePtr, interface: SMInterfacePtr) {
            (*this.cast::<Self>()).delegate.notify_interface_drop(SMInterface(interface))
        }

        unsafe extern "thiscall" fn query_running(this: IExtensionInterfacePtr, error: *mut c_char, maxlength: size_t) -> bool {
            match (*this.cast::<Self>()).delegate.query_running() {
                Ok(_) => true,
                Err(str) => {
                    libc::strncpy(error, str.as_ptr(), maxlength);
                    false
                }
            }
        }

        unsafe extern "thiscall" fn is_metamod_extension(this: IExtensionInterfacePtr) -> bool {
            false
        }

        unsafe extern "thiscall" fn get_extension_name(this: IExtensionInterfacePtr) -> *const c_char {
            (*this.cast::<Self>()).delegate.get_extension_name().as_ptr()
        }

        unsafe extern "thiscall" fn get_extension_url(this: IExtensionInterfacePtr) -> *const c_char {
            (*this.cast::<Self>()).delegate.get_extension_url().as_ptr()
        }

        unsafe extern "thiscall" fn get_extension_tag(this: IExtensionInterfacePtr) -> *const c_char {
            (*this.cast::<Self>()).delegate.get_extension_tag().as_ptr()
        }

        unsafe extern "thiscall" fn get_extension_author(this: IExtensionInterfacePtr) -> *const c_char {
            (*this.cast::<Self>()).delegate.get_extension_author().as_ptr()
        }

        unsafe extern "thiscall" fn get_extension_ver_string(this: IExtensionInterfacePtr) -> *const c_char {
            (*this.cast::<Self>()).delegate.get_extension_ver_string().as_ptr()
        }

        unsafe extern "thiscall" fn get_extension_description(this: IExtensionInterfacePtr) -> *const c_char {
            (*this.cast::<Self>()).delegate.get_extension_description().as_ptr()
        }

        unsafe extern "thiscall" fn get_extension_date_string(this: IExtensionInterfacePtr) -> *const c_char {
            (*this.cast::<Self>()).delegate.get_extension_date_string().as_ptr()
        }

        unsafe extern "thiscall" fn on_core_map_start(this: IExtensionInterfacePtr, edict_list: *mut c_void, edict_count: c_int, client_max: c_int) {
            (*this.cast::<Self>()).delegate.on_core_map_start(edict_list, edict_count, client_max)
        }

        unsafe extern "thiscall" fn on_dependencies_dropped(this: IExtensionInterfacePtr) {
            (*this.cast::<Self>()).delegate.on_dependencies_dropped()
        }

        unsafe extern "thiscall" fn on_core_map_end(this: IExtensionInterfacePtr) {
            (*this.cast::<Self>()).delegate.on_core_map_end()
        }
    }
}

pub use IExtensionApi::*;
mod IExtensionApi {
    use super::types::{IExtensionInterfacePtr, IExtensionPtr, IdentityTokenPtr};

    use std::ffi::CStr;
    use std::os::raw::c_char;
    use std::str::Utf8Error;

    #[derive(Debug)]
    pub enum IsRunningError<'a> {
        WithReason(&'a str),
        InvalidReason(Utf8Error),
    }

    #[derive(Debug)]
    pub struct IExtension(pub IExtensionPtr);

    impl IExtension {
        pub fn is_loaded(&self) -> bool {
            unsafe { ((**self.0).IsLoaded)(self.0) }
        }

        pub fn get_api(&self) -> IExtensionInterfacePtr {
            unsafe { ((**self.0).GetAPI)(self.0) }
        }

        pub fn get_filename(&self) -> Result<&str, Utf8Error> {
            unsafe {
                let c_name = ((**self.0).GetFilename)(self.0);

                CStr::from_ptr(c_name).to_str()
            }
        }

        pub fn get_identity(&self) -> IdentityTokenPtr {
            unsafe { ((**self.0).GetIdentity)(self.0) }
        }

        pub fn is_running(&self) -> Result<(), IsRunningError> {
            unsafe {
                let mut c_error = [0 as c_char; 256];
                let result = ((**self.0).IsRunning)(self.0, c_error.as_mut_ptr(), c_error.len());

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
            unsafe { ((**self.0).IsExternal)(self.0) }
        }
    }
}

pub use SMInterfaceApi::*;
mod SMInterfaceApi {
    use super::types::SMInterfacePtr;

    use std::ffi::CStr;
    use std::str::Utf8Error;

    #[derive(Debug)]
    pub struct SMInterface(pub SMInterfacePtr);

    impl SMInterface {
        pub fn get_interface_version(&self) -> u32 {
            unsafe { ((**self.0).GetInterfaceVersion)(self.0) }
        }

        pub fn get_interface_name(&self) -> Result<&str, Utf8Error> {
            unsafe {
                let c_name = ((**self.0).GetInterfaceName)(self.0);

                CStr::from_ptr(c_name).to_str()
            }
        }

        pub fn is_version_compatible(&self, version: u32) -> bool {
            unsafe { ((**self.0).IsVersionCompatible)(self.0, version) }
        }
    }
}

pub use IShareSysApi::*;
mod IShareSysApi {
    use super::types::{IShareSysPtr, NativeInfo, SMInterfacePtr};
    use super::IExtensionApi::IExtension;
    use super::SMInterfaceApi::SMInterface;

    use std::ffi::{CString, NulError};
    use std::ptr::null_mut;

    #[derive(Debug)]
    pub enum RequestInterfaceError {
        StringError(NulError),
        InterfaceError(),
    }

    #[derive(Debug)]
    pub struct IShareSys(pub IShareSysPtr);

    impl IShareSys {
        pub fn request_interface(&self, myself: &IExtension, name: &str, version: u32) -> Result<SMInterface, RequestInterfaceError> {
            let c_name = CString::new(name).map_err(RequestInterfaceError::StringError)?;

            unsafe {
                let mut iface: SMInterfacePtr = null_mut();
                let res = ((**self.0).RequestInterface)(self.0, c_name.as_ptr(), version, myself.0, &mut iface);

                if res {
                    Ok(SMInterface(iface))
                } else {
                    Err(RequestInterfaceError::InterfaceError())
                }
            }
        }

        /// # Safety
        ///
        /// This is should be be used via the `register_natives!` macro only.
        pub unsafe fn add_natives(&self, myself: &IExtension, natives: *const NativeInfo) {
            ((**self.0).AddNatives)(self.0, myself.0, natives)
        }
    }
}

pub use IPluginContextApi::*;
mod IPluginContextApi {
    use super::types::{cell_t, IPluginContextPtr};
    use c_str_macro::c_str;
    use std::ffi::{CStr, CString};
    use std::os::raw::c_char;
    use std::ptr::null_mut;

    #[derive(Debug)]
    pub struct IPluginContext(pub IPluginContextPtr);

    impl IPluginContext {
        pub fn throw_native_error(&self, err: String) -> cell_t {
            let fmt = c_str!("%s");
            let err = CString::new(err).unwrap_or_else(|_| c_str!("ThrowNativeError message contained NUL byte").into());
            unsafe { ((**self.0).ThrowNativeError)(self.0, fmt.as_ptr(), err.as_ptr()) }
        }

        pub fn local_to_phys_addr(&self, local: cell_t) -> Result<&mut cell_t, i32> {
            unsafe {
                let mut addr: *mut cell_t = null_mut();
                let res = ((**self.0).LocalToPhysAddr)(self.0, local, &mut addr);

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
                let res = ((**self.0).LocalToString)(self.0, local, &mut addr);

                if res == 0 {
                    Ok(CStr::from_ptr(addr))
                } else {
                    Err(res)
                }
            }
        }
    }
}

// TODO: Not a huge fan of this one, but it seems to be the most user-friendly option without requiring the new macro features in rust nightly.
// New macros would allow proper IDE auto-completion, as we could generate the conversion function externally, and probably handle arguments better.
// I'd like to offer both routes though I think.
#[macro_export]
macro_rules! declare_native {
    (fn $name:ident($ctx:ident: &IPluginContext, $args:ident: &[cell_t]) -> cell_t $body:tt) => {
        unsafe extern "C" fn $name(ctx: $crate::types::IPluginContextPtr, args: *const $crate::types::cell_t) -> $crate::types::cell_t {
            use ::std::convert::TryInto;

            let callback = |$ctx: &$crate::IPluginContext, $args: &[$crate::types::cell_t]| -> $crate::types::cell_t { $body };

            let count: i32 = (*args).into();
            let args = ::std::slice::from_raw_parts(args.offset(1), count.try_into().unwrap());
            callback(&$crate::IPluginContext(ctx), &args)
        }
    };
}

#[macro_export]
macro_rules! register_natives {
    ($sys:expr, $myself:expr, [$(($name:expr, $func:expr)),* $(,)?]) => {
        unsafe {
            let mut vec = Vec::new();
            $(
                let name = concat!($name, "\0").as_ptr() as *const ::std::os::raw::c_char;
                vec.push($crate::types::NativeInfo {
                    name: name,
                    func: Some($func),
                });
            )*
            vec.push($crate::types::NativeInfo {
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

pub fn safe_native_invoke<F: FnOnce() -> Result<types::cell_t, Box<dyn ::std::error::Error>> + std::panic::UnwindSafe>(ctx: &IPluginContext, f: F) -> types::cell_t {
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
