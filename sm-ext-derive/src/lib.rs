extern crate proc_macro;

use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::{format_ident, quote, quote_spanned, ToTokens, TokenStreamExt};
use syn;
use syn::spanned::Spanned;

/// Creates the entry point for SourceMod to recognise this library as an extension and set the required metadata.
///
/// The `#[extension]` attribute recognises the following optional keys using the *MetaListNameValueStr* syntax:
///   * `name`
///   * `description`
///   * `url`
///   * `author`
///   * `version`
///   * `tag`
///   * `date`
///
/// If not overridden, all extension metadata will be set to suitable values using the Cargo package metadata.
///
/// An instance of the struct this is applied to will be created with [`Default::default()`] to serve
/// as the global singleton instance, and you can implement the [`IExtensionInterface`] trait on the
/// type to handle SourceMod lifecycle callbacks.
#[proc_macro_derive(SMExtension, attributes(extension))]
pub fn derive_extension_metadata(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let name = &ast.ident;
    let input = MetadataInput::new(&ast);

    let extension_name = CStringToken(input.name);
    let extension_description = CStringToken(input.description);
    let extension_url = CStringToken(input.url);
    let extension_author = CStringToken(input.author);
    let extension_version = CStringToken(input.version);
    let extension_tag = CStringToken(input.tag);
    let extension_date = CStringToken(input.date);

    let expanded = quote! {
        // TODO: Checking for a test build here doesn't work when a dependent crate is being tested.
        #[cfg(all(windows, not(target_feature = "crt-static"), not(test)))]
        compile_error!("SourceMod requires the Windows CRT to be statically linked (pass `-C target-feature=+crt-static` to rustc)");

        #[no_mangle]
        pub extern "C" fn GetSMExtAPI() -> *mut ::sm_ext::IExtensionInterfaceAdapter<#name> {
            let delegate: #name = Default::default();
            let extension = ::sm_ext::IExtensionInterfaceAdapter::new(delegate);
            Box::into_raw(Box::new(extension))
        }

        impl ::sm_ext::IExtensionMetadata for #name {
            fn get_extension_name(&self) -> &'static ::std::ffi::CStr {
                #extension_name
            }
            fn get_extension_url(&self) -> &'static ::std::ffi::CStr {
                #extension_url
            }
            fn get_extension_tag(&self) -> &'static ::std::ffi::CStr {
                #extension_tag
            }
            fn get_extension_author(&self) -> &'static ::std::ffi::CStr {
                #extension_author
            }
            fn get_extension_ver_string(&self) -> &'static ::std::ffi::CStr {
                #extension_version
            }
            fn get_extension_description(&self) -> &'static ::std::ffi::CStr {
                #extension_description
            }
            fn get_extension_date_string(&self) -> &'static ::std::ffi::CStr {
                #extension_date
            }
        }
    };

    expanded.into()
}

struct CStringToken(MetadataString);

impl ToTokens for CStringToken {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let value = match &self.0 {
            MetadataString::String(str) => str.to_token_stream(),
            MetadataString::EnvVar(var) => quote! {
                env!(#var)
            },
        };

        // Inspired by https://crates.io/crates/c_str_macro
        tokens.append_all(quote! {
            unsafe {
                ::std::ffi::CStr::from_ptr(concat!(#value, "\0").as_ptr() as *const ::std::os::raw::c_char)
            }
        });
    }
}

enum MetadataString {
    String(String),
    EnvVar(String),
}

struct MetadataInput {
    pub name: MetadataString,
    pub description: MetadataString,
    pub url: MetadataString,
    pub author: MetadataString,
    pub version: MetadataString,
    pub tag: MetadataString,
    pub date: MetadataString,
}

impl MetadataInput {
    #[allow(clippy::cognitive_complexity)]
    pub fn new(ast: &syn::DeriveInput) -> MetadataInput {
        let mut name = None;
        let mut description = None;
        let mut url = None;
        let mut author = None;
        let mut version = None;
        let mut tag = None;
        let mut date = None;

        let meta = ast.attrs.iter().find_map(|attr| match attr.parse_meta() {
            Ok(m) => {
                if m.path().is_ident("extension") {
                    Some(m)
                } else {
                    None
                }
            }
            Err(e) => panic!("unable to parse attribute: {}", e),
        });

        if let Some(meta) = meta {
            let meta_list = match meta {
                syn::Meta::List(inner) => inner,
                _ => panic!("attribute 'extension' has incorrect type"),
            };

            for item in meta_list.nested {
                let pair = match item {
                    syn::NestedMeta::Meta(syn::Meta::NameValue(ref pair)) => pair,
                    _ => panic!("unsupported attribute argument {:?}", item.to_token_stream()),
                };

                if pair.path.is_ident("name") {
                    if let syn::Lit::Str(ref s) = pair.lit {
                        name = Some(s.value());
                    } else {
                        panic!("name value must be string literal");
                    }
                } else if pair.path.is_ident("description") {
                    if let syn::Lit::Str(ref s) = pair.lit {
                        description = Some(s.value())
                    } else {
                        panic!("description value must be string literal");
                    }
                } else if pair.path.is_ident("url") {
                    if let syn::Lit::Str(ref s) = pair.lit {
                        url = Some(s.value())
                    } else {
                        panic!("url value must be string literal");
                    }
                } else if pair.path.is_ident("author") {
                    if let syn::Lit::Str(ref s) = pair.lit {
                        author = Some(s.value())
                    } else {
                        panic!("author value must be string literal");
                    }
                } else if pair.path.is_ident("version") {
                    if let syn::Lit::Str(ref s) = pair.lit {
                        version = Some(s.value())
                    } else {
                        panic!("version value must be string literal");
                    }
                } else if pair.path.is_ident("tag") {
                    if let syn::Lit::Str(ref s) = pair.lit {
                        tag = Some(s.value())
                    } else {
                        panic!("tag value must be string literal");
                    }
                } else if pair.path.is_ident("date") {
                    if let syn::Lit::Str(ref s) = pair.lit {
                        date = Some(s.value())
                    } else {
                        panic!("date value must be string literal");
                    }
                } else {
                    panic!("unsupported attribute key '{}' found", pair.path.to_token_stream())
                }
            }
        }

        let name = match name {
            Some(name) => MetadataString::String(name),
            None => MetadataString::EnvVar("CARGO_PKG_NAME".into()),
        };

        let description = match description {
            Some(description) => MetadataString::String(description),
            None => MetadataString::EnvVar("CARGO_PKG_DESCRIPTION".into()),
        };

        let url = match url {
            Some(url) => MetadataString::String(url),
            None => MetadataString::EnvVar("CARGO_PKG_HOMEPAGE".into()),
        };

        // TODO: This probably needs a special type to post-process the author list later.
        let author = match author {
            Some(author) => MetadataString::String(author),
            None => MetadataString::EnvVar("CARGO_PKG_AUTHORS".into()),
        };

        let version = match version {
            Some(version) => MetadataString::String(version),
            None => MetadataString::EnvVar("CARGO_PKG_VERSION".into()),
        };

        let tag = match tag {
            Some(tag) => MetadataString::String(tag),
            None => MetadataString::EnvVar("CARGO_PKG_NAME".into()),
        };

        let date = match date {
            Some(date) => MetadataString::String(date),
            None => MetadataString::String("with Rust".into()),
        };

        MetadataInput { name, description, url, author, version, tag, date }
    }
}

#[proc_macro_derive(SMInterfaceApi, attributes(interface))]
pub fn derive_interface_api(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let ident = input.ident;

    let attribute = input.attrs.iter().find_map(|attr| match attr.parse_meta() {
        Ok(m) => {
            if m.path().is_ident("interface") {
                Some(m)
            } else {
                None
            }
        }
        Err(e) => panic!("unable to parse attribute: {}", e),
    });

    let mut output = TokenStream::new();

    if let Some(attribute) = attribute {
        let nested = match attribute {
            syn::Meta::List(inner) => inner.nested,
            _ => panic!("attribute 'interface' has incorrect type"),
        };

        if nested.len() != 2 {
            panic!("attribute 'interface' expected 2 params: name, version")
        }

        let interface_name = match &nested[0] {
            syn::NestedMeta::Lit(lit) => match lit {
                syn::Lit::Str(str) => str,
                _ => panic!("attribute 'interface' param 1 should be a string"),
            },
            _ => panic!("attribute 'interface' param 1 should be a literal string"),
        };

        let interface_version = match &nested[1] {
            syn::NestedMeta::Lit(lit) => match lit {
                syn::Lit::Int(int) => int,
                _ => panic!("attribute 'interface' param 2 should be an integer"),
            },
            _ => panic!("attribute 'interface' param 2 should be a literal integer"),
        };

        output.extend(quote! {
            impl RequestableInterface for #ident {
                fn get_interface_name() -> &'static str {
                    #interface_name
                }

                fn get_interface_version() -> u32 {
                    #interface_version
                }

                #[allow(clippy::transmute_ptr_to_ptr)]
                unsafe fn from_raw_interface(iface: SMInterface) -> #ident {
                    #ident(std::mem::transmute(iface.0))
                }
            }
        });
    }

    output.extend(quote! {
        impl SMInterfaceApi for #ident {
            fn get_interface_version(&self) -> u32 {
                unsafe { virtual_call!(GetInterfaceVersion, self.0) }
            }

            fn get_interface_name(&self) -> &str {
                unsafe {
                    let c_name = virtual_call!(GetInterfaceName, self.0);

                    std::ffi::CStr::from_ptr(c_name).to_str().unwrap()
                }
            }

            fn is_version_compatible(&self, version: u32) -> bool {
                unsafe { virtual_call!(IsVersionCompatible, self.0, version) }
            }
        }
    });

    output.into()
}

#[proc_macro_derive(ICallableApi)]
pub fn derive_callable_api(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let ident = input.ident;
    let output = quote! {
        impl ICallableApi for #ident {
            fn push_int(&self, cell: i32) -> Result<(), SPError> {
                unsafe {
                    let res = virtual_call!(PushCell, self.0, cell.into());
                    match res {
                        SPError::None => Ok(()),
                        _ => Err(res),
                    }
                }
            }

            fn push_float(&self, number: f32) -> Result<(), SPError> {
                unsafe {
                    let res = virtual_call!(PushFloat, self.0, number);
                    match res {
                        SPError::None => Ok(()),
                        _ => Err(res),
                    }
                }
            }

            fn push_string(&self, string: &CStr) -> Result<(), SPError> {
                unsafe {
                    let res = virtual_call!(PushString, self.0, string.as_ptr());
                    match res {
                        SPError::None => Ok(()),
                        _ => Err(res),
                    }
                }
            }
        }
    };

    output.into()
}

/// Declares a function as a native callback and generates internal support code.
///
/// A valid native callback must be a free function that is not async, not unsafe, not extern, has
/// no generic parameters, the first argument takes a [`&IPluginContext`](IPluginContext), any
/// remaining arguments are convertible to [`cell_t`] using [`TryIntoPlugin`] (possibly wrapped in
/// an [`Option`]), and returns a type that satisfies the [`NativeResult`] trait.
///
/// When the native is invoked by SourceMod the input arguments will be checked to ensure all required
/// arguments have been passed and are of the correct type, and panics or error results will automatically
/// be converted into a SourceMod native error using [`safe_native_invoke`].
///
/// # Example
///
/// ```
/// use sm_ext::{native, IPluginContext};
///
/// #[native]
/// fn simple_add_native(_ctx: &IPluginContext, a: i32, b: i32) -> i32 {
///     a + b
/// }
/// ```
#[proc_macro_attribute]
pub fn native(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = syn::parse_macro_input!(item as syn::ItemFn);
    // println!("{:#?}", input);

    let mut output = TokenStream::new();

    if let Some(asyncness) = &input.sig.asyncness {
        output.extend(error("Native callback must not be async", asyncness.span()));
    }

    if let Some(unsafety) = &input.sig.unsafety {
        output.extend(error("Native callback must not be unsafe", unsafety.span()));
    }

    if let Some(abi) = &input.sig.abi {
        output.extend(error("Native callback must use the default Rust ABI", abi.span()));
    }

    if !input.sig.generics.params.is_empty() {
        output.extend(error("Native callback must not have any generic parameters", input.sig.generics.span()));
    }

    let mut param_count: i32 = 0;
    let mut trailing_optional_count = 0;
    let mut param_output = TokenStream::new();
    for param in &input.sig.inputs {
        match param {
            syn::FnArg::Receiver(param) => {
                output.extend(error("Native callback must not be a method", param.span()));
            }
            syn::FnArg::Typed(param) => {
                param_count += 1;
                if param_count == 1 {
                    param_output.extend(quote_spanned!(param.span() => &ctx,));
                    continue;
                }

                let mut is_optional = false;
                if let syn::Type::Path(path) = &*param.ty {
                    if path.path.segments.last().unwrap().ident == "Option" {
                        is_optional = true;
                        trailing_optional_count += 1;
                    } else {
                        trailing_optional_count = 0;
                    }
                } else {
                    trailing_optional_count = 0;
                }

                let param_idx = param_count - 1;
                let convert_param = quote_spanned!(param.span() =>
                    (*(args.offset(#param_idx as isize)))
                        .try_into_plugin(&ctx)
                        .map_err(|err| format!("Error processing argument {}: {}", #param_idx, err))?
                );

                if is_optional {
                    param_output.extend(quote! {
                        if #param_idx <= count {
                            Some(#convert_param)
                        } else {
                            None
                        },
                    });
                } else {
                    param_output.extend(quote! {
                        #convert_param,
                    });
                }
            }
        };
    }

    let args_minimum = (param_count - 1) - trailing_optional_count;
    let wrapper_ident = &input.sig.ident;
    let callback_ident = format_ident!("__{}_impl", wrapper_ident);
    output.extend(quote! {
        unsafe extern "C" fn #wrapper_ident(ctx: sm_ext::IPluginContextPtr, args: *const sm_ext::cell_t) -> sm_ext::cell_t {
            let ctx = sm_ext::IPluginContext(ctx);

            sm_ext::safe_native_invoke(&ctx, || -> Result<sm_ext::cell_t, Box<dyn std::error::Error>> {
                use sm_ext::NativeResult;
                use sm_ext::TryIntoPlugin;

                let count: i32 = (*args).into();
                if count < #args_minimum {
                    return Err(format!("Not enough arguments, got {}, expected at least {}", count, #args_minimum).into());
                }

                let result = #callback_ident(
                    #param_output
                ).into_result()?;

                Ok(result.try_into_plugin(&ctx)
                    .map_err(|err| format!("Error processing return value: {}", err))?)
            })
        }
    });

    input.sig.ident = callback_ident;
    output.extend(input.to_token_stream());

    // println!("{}", output.to_string());
    output.into()
}

#[derive(Debug)]
struct ForwardInfo {
    ident: syn::Ident,
    name: Option<syn::LitStr>,
    exec_type: syn::Path,
    params: Vec<syn::BareFnArg>,
    ret: syn::Type,
}

fn parse_forward_from_field(field: &syn::Field, output: &mut TokenStream) -> Option<ForwardInfo> {
    // TODO: It would improve diagnostics to remove the attribute if it is found.
    let attribute = field.attrs.iter().find_map(|attr| match attr.parse_meta() {
        Ok(m) => {
            if m.path().is_ident("global_forward") || m.path().is_ident("private_forward") {
                Some(m)
            } else {
                None
            }
        }
        Err(e) => {
            output.extend(e.to_compile_error());
            None
        }
    })?;

    let (params, ret): (Vec<syn::BareFnArg>, _) = match &field.ty {
        syn::Type::BareFn(ty) => (
            ty.inputs.iter().cloned().collect(),
            match &ty.output {
                syn::ReturnType::Default => syn::parse_quote!(()),
                syn::ReturnType::Type(_, ty) => (*ty.as_ref()).clone(),
            },
        ),
        _ => {
            output.extend(error("expected bare function", field.ty.span()));
            return None;
        }
    };

    let nested = match &attribute {
        syn::Meta::List(inner) => &inner.nested,
        _ => {
            output.extend(error(&format!("attribute '{}' has incorrect type", attribute.path().get_ident().unwrap()), attribute.span()));
            return None;
        }
    };

    if attribute.path().is_ident("global_forward") {
        if nested.len() != 2 {
            output.extend(error("Usage: #[global_forward(Forward_Name, ExecType::)]", attribute.span()));
            return None;
        }

        let forward_name = match &nested[0] {
            syn::NestedMeta::Lit(lit) => match lit {
                syn::Lit::Str(str) => str,
                _ => {
                    output.extend(error("expected string literal", nested[0].span()));
                    return None;
                }
            },
            _ => {
                output.extend(error("expected string literal", nested[0].span()));
                return None;
            }
        };

        let forward_exec_type = match &nested[1] {
            syn::NestedMeta::Meta(meta) => match meta {
                syn::Meta::Path(path) => path,
                _ => {
                    output.extend(error("expected type path", nested[1].span()));
                    return None;
                }
            },
            _ => {
                output.extend(error("expected type path", nested[1].span()));
                return None;
            }
        };

        Some(ForwardInfo { ident: field.ident.as_ref().unwrap().clone(), name: Some((*forward_name).clone()), exec_type: (*forward_exec_type).clone(), params, ret })
    } else if attribute.path().is_ident("private_forward") {
        output.extend(error("#[private_forward] not implemented", attribute.span()));

        if nested.len() != 1 {
            output.extend(error("Usage: #[private_forward(ExecType::)]", attribute.span()));
            return None;
        }

        let forward_exec_type = match &nested[0] {
            syn::NestedMeta::Meta(meta) => match meta {
                syn::Meta::Path(path) => path,
                _ => {
                    output.extend(error("expected type path", nested[0].span()));
                    return None;
                }
            },
            _ => {
                output.extend(error("expected type path", nested[0].span()));
                return None;
            }
        };

        Some(ForwardInfo { ident: field.ident.as_ref().unwrap().clone(), name: None, exec_type: (*forward_exec_type).clone(), params, ret })
    } else {
        None
    }
}

#[proc_macro_attribute]
pub fn forwards(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = syn::parse_macro_input!(item as syn::ItemStruct);
    // println!("{:#?}", input);

    let mut fields = match &mut input.fields {
        syn::Fields::Named(fields) => fields,
        _ => panic!("Expected a struct with named fields"),
    };

    let mut output = TokenStream::new();

    let mut forwards = Vec::new();
    let mut filtered_fields: syn::punctuated::Punctuated<syn::Field, syn::Token![,]> = syn::punctuated::Punctuated::new();

    for field in &fields.named {
        if let Some(forward) = parse_forward_from_field(field, &mut output) {
            forwards.push(forward);
        } else {
            filtered_fields.push((*field).clone());
        }
    }

    fields.named = filtered_fields;

    output.extend(input.to_token_stream());

    if forwards.is_empty() {
        output.extend(error("#[forwards] attribute used on struct with no forward members", input.ident.span()));
        return output.into();
    }

    let mut output_thread_locals = TokenStream::new();
    let mut output_trait = TokenStream::new();
    let mut output_trait_impl = TokenStream::new();
    let mut output_trait_impl_register = TokenStream::new();
    let mut output_trait_impl_unregister = TokenStream::new();

    for forward in forwards {
        let forward_ident = &forward.ident;
        let type_ident = format_ident!("__{}_forward", forward.ident);
        let global_ident = format_ident!("__g_{}_forward", forward.ident);

        let forward_name = forward.name.unwrap(); // TODO: Handle private forwards.
        let forward_exec_type = forward.exec_type;

        let mut forward_param_types = TokenStream::new();

        let forward_call_return = forward.ret;
        let mut forward_call_args = TokenStream::new();
        let mut forward_call_pushes = TokenStream::new();

        for param in forward.params {
            let param_type = match &param.ty {
                syn::Type::Reference(ty) => ty.elem.as_ref(),
                ty => ty,
            };
            let param_name = &param.name.as_ref().unwrap().0;
            forward_param_types.extend(quote_spanned!(param_type.span() =>
                #param_type::param_type(),
            ));
            forward_call_args.extend(quote_spanned!(param.span() =>
                #param,
            ));
            forward_call_pushes.extend(quote_spanned!(param_name.span() =>
                #param_name.push(self.0)?;
            ));
        }

        output.extend(quote_spanned!(forward.ident.span() =>
            #[allow(non_camel_case_types)]
            struct #type_ident<'a>(&'a sm_ext::IForward);
        ));

        let execute_return = match &forward_call_return {
            syn::Type::Tuple(tuple) if tuple.elems.is_empty() => quote!(self.0.execute()?; Ok(())),
            _ => quote!(Ok(self.0.execute()?.into())),
        };

        output.extend(quote_spanned!(forward.ident.span() =>
            impl #type_ident<'_> {
                fn execute(&self, #forward_call_args) -> Result<#forward_call_return, sm_ext::SPError> {
                    use sm_ext::CallableParam;
                    #forward_call_pushes
                    #execute_return
                }
            }
        ));

        output_thread_locals.extend(quote_spanned!(forward.ident.span() =>
            #[allow(non_upper_case_globals)]
            static #global_ident: std::cell::RefCell<Option<sm_ext::IForward>> = std::cell::RefCell::new(None);
        ));

        output_trait.extend(quote_spanned!(forward.ident.span() =>
            fn #forward_ident<F, R>(f: F) -> R where F: FnOnce(&#type_ident) -> R;
        ));

        output_trait_impl_register.extend(quote_spanned!(forward.ident.span() =>
            let #forward_ident = forward_manager.create_forward(#forward_name, #forward_exec_type, &[#forward_param_types])?;
            #global_ident.with(|fwd| {
                *fwd.borrow_mut() = Some(#forward_ident);
            });
        ));

        output_trait_impl_unregister.extend(quote_spanned!(forward.ident.span() =>
            #global_ident.with(|fwd| {
                *fwd.borrow_mut() = None;
            });
        ));

        output_trait_impl.extend(quote_spanned!(forward.ident.span() =>
            fn #forward_ident<F, R>(f: F) -> R where F: FnOnce(&#type_ident) -> R {
                #global_ident.with(|fwd| {
                    let fwd = fwd.borrow();
                    let fwd = fwd.as_ref().unwrap();
                    let fwd = #type_ident(fwd);
                    f(&fwd)
                })
            }
        ));
    }

    output.extend(quote! {
        thread_local! {
            #output_thread_locals
        }
    });

    let struct_ident = &input.ident;
    let trait_ident = format_ident!("__{}_forwards", input.ident);

    output.extend(quote! {
        #[allow(non_camel_case_types)]
        trait #trait_ident {
            fn register(forward_manager: &sm_ext::IForwardManager) -> Result<(), sm_ext::CreateForwardError>;
            fn unregister();
            #output_trait
        }
    });

    output.extend(quote! {
        impl #trait_ident for #struct_ident {
            fn register(forward_manager: &sm_ext::IForwardManager) -> Result<(), sm_ext::CreateForwardError> {
                use sm_ext::CallableParam;
                #output_trait_impl_register
                Ok(())
            }

            fn unregister() {
                #output_trait_impl_unregister
            }

            #output_trait_impl
        }
    });

    // println!("{}", output.to_string());
    output.into()
}

#[proc_macro_attribute]
pub fn vtable(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let this_ptr_type = syn::parse_macro_input!(attr as syn::Path);
    let mut input = syn::parse_macro_input!(item as syn::ItemStruct);
    let mut output = TokenStream::new();

    // println!("{}", input.to_token_stream().to_string());

    input.attrs.push(syn::parse_quote!(#[doc(hidden)]));
    input.attrs.push(syn::parse_quote!(#[repr(C)]));

    let mut did_error = false;
    for field in &mut input.fields {
        if let syn::Type::BareFn(ty) = &mut field.ty {
            ty.unsafety = syn::parse_quote!(unsafe);
            ty.abi = syn::parse_quote!(extern "C");

            // Prepend the thisptr argument
            ty.inputs.insert(0, syn::parse_quote!(this: #this_ptr_type));
        } else {
            output.extend(error("All vtable struct fields must be bare functions", field.span()));
            did_error = true;
        }
    }

    if !did_error {
        input.attrs.push(syn::parse_quote!(#[cfg(not(all(windows, target_arch = "x86")))]));
    }

    output.extend(input.to_token_stream());

    if did_error {
        return output.into();
    }

    input.attrs.pop();
    input.attrs.push(syn::parse_quote!(#[cfg(all(windows, target_arch = "x86", feature = "thiscall"))]));

    for field in &mut input.fields {
        if let syn::Type::BareFn(ty) = &mut field.ty {
            if ty.variadic.is_none() {
                ty.abi = syn::parse_quote!(extern "thiscall");
            }
        }
    }

    output.extend(input.to_token_stream());

    input.attrs.pop();
    input.attrs.push(syn::parse_quote!(#[cfg(all(windows, target_arch = "x86", not(feature = "thiscall")))]));

    for field in &mut input.fields {
        if let syn::Type::BareFn(ty) = &mut field.ty {
            if ty.variadic.is_none() {
                ty.abi = syn::parse_quote!(extern "fastcall");

                // Add a dummy argument to be passed in edx
                ty.inputs.insert(1, syn::parse_quote!(_dummy: *const usize));
            }
        }
    }

    output.extend(input.to_token_stream());

    // println!("{}", output.to_string());

    output.into()
}

// TODO: This needs a lot of input checking and error reporting work
#[proc_macro_attribute]
pub fn vtable_override(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = syn::parse_macro_input!(item as syn::ItemFn);
    let mut output = TokenStream::new();

    // println!("{}", input.to_token_stream().to_string());

    input.attrs.push(syn::parse_quote!(#[cfg(not(all(windows, target_arch = "x86")))]));

    input.sig.abi = syn::parse_quote!(extern "C");

    output.extend(input.to_token_stream());

    input.attrs.pop();
    input.attrs.push(syn::parse_quote!(#[cfg(all(windows, target_arch = "x86", feature = "thiscall"))]));

    input.sig.abi = syn::parse_quote!(extern "thiscall");

    output.extend(input.to_token_stream());

    input.attrs.pop();
    input.attrs.push(syn::parse_quote!(#[cfg(all(windows, target_arch = "x86", not(feature = "thiscall")))]));

    // Add a dummy argument to be passed in edx
    input.sig.inputs.insert(1, syn::parse_quote!(_dummy: *const usize));

    input.sig.abi = syn::parse_quote!(extern "fastcall");

    output.extend(input.to_token_stream());

    // println!("{}", output.to_string());

    output.into()
}

fn error(s: &str, span: Span) -> TokenStream {
    let mut v = Vec::new();
    v.push(respan(Literal::string(&s), Span::call_site()));
    let group = v.into_iter().collect();

    let mut r = Vec::<TokenTree>::new();
    r.push(respan(Ident::new("compile_error", span), span));
    r.push(respan(Punct::new('!', Spacing::Alone), span));
    r.push(respan(Group::new(Delimiter::Brace, group), span));

    r.into_iter().collect()
}

fn respan<T: Into<TokenTree>>(t: T, span: Span) -> TokenTree {
    let mut t = t.into();
    t.set_span(span);
    t
}
