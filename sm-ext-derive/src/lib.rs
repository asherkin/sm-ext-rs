extern crate proc_macro;
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::{format_ident, quote, quote_spanned, ToTokens, TokenStreamExt};
use syn;
use syn::spanned::Spanned;

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
        #[no_mangle]
        pub extern "C" fn GetSMExtAPI() -> *mut ::sm_ext::IExtensionInterfaceAdapter<#name> {
            let delegate = #name();
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

#[proc_macro_attribute]
pub fn native(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as syn::ItemFn);
    // println!("{:#?}", input);

    let mut output = TokenStream::new();

    if let Some(asyncness) = &input.sig.asyncness {
        let span = asyncness.span();
        output.extend(error("Native callback must not be async", span, span));
    }

    if let Some(unsafety) = &input.sig.unsafety {
        let span = unsafety.span();
        output.extend(error("Native callback must not be unsafe", span, span));
    }

    if let Some(abi) = &input.sig.abi {
        let span = abi.span();
        output.extend(error("Native callback must use the default Rust ABI", span, span));
    }

    if !input.sig.generics.params.is_empty() {
        let span = input.sig.generics.span();
        output.extend(error("Native callback must not have any generic parameters", span, span));
    }

    let mut param_count: i32 = 0;
    let mut param_output = TokenStream::new();
    for param in &input.sig.inputs {
        match param {
            syn::FnArg::Receiver(param) => {
                let span = param.span();
                output.extend(error("Native callback must not be a method", span, span));
            }
            syn::FnArg::Typed(param) => {
                param_count += 1;
                if param_count == 1 {
                    param_output.extend(quote_spanned!(param.span() => &ctx,));
                    continue;
                }

                let param_idx = (param_count - 1) as isize;
                param_output.extend(quote_spanned!(param.span() => (*(args.offset(#param_idx))).try_into_plugin(&ctx)?,));
            }
        };
    }

    let args_minimum = param_count - 1; // TODO: Handle optional args.
    let callback_ident = &input.sig.ident;
    let wrapper_ident = format_ident!("__{}_adapter", callback_ident);
    output.extend(quote! {
        unsafe extern "C" fn #wrapper_ident(ctx: sm_ext::types::IPluginContextPtr, args: *const sm_ext::types::cell_t) -> sm_ext::types::cell_t {
            let ctx = sm_ext::IPluginContext(ctx);

            sm_ext::safe_native_invoke(&ctx, || -> Result<cell_t, Box<dyn std::error::Error>> {
                use std::convert::TryInto;
                use sm_ext::types::TryIntoWithContext;

                let count: i32 = (*args).into();
                if count < #args_minimum {
                    return Err(format!("Not enough arguments, got {}, expected at least {}", count, #args_minimum).into());
                }

                let result = #callback_ident(
                    #param_output
                )?;

                Ok(result.try_into_plugin(&ctx)?)
            })
        }
    });

    output.extend(input.to_token_stream());

    // println!("{}", output.to_string());

    output.into()
}

#[proc_macro_attribute]
pub fn vtable(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let this_ptr_type = syn::parse_macro_input!(attr as syn::Path);
    let mut input = syn::parse_macro_input!(item as syn::ItemStruct);
    let mut output = TokenStream::new();

    // println!("{}", input.to_token_stream().to_string());

    input.attrs.push(syn::parse_quote!(#[repr(C)]));

    for field in &mut input.fields {
        if let syn::Type::BareFn(ty) = &mut field.ty {
            ty.unsafety = syn::parse_quote!(unsafe);

            // TODO: The dummy arg needs inserting for doing fastcall on Windows.
            ty.inputs.insert(0, syn::parse_quote!(this: #this_ptr_type));

            // TODO: This depends on the platform (once we move away from thiscall) and whether there are varargs
            ty.abi = Some(match ty.variadic {
                Some(_) => syn::parse_quote!(extern "cdecl"),
                None => syn::parse_quote!(extern "thiscall"),
            });
        } else {
            let span = field.span();
            output.extend(error("All vtable struct fields must be bare functions", span, span));
        }
    }

    output.extend(input.to_token_stream());

    // println!("{}", output.to_string());

    output.into()
}

fn error(s: &str, start: Span, end: Span) -> TokenStream {
    let mut v = Vec::new();
    v.push(respan(Literal::string(&s), Span::call_site()));
    let group = v.into_iter().collect();

    let mut r = Vec::<TokenTree>::new();
    r.push(respan(Ident::new("compile_error", start), start));
    r.push(respan(Punct::new('!', Spacing::Alone), Span::call_site()));
    r.push(respan(Group::new(Delimiter::Brace, group), end));

    r.into_iter().collect()
}

fn respan<T: Into<TokenTree>>(t: T, span: Span) -> TokenTree {
    let mut t = t.into();
    t.set_span(span);
    t
}
