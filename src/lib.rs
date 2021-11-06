//!
//! # `#[real_async_trait]`
//! [![travis]](https://travis-ci.org/4lDO2/real-async-trait-rs)
//! [![cratesio]](https://crates.io/crates/real-async-trait)
//! [![docsrs]](https://docs.rs/real-async-trait/)
//!
//! [travis]: https://travis-ci.org/4lDO2/real-async-trait-rs.svg?branch=master
//! [cratesio]: https://img.shields.io/crates/v/real-async-trait.svg
//! [docsrs]: https://docs.rs/real-async-trait/badge.svg
//!
//! This crate provides a producedural macro that works around the current limitation of not being
//! able to put `async fn`s in a trait, _without type erasure_, by using experimental
//! nightly-features, namely [generic associated types
//! (GATs)](https://github.com/rust-lang/rfcs/blob/master/text/1598-generic_associated_types.md)
//! and [existential
//! types](https://github.com/rust-lang/rfcs/blob/master/text/2515-type_alias_impl_trait.md).
//!
//! ## Caveats
//!
//! While this proc macro will allow you to write non-type-erased allocation-free async fns within
//! traits, there are a few caveats to this (non-exhaustive):
//!
//! * at the moment, all references used in the async fn, must have their lifetimes be explicitly
//! specified, either from the top-level of the trait, or in the function declaration;
//! * there can only be a single lifetime in use simultaneously. It appears to be caused by
//! [#61756](https://github.com/rust-lang/rust/issues/61756), but could perhaps be worked around
//! using a `Capture` trait;
//! * While this proc macro allows traits to also require a `Send` trait bound on futures, it is
//! not particularly trivial if at all possible, to require that the future implement `Send`, if
//! and only if `Self: Send`. It _may_ be possible using some HRTB hack, but would require a
//! language feature in the long term.
//! * since GATs are an "incomplete" feature in rust, it may not be sound or just not compile
//! correctly or at all. __Don't use this in production code!__
//!
//! ## Example
//! ```
//! #![feature(generic_associated_types, type_alias_impl_trait)]
//! use real_async_trait::real_async_trait;
//!
//! /// An error code, similar to `errno` in C.
//! pub type Errno = usize;
//!
//! /// A UNIX-like file descriptor.
//! pub type FileDescriptor = usize;
//!
//! /// "No such file or directory"
//! pub const ENOENT: usize = 1;
//!
//! /// "Bad file descriptor"
//! pub const EBADF: usize = 2;
//!
//! /// A filesystem-like primitive, used in the Redox Operating System.
//! #[real_async_trait]
//! pub trait RedoxScheme {
//!     /// Some async fns can force the implementor to also implement `Send` for the future.
//!     #[send]
//!     async fn open<'a>(&'a self, path: &'a [u8], flags: usize) -> Result<FileDescriptor, Errno>;
//!     async fn read<'a>(&'a self, fd: FileDescriptor, buffer: &'a mut [u8]) -> Result<usize, Errno>;
//!     async fn write<'a>(&'a self, fd: FileDescriptor, buffer: &'a [u8]) -> Result<usize, Errno>;
//!     async fn close<'a>(&'a self, fd: FileDescriptor) -> Result<(), Errno>;
//! }
//!
//! /// A scheme that does absolutely nothing.
//! struct MyNothingScheme;
//!
//! #[real_async_trait]
//! impl RedoxScheme for MyNothingScheme {
//!     #[send]
//!     async fn open<'a>(&'a self, path: &'a [u8], flags: usize) -> Result<FileDescriptor, Errno> {
//!         // I can write async code in here!
//!         Err(ENOENT)
//!     }
//!     async fn read<'a>(&'a self, fd: FileDescriptor, buffer: &'a mut [u8]) -> Result<usize, Errno> {
//!         Err(EBADF)
//!     }
//!     async fn write<'a>(&'a self, fd: FileDescriptor, path: &'a [u8]) -> Result<usize, Errno> {
//!         Err(EBADF)
//!     }
//!     async fn close<'a>(&'a self, fd: FileDescriptor) -> Result<(), Errno> {
//!         Err(EBADF)
//!     }
//! }
//!
//! # #[async_std::main]
//! # async fn main() {
//!
//! let my_nothing_scheme = MyNothingScheme;
//!
//! assert_eq!(my_nothing_scheme.open(b"nothing exists here", 0).await, Err(ENOENT), "why would anything exist here?");
//! assert_eq!(my_nothing_scheme.read(1337, &mut []).await, Err(EBADF));
//! assert_eq!(my_nothing_scheme.write(1337, &[]).await, Err(EBADF));
//! assert_eq!(my_nothing_scheme.close(1337).await, Err(EBADF));
//!
//! # }
//!
//! ```
//! ## How it works
//!
//! Under the hood, this proc macro will insert generic associated types (GATs) for the the futures
//! that are the return types of the async fns in the trait definition. The macro will generate the
//! following for the `RedoxScheme` trait (simplified generated names):
//!
//! ```
//! # #![feature(generic_associated_types)]
//! # struct FileDescriptor;
//! # struct Errno;
//! pub trait RedoxScheme {
//!     // Downgraded functions, from async fn to fn. Their types have changed into a generic
//!     // associated type.
//!     fn open<'a>(&'a self, path: &'a [u8], flags: usize) -> Self::OpenFuture<'a>;
//!     fn read<'a>(&'a self, fd: FileDescriptor, buf: &'a mut [u8]) -> Self::ReadFuture<'a>;
//!     fn write<'a>(&'a self, fd: FileDescriptor, buf: &'a [u8]) -> Self::WriteFuture<'a>;
//!     fn close<'a>(&'a self, fd: FileDescriptor) -> Self::CloseFuture<'a>;
//!
//!     // Generic associated types, the return values are moved to here.
//!     // NOTE: all #[send] methods also get the `Send` trait bound.
//!     type OpenFuture<'a>: ::core::future::Future<Output = Result<FileDescriptor, Errno>> +
//!         ::core::marker::Send + 'a;
//!     type ReadFuture<'a>: ::core::future::Future<Output = Result<usize, Errno>> + 'a;
//!     type WriteFuture<'a>: ::core::future::Future<Output = Result<usize, Errno>> + 'a;
//!     type CloseFuture<'a>: ::core::future::Future<Output = Result<(), Errno>> + 'a;
//! }
//! ```
//!
//! Meanwhile, the impls will get the following generated code (simplified here as well):
//!
//! ```
//! # #![feature(generic_associated_types, type_alias_impl_trait)]
//! # struct FileDescriptor;
//! # struct Errno;
//! # pub trait RedoxScheme {
//! #   fn open<'a>(&'a self, path: &'a [u8], flags: usize) -> Self::OpenFuture<'a>;
//! #   fn read<'a>(&'a self, fd: FileDescriptor, buf: &'a mut [u8]) -> Self::ReadFuture<'a>;
//! #   fn write<'a>(&'a self, fd: FileDescriptor, buf: &'a [u8]) -> Self::WriteFuture<'a>;
//! #   fn close<'a>(&'a self, fd: FileDescriptor) -> Self::CloseFuture<'a>;
//! #   type OpenFuture<'a>: ::core::future::Future<Output = Result<FileDescriptor, Errno>> + Send + 'a;
//! #   type ReadFuture<'a>: ::core::future::Future<Output = Result<usize, Errno>> + 'a;
//! #   type WriteFuture<'a>: ::core::future::Future<Output = Result<usize, Errno>> + 'a;
//! #   type CloseFuture<'a>: ::core::future::Future<Output = Result<(), Errno>> + 'a;
//! # }
//! # const ENOENT: Errno = Errno;
//! # const EBADF: Errno = Errno;
//!
//! // Wrap everything in a private module to prevent the existential types from leaking.
//! // XXX: Apparently rust doctests don't work that great with private modules :(
//! // mod __private {
//! #   struct MyNothingScheme;
//!     impl RedoxScheme for MyNothingScheme {
//!         // Async fns are downgraded here as well, and the same thing goes with the return
//!         // values.
//!         fn open<'a>(&'a self, path: &'a [u8], flags: usize) -> Self::OpenFuture<'a> {
//!             // All expressions in async fns are wrapped in async blocks. The compiler will
//!             // automagically figure out the actual types of the existential type aliases, even
//!             // though they are anonymous.
//!             async move { Err(ENOENT) }
//!         }
//!         fn read<'a>(&'a self, fd: FileDescriptor, buf: &'a mut [u8]) -> Self::ReadFuture<'a> {
//!             async move { Err(EBADF) }
//!         }
//!         fn write<'a>(&'a self, fd: FileDescriptor, buf: &'a [u8]) -> Self::WriteFuture<'a> {
//!             async move { Err(EBADF) }
//!         }
//!         fn close<'a>(&'a self, fd: FileDescriptor) -> Self::CloseFuture<'a> {
//!             async move { Err(EBADF) }
//!         }
//!
//!         // This is the part where the existential types come in. Currently, there is no possible
//!         // way to use types within type aliases inside of trait implementations, that aren't
//!         // publicly accessible. As we need some way to assign a user-visible type to the async
//!         // blocks, and we don't want the user to have to box futures, existential types are the
//!         // solution.  use existential types.
//!         type OpenFuture<'a> = OpenFutureExistentialType<'a>;
//!         type ReadFuture<'a> = ReadFutureExistentialType<'a>;
//!         type WriteFuture<'a> = WriteFutureExistentialType<'a>;
//!         type CloseFuture<'a> = CloseFutureExistentialType<'a>;
//!     }
//!     // This is where the return values actually are defined. At the moment these type alises
//!     // with impl trait can only occur outside of the trait itself, unfortunately. There can
//!     // only be one type that this type alias refers to, which the compiler will keep track of.
//!     type OpenFutureExistentialType<'a> = impl ::core::future::Future<Output = Result<FileDescriptor, Errno>> +
//!         ::core::marker::Send + 'a;
//!     type ReadFutureExistentialType<'a> = impl ::core::future::Future<Output = Result<usize, Errno>> + 'a;
//!     type WriteFutureExistentialType<'a> = impl ::core::future::Future<Output = Result<usize, Errno>> + 'a;
//!     type CloseFutureExistentialType<'a> = impl ::core::future::Future<Output = Result<(), Errno>> + 'a;
//! // }
//! ```
//!

extern crate proc_macro;

use std::str::FromStr;
use std::{iter, mem};

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token;
use syn::{
    AngleBracketedGenericArguments, AttrStyle, Attribute, Binding, Block, Expr, ExprAsync, FnArg,
    GenericArgument, GenericParam, Generics, Ident, ImplItem, ImplItemType, ItemImpl, ItemTrait,
    ItemType, Lifetime, LifetimeDef, PatType, Path, PathArguments, PathSegment, ReturnType,
    Signature, Stmt, Token, TraitBound, TraitBoundModifier, TraitItem, TraitItemType, Type,
    TypeImplTrait, TypeParamBound, TypePath, TypeReference, TypeTuple, Visibility,
};

mod tests;

struct LifetimeVisitor;

impl<'ast> syn::visit::Visit<'ast> for LifetimeVisitor {
    fn visit_type_reference(&mut self, i: &'ast TypeReference) {
        if i.lifetime.is_none() {
            panic!("Reference at {:?} lacked an explicit lifetime, which is required by this proc macro", i.and_token.span);
        }
    }
}

fn handle_item_impl(mut item: ItemImpl) -> TokenStream {
    let mut existential_type_defs = Vec::new();
    let mut gat_defs = Vec::new();

    for method in item
        .items
        .iter_mut()
        .filter_map(|item| {
            if let ImplItem::Method(method) = item {
                Some(method)
            } else {
                None
            }
        })
        .filter(|method| method.sig.asyncness.is_some())
    {
        method.sig.asyncness = None;

        validate_that_function_always_has_lifetimes(&method.sig);

        let (toplevel_lifetimes, function_lifetimes) =
            already_defined_lifetimes(&item.generics, &method.sig.generics);

        let existential_type_name = format!(
            "__real_async_trait_impl_ExistentialTypeFor_{}",
            method.sig.ident
        );
        let existential_type_ident = Ident::new(&existential_type_name, Span::call_site());

        let requires_send = method_requires_send(&mut method.attrs);

        existential_type_defs.push(ItemType {
            attrs: Vec::new(),
            eq_token: Token!(=)(Span::call_site()),
            generics: Generics {
                gt_token: Some(Token!(>)(Span::call_site())),
                lt_token: Some(Token!(<)(Span::call_site())),
                params: toplevel_lifetimes
                    .iter()
                    .cloned()
                    .map(GenericParam::Lifetime)
                    .collect(),
                where_clause: None,
            },
            ident: existential_type_ident,
            semi_token: Token!(;)(Span::call_site()),
            vis: Visibility::Inherited,
            ty: Box::new(Type::ImplTrait(TypeImplTrait {
                bounds: iter::once(TypeParamBound::Trait(future_trait_bound(return_type(
                    method.sig.output.clone(),
                ))))
                .chain(if requires_send {
                    Some(TypeParamBound::Trait(send_trait_bound()))
                } else {
                    None
                })
                .chain(
                    toplevel_lifetimes
                        .iter()
                        .cloned()
                        .map(|lifetime_def| TypeParamBound::Lifetime(lifetime_def.lifetime)),
                )
                .collect(),
                impl_token: Token!(impl)(Span::call_site()),
            })),
            type_token: Token!(type)(Span::call_site()),
        });

        let existential_type_path_for_impl = Path {
            // self::__real_async_trait_impl_ExistentialTypeFor_FUNCTIONNAME
            leading_colon: None,
            segments: vec![
                PathSegment {
                    arguments: PathArguments::None,
                    ident: Ident::new("self", Span::call_site()),
                },
                PathSegment {
                    arguments: PathArguments::AngleBracketed(lifetime_angle_bracketed_bounds(
                        toplevel_lifetimes
                            .into_iter()
                            .map(|lifetime_def| lifetime_def.lifetime),
                    )),
                    ident: Ident::new(&existential_type_name, Span::call_site()),
                },
            ]
            .into_iter()
            .collect(),
        };
        let existential_path_type = Type::Path(TypePath {
            path: existential_type_path_for_impl,
            qself: None,
        });

        let gat_ident = gat_ident_for_sig(&method.sig);

        gat_defs.push(ImplItemType {
            attrs: Vec::new(),
            defaultness: None,
            eq_token: Token!(=)(Span::call_site()),
            generics: Generics {
                lt_token: Some(Token!(<)(Span::call_site())),
                gt_token: Some(Token!(>)(Span::call_site())),
                where_clause: None,
                params: function_lifetimes
                    .iter()
                    .cloned()
                    .map(GenericParam::Lifetime)
                    .collect(),
            },
            ident: gat_ident.clone(),
            semi_token: Token!(;)(Span::call_site()),
            ty: existential_path_type.clone(),
            type_token: Token!(type)(Span::call_site()),
            vis: Visibility::Inherited,
        });

        let gat_self_type = self_gat_type(
            gat_ident,
            function_lifetimes
                .into_iter()
                .map(|lifetime_def| lifetime_def.lifetime),
        );

        method.sig.output = ReturnType::Type(
            Token!(->)(Span::call_site()),
            Box::new(gat_self_type.into()),
        );

        let method_stmts = mem::replace(&mut method.block.stmts, Vec::new());

        method.block.stmts = vec![Stmt::Expr(Expr::Async(ExprAsync {
            async_token: Token!(async)(Span::call_site()),
            attrs: Vec::new(),
            block: Block {
                brace_token: token::Brace {
                    span: Span::call_site(),
                },
                stmts: method_stmts,
            },
            capture: Some(Token!(move)(Span::call_site())),
        }))];
    }

    item.items.extend(gat_defs.into_iter().map(Into::into));

    quote! {

        const _: () = {
            #item

            #(#existential_type_defs)*
        };
    }
}

fn return_type(retval: ReturnType) -> Type {
    match retval {
        ReturnType::Default => Type::Tuple(TypeTuple {
            elems: Punctuated::new(),
            paren_token: token::Paren {
                span: Span::call_site(),
            },
        }),
        ReturnType::Type(_, ty) => *ty,
    }
}

fn future_trait_bound(fn_output_ty: Type) -> TraitBound {
    const FUTURE_TRAIT_PATH_STR: &str = "::core::future::Future";
    const FUTURE_TRAIT_OUTPUT_IDENT_STR: &str = "Output";

    let mut future_trait_path =
        syn::parse2::<Path>(TokenStream::from_str(FUTURE_TRAIT_PATH_STR).unwrap())
            .expect("failed to parse `::core::future::Future` as a syn `Path`");

    let future_angle_bracketed_args = AngleBracketedGenericArguments {
        colon2_token: None, // FIXME
        lt_token: Token!(<)(Span::call_site()),
        gt_token: Token!(>)(Span::call_site()),
        args: iter::once(GenericArgument::Binding(Binding {
            ident: Ident::new(FUTURE_TRAIT_OUTPUT_IDENT_STR, Span::call_site()),
            eq_token: Token!(=)(Span::call_site()),
            ty: fn_output_ty,
        }))
        .collect(),
    };

    future_trait_path
        .segments
        .last_mut()
        .expect("Expected ::core::future::Future to have `Future` as the last segment")
        .arguments = PathArguments::AngleBracketed(future_angle_bracketed_args);

    TraitBound {
        // for TraitBounds, these are HRTBs, which are useless since there are already GATs present
        lifetimes: None,
        // This is not ?Sized or something like that
        modifier: TraitBoundModifier::None,
        paren_token: None,
        path: future_trait_path,
    }
}
fn send_trait_bound() -> TraitBound {
    const SEND_TRAIT_PATH_STR: &str = "::core::marker::Send";

    let send_trait_path = syn::parse2::<Path>(TokenStream::from_str(SEND_TRAIT_PATH_STR).unwrap())
        .expect("failed to parse `::core::marker::Send` as a syn `Path`");

    TraitBound {
        lifetimes: None,
        modifier: TraitBoundModifier::None,
        paren_token: None,
        path: send_trait_path,
    }
}

fn validate_that_function_always_has_lifetimes(signature: &Signature) {
    for input in signature.inputs.iter() {
        match input {
            FnArg::Receiver(ref recv) => {
                if let Some((_ampersand, _lifetime @ None)) = &recv.reference {
                    panic!("{}self parameter lacked an explicit lifetime, which is required by this proc macro", if recv.mutability.is_some() { "&mut " } else { "&" });
                }
            }
            FnArg::Typed(PatType { ref ty, .. }) => {
                syn::visit::visit_type(&mut LifetimeVisitor, ty)
            }
        }
    }
    if let ReturnType::Type(_, ref ty) = signature.output {
        syn::visit::visit_type(&mut LifetimeVisitor, ty);
    };
}
fn already_defined_lifetimes(
    toplevel_generics: &Generics,
    method_generics: &Generics,
) -> (Vec<LifetimeDef>, Vec<LifetimeDef>) {
    //Global scope
    //let mut lifetimes = vec! [LifetimeDef::new(Lifetime::new("'static", Span::call_site()))];

    let mut lifetimes = Vec::new();
    // Trait definition scope
    lifetimes.extend(toplevel_generics.lifetimes().cloned());
    // Function definition scope
    let function_lifetimes = method_generics.lifetimes().cloned().collect::<Vec<_>>();
    lifetimes.extend(function_lifetimes.iter().cloned());
    (lifetimes, function_lifetimes)
}
fn lifetime_angle_bracketed_bounds(
    lifetimes: impl IntoIterator<Item = Lifetime>,
) -> AngleBracketedGenericArguments {
    AngleBracketedGenericArguments {
        colon2_token: None,
        lt_token: Token!(<)(Span::call_site()),
        gt_token: Token!(>)(Span::call_site()),
        args: lifetimes
            .into_iter()
            .map(GenericArgument::Lifetime)
            .collect(),
    }
}
fn gat_ident_for_sig(sig: &Signature) -> Ident {
    let gat_name = format!("__real_async_trait_impl_TypeFor_{}", sig.ident);
    Ident::new(&gat_name, Span::call_site())
}
fn self_gat_type(
    gat_ident: Ident,
    function_lifetimes: impl IntoIterator<Item = Lifetime>,
) -> TypePath {
    TypePath {
        path: Path {
            // represents the pattern Self::GAT_NAME...
            leading_colon: None,
            segments: vec![
                PathSegment {
                    ident: Ident::new("Self", Span::call_site()),
                    arguments: PathArguments::None,
                },
                PathSegment {
                    ident: gat_ident,
                    arguments: PathArguments::AngleBracketed(lifetime_angle_bracketed_bounds(
                        function_lifetimes,
                    )),
                },
            ]
            .into_iter()
            .collect(),
        },
        qself: None,
    }
}
fn method_requires_send(attrs: &mut Vec<Attribute>) -> bool {
    if let Some(first_attr_idx) = attrs.iter().position(|attr| {
        // This represents #[send], which will be the most common way to express the Send
        // future bound.
        let is_direct_send = attr.path.is_ident("send");

        // However, in some edge-cases, it may not be impossible that this proc macro is
        // combined with another one, that has a special use-case for `#[send]`. Thus, we will
        // also allow the user to type `#[::real_async_trait::send]` instead.
        let is_indirect_send = {
            let has_leading_colon = attr.path.leading_colon.is_some();
            let corresponding_segments = [
                PathSegment {
                    ident: Ident::new("real_async_trait", Span::call_site()),
                    arguments: PathArguments::None,
                },
                PathSegment {
                    ident: Ident::new("send", Span::call_site()),
                    arguments: PathArguments::None,
                },
            ];

            has_leading_colon
                && Iterator::eq(attr.path.segments.iter(), corresponding_segments.iter())
        };

        is_direct_send || is_indirect_send
    }) {
        let attr = attrs.remove(first_attr_idx);
        assert_eq!(attr.style, AttrStyle::Outer);
        true
    } else {
        false
    }
}
fn handle_item_trait(mut item: ItemTrait) -> TokenStream {
    let mut new_gat_items = Vec::new();

    // Loop through every single async fn declared in the trait.
    for method in item
        .items
        .iter_mut()
        .filter_map(|item| {
            if let TraitItem::Method(func) = item {
                Some(func)
            } else {
                None
            }
        })
        .filter(|method| method.sig.asyncness.is_some())
    {
        // For each async fn, remove the async part, replace the return value with a generic
        // associated type, and add that generic associated type to the trait item.

        // Check that all types have a lifetime that is either specific to the trait item, or
        // to the current function (or 'static). Any other lifetime will and must produce a
        // compiler error.
        let gat_ident = gat_ident_for_sig(&method.sig);

        let requires_send = method_requires_send(&mut method.attrs);
        let method_return_ty = return_type(method.sig.output.clone());

        validate_that_function_always_has_lifetimes(&method.sig);

        method.sig.asyncness = None;

        let (toplevel_lifetimes, function_lifetimes) =
            already_defined_lifetimes(&item.generics, &method.sig.generics);

        new_gat_items.push(TraitItemType {
            attrs: Vec::new(),
            type_token: Token!(type)(Span::call_site()),
            bounds: iter::once(TypeParamBound::Trait(future_trait_bound(method_return_ty)))
                .chain(if requires_send {
                    Some(TypeParamBound::Trait(send_trait_bound()))
                } else {
                    None
                })
                .chain(
                    toplevel_lifetimes
                        .into_iter()
                        .map(|lifetime_def| lifetime_def.lifetime)
                        .map(TypeParamBound::Lifetime),
                )
                .collect(),
            colon_token: Some(Token!(:)(Span::call_site())),
            default: None,
            generics: Generics {
                lt_token: Some(Token!(<)(Span::call_site())),
                gt_token: Some(Token!(>)(Span::call_site())),
                where_clause: None,
                params: function_lifetimes
                    .iter()
                    .cloned()
                    .map(GenericParam::Lifetime)
                    .collect(),
            },
            ident: gat_ident.clone(),
            semi_token: Token!(;)(Span::call_site()),
        });

        let self_gat_type = self_gat_type(
            gat_ident,
            function_lifetimes
                .into_iter()
                .map(|lifetime_def| lifetime_def.lifetime),
        );

        method.sig.output = ReturnType::Type(
            Token!(->)(Span::call_site()),
            Box::new(self_gat_type.into()),
        );
    }
    item.items
        .extend(new_gat_items.into_iter().map(TraitItem::Type));

    quote! {
        #item
    }
}
fn real_async_trait2(_args_stream: TokenStream, token_stream: TokenStream) -> TokenStream {
    // The #[real_async_trait] attribute macro, is applicable to both trait blocks, and to impl
    // blocks that operate on that trait.

    if let Ok(item_trait) = syn::parse2::<ItemTrait>(token_stream.clone()) {
        handle_item_trait(item_trait)
    } else if let Ok(item_impl) = syn::parse2::<ItemImpl>(token_stream) {
        handle_item_impl(item_impl)
    } else {
        panic!("expected either a trait or an impl item")
    }
}

/// A proc macro that supports using async fn in traits and trait impls. Refer to the top-level
/// crate documentation for more information.
#[proc_macro_attribute]
pub fn real_async_trait(
    args_stream: proc_macro::TokenStream,
    token_stream: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    real_async_trait2(args_stream.into(), token_stream.into()).into()
}
