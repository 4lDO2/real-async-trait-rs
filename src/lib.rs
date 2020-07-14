extern crate proc_macro;

use std::str::FromStr;
use std::{iter, mem};

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token;
use syn::{
    AngleBracketedGenericArguments, Binding, Block, Expr, ExprAsync, FnArg, GenericArgument,
    GenericParam, Generics, Ident, ImplItem, ImplItemType, ItemImpl, ItemTrait, ItemType, Lifetime,
    LifetimeDef, PatType, Path, PathArguments, PathSegment, ReturnType, Signature, Stmt, Token,
    TraitBound, TraitBoundModifier, TraitItem, TraitItemType, Type, TypeImplTrait, TypeParamBound,
    TypePath, TypeReference, TypeTuple, VisRestricted, Visibility,
};

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
            vis: Visibility::Restricted(VisRestricted {
                in_token: None,
                paren_token: token::Paren {
                    span: Span::call_site(),
                },
                pub_token: Token!(pub)(Span::call_site()),
                path: Box::new(Path {
                    leading_colon: None,
                    segments: iter::once(PathSegment {
                        arguments: PathArguments::None,
                        ident: Ident::new("super", Span::call_site()),
                    })
                    .collect(),
                }),
            }),
            ty: Box::new(Type::ImplTrait(TypeImplTrait {
                bounds: iter::once(TypeParamBound::Trait(future_trait_bound(return_type(
                    method.sig.output.clone(),
                ))))
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
            // self::__real_async_trait_impl::__real_async_trait_impl_ExistentialTypeFor_FUNCTIONNAME
            leading_colon: None,
            segments: vec![
                PathSegment {
                    arguments: PathArguments::None,
                    ident: Ident::new("self", Span::call_site()),
                },
                PathSegment {
                    arguments: PathArguments::None,
                    ident: Ident::new("__real_async_trait_impl", Span::call_site()),
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
        #item

        mod __real_async_trait_impl {
            use super::*;

            #(#existential_type_defs)*
        }
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
            .map(|lifetime_def| GenericArgument::Lifetime(lifetime_def))
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

        let method_return_ty = return_type(method.sig.output.clone());

        validate_that_function_always_has_lifetimes(&method.sig);

        method.sig.asyncness = None;

        let (_toplevel_lifetimes, function_lifetimes) =
            already_defined_lifetimes(&item.generics, &method.sig.generics);

        new_gat_items.push(TraitItemType {
            attrs: Vec::new(),
            type_token: Token!(type)(Span::call_site()),
            bounds: iter::once(TypeParamBound::Trait(future_trait_bound(method_return_ty)))
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

#[proc_macro_attribute]
pub fn real_async_trait(
    _args_stream: proc_macro::TokenStream,
    token_stream: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // The #[real_async_trait] attribute macro, is applicable to both trait blocks, and to impl
    // blocks that operate on that trait.

    if let Ok(item_trait) = syn::parse::<ItemTrait>(token_stream.clone()) {
        handle_item_trait(item_trait)
    } else if let Ok(item_impl) = syn::parse::<ItemImpl>(token_stream) {
        handle_item_impl(item_impl)
    } else {
        panic!("expected either a trait or an impl item")
    }
    .into()
}
