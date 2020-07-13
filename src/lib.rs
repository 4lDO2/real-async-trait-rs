extern crate proc_macro;

use std::iter;
use std::str::FromStr;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token;
use syn::{
    AngleBracketedGenericArguments, Binding, FnArg, GenericArgument, GenericParam, Generics, Ident,
    ItemImpl, ItemTrait, Lifetime, LifetimeDef, PatType, Path, PathArguments, PathSegment,
    ReturnType, Token, TraitBound, TraitBoundModifier, TraitItem, TraitItemType, Type,
    TypeParamBound, TypePath, TypeReference, TypeTuple,
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
    // FIXME
    quote! {
        //#item
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
        let (function_lifetimes, already_defined_lifetimes) = {
            //Global scope
            //let mut lifetimes = vec! [LifetimeDef::new(Lifetime::new("'static", Span::call_site()))];

            let mut lifetimes = Vec::new();
            // Trait definition scope
            lifetimes.extend(item.generics.lifetimes().cloned());
            // Function definition scope
            let function_lifetimes = method.sig.generics.lifetimes().cloned().collect::<Vec<_>>();
            lifetimes.extend(function_lifetimes.iter().cloned());
            (function_lifetimes, lifetimes)
        };

        let method_return_ty = match method.sig.output {
            ReturnType::Default => Box::new(Type::Tuple(TypeTuple {
                elems: Punctuated::new(),
                paren_token: token::Paren {
                    span: Span::call_site(),
                },
            })),
            ReturnType::Type(_, ref ty) => ty.clone(),
        };

        for input in method.sig.inputs.iter() {
            match input {
                FnArg::Receiver(ref recv) => {
                    if let Some((_ampersand, _lifetime @ None)) = &recv.reference {
                        panic!("{}self parameter lacked an explicit lifetime, which is required by this proc macro", if recv.mutability.is_some() { "&mut" } else { "&" });
                    }
                }
                FnArg::Typed(PatType { ref ty, .. }) => {
                    syn::visit::visit_type(&mut LifetimeVisitor, ty)
                }
            }
        }
        syn::visit::visit_type(&mut LifetimeVisitor, &method_return_ty);

        method.sig.asyncness = None;

        let gat_name = format!("__real_async_trait_impl_TypeFor_{}", method.sig.ident);
        let gat_ident = Ident::new(&gat_name, Span::call_site());

        new_gat_items.push(TraitItemType {
            attrs: Vec::new(),
            type_token: Token!(type)(Span::call_site()),
            bounds: iter::once(TypeParamBound::Trait(future_trait_bound(*method_return_ty)))
                .collect(),
            colon_token: Some(Token!(:)(Span::call_site())),
            default: None,
            generics: Generics {
                lt_token: Some(Token!(<)(Span::call_site())),
                gt_token: Some(Token!(>)(Span::call_site())),
                where_clause: None,
                params: function_lifetimes
                    .into_iter()
                    .map(GenericParam::Lifetime)
                    .collect(),
            },
            ident: gat_ident.clone(),
            semi_token: Token!(;)(Span::call_site()),
        });

        method.sig.output = ReturnType::Type(
            Token!(->)(Span::call_site()),
            Box::new(Type::Path(TypePath {
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
                            arguments: PathArguments::AngleBracketed(
                                AngleBracketedGenericArguments {
                                    colon2_token: None,
                                    lt_token: Token!(<)(Span::call_site()),
                                    gt_token: Token!(>)(Span::call_site()),
                                    args: already_defined_lifetimes
                                        .iter()
                                        .cloned()
                                        .map(|lifetime_def| {
                                            GenericArgument::Lifetime(lifetime_def.lifetime)
                                        })
                                        .collect(),
                                },
                            ),
                        },
                    ]
                    .into_iter()
                    .collect(),
                },
                qself: None,
            })),
        );
    }
    item.items
        .extend(new_gat_items.into_iter().map(TraitItem::Type));

    quote! {
        #item

        mod __real_async_trait_impl {
        }
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
