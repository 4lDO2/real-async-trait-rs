#[test]
fn correct_trait_output() {
    let input = quote::quote! {
        pub trait RedoxScheme {
            async fn open<'a>(&'a mut self, path: &'a [u8], flags: usize) -> Result<usize, Errno>;
            async fn read<'a>(&'a mut self, fd: usize, buf: &'a mut [u8]) -> Result<usize, Errno>;
            async fn write<'a>(&'a mut self, fd: usize, buf: &'a [u8]) -> Result<usize, Errno>;
            async fn close<'a>(&'a mut self, fd: usize) -> Result<(), Errno>;
        }
    };
    let expected_output = quote::quote! {
        pub trait RedoxScheme {
            fn open<'a>(&'a mut self, path: &'a [u8], flags: usize) -> Self::__real_async_trait_impl_TypeFor_open<'a>;
            fn read<'a>(&'a mut self, fd: usize, buf: &'a mut [u8]) -> Self::__real_async_trait_impl_TypeFor_read<'a>;
            fn write<'a>(&'a mut self, fd: usize, buf: &'a [u8]) -> Self::__real_async_trait_impl_TypeFor_write<'a>;
            fn close<'a>(&'a mut self, fd: usize) -> Self::__real_async_trait_impl_TypeFor_close<'a>;

            type __real_async_trait_impl_TypeFor_open<'a>: ::core::future::Future<Output = Result<usize, Errno>> + 'a;
            type __real_async_trait_impl_TypeFor_read<'a>: ::core::future::Future<Output = Result<usize, Errno>> + 'a;
            type __real_async_trait_impl_TypeFor_write<'a>: ::core::future::Future<Output = Result<usize, Errno>> + 'a;
            type __real_async_trait_impl_TypeFor_close<'a>: ::core::future::Future<Output = Result<(), Errno>> + 'a;
        }
    };
    let actual_output = crate::real_async_trait2(proc_macro2::TokenStream::new(), input);

    // TODO: Any better way to do this?
    let expected_output_trait = syn::parse2::<syn::Item>(expected_output).unwrap();
    let actual_output_trait = syn::parse2::<syn::Item>(actual_output).unwrap();

    assert_eq!(expected_output_trait, actual_output_trait);
}
#[test]
fn correct_impl_output() {
    let input = quote::quote! {
        impl RedoxScheme for MyType {
            async fn open<'a>(&'a mut self, path: &'a [u8], flags: usize) -> Result<usize, Errno> {
                Ok(0)
            }
            async fn read<'a>(&'a mut self, fd: usize, buf: &'a mut [u8]) -> Result<usize, Errno> {
                Ok(0)
            }
            async fn write<'a>(&'a mut self, fd: usize, buf: &'a [u8]) -> Result<usize, Errno> {
                Ok(0)
            }
            async fn close<'a>(&'a mut self, fd: usize) -> Result<(), Errno> {
                Ok(())
            }
        }
    };
    let expected_output = quote::quote! {
        mod __real_async_trait_impl {
            use super::*;
            impl RedoxScheme for MyType {
                fn open<'a>(&'a mut self, path: &'a [u8], flags: usize) -> Self::__real_async_trait_impl_TypeFor_open<'a> {
                    async move { Ok(0) }
                }
                fn read<'a>(&'a mut self, fd: usize, buf: &'a mut [u8]) -> Self::__real_async_trait_impl_TypeFor_read<'a> {
                    async move { Ok(0) }
                }
                fn write<'a>(&'a mut self, fd: usize, buf: &'a [u8]) -> Self::__real_async_trait_impl_TypeFor_write<'a> {
                    async move { Ok(0) }
                }
                fn close<'a>(&'a mut self, fd: usize) -> Self::__real_async_trait_impl_TypeFor_close<'a> {
                    async move { Ok(()) }
                }

                type __real_async_trait_impl_TypeFor_open<'a> = self::__real_async_trait_impl_ExistentialTypeFor_open<'a>;
                type __real_async_trait_impl_TypeFor_read<'a> = self::__real_async_trait_impl_ExistentialTypeFor_read<'a>;
                type __real_async_trait_impl_TypeFor_write<'a> = self::__real_async_trait_impl_ExistentialTypeFor_write<'a>;
                type __real_async_trait_impl_TypeFor_close<'a> = self::__real_async_trait_impl_ExistentialTypeFor_close<'a>;
            }
            type __real_async_trait_impl_ExistentialTypeFor_open<'a> = impl ::core::future::Future<Output = Result<usize, Errno>> + 'a;
            type __real_async_trait_impl_ExistentialTypeFor_read<'a> = impl ::core::future::Future<Output = Result<usize, Errno>> + 'a;
            type __real_async_trait_impl_ExistentialTypeFor_write<'a> = impl ::core::future::Future<Output = Result<usize, Errno>> + 'a;
            type __real_async_trait_impl_ExistentialTypeFor_close<'a> = impl ::core::future::Future<Output = Result<(), Errno>> + 'a;
        }
    };
    let actual_output = crate::real_async_trait2(proc_macro2::TokenStream::new(), input);

    // TODO: Any better way to do this?
    let expected_output_trait = syn::parse2::<syn::Item>(expected_output).unwrap();
    let actual_output_trait = syn::parse2::<syn::Item>(actual_output).unwrap();

    assert_eq!(expected_output_trait, actual_output_trait);
}

// TODO: Expand tests, and add integration tests.
