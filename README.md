# `#[real_async_trait]`
This crate provides a proof-of-concept proc macro attribute that allows async
traits to be possible, without wrapping everything in a `Box` and erase the
types. This is made possible due to experimental `generic_associated_types`
feature, as well as the unstable `type_alias_impl_trait` feature.

## Disclaimer
The `generic_associated_types` feature is not unstable, but an "incomplete"
experimental feature; rustc even gives you a warning when using it. __Don't use
this in production code__. At the moment it's probably a better idea to use a
type-erased async trait attribute proc macro, such as Dtolnay's `async-trait`
crate, or a similar one. That said, it'd be awesome both for me and for the
rust compiler if you could test this, find something that's missing in the
compiler or here, and file an issue.
