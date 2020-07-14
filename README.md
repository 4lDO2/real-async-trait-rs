# `#[real_async_trait]`
[![Build Status](https://travis-ci.org/4lDO2/real-async-trait-rs.svg?branch=master)](https://travis-ci.org/4lDO2/real-async-trait-rs)
[![Crates.io](https://img.shields.io/crates/v/real-async-trait.svg)](https://crates.io/crates/real-async-trait)
[![Documentation](https://docs.rs/real-async-trait/badge.svg)](https://docs.rs/real-async-trait/)

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

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
