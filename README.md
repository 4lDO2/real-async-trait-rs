# `#[real_async_trait]`
[![Build Status](https://travis-ci.org/4lDO2/real-async-trait-rs.svg?branch=master)](https://travis-ci.org/4lDO2/real-async-trait-rs)
[![Crates.io](https://img.shields.io/crates/v/real-async-trait.svg)](https://crates.io/crates/real-async-trait)
[![Documentation](https://docs.rs/real-async-trait/badge.svg)](https://docs.rs/real-async-trait/)

This nightly-only crate provides a proof-of-concept proc macro attribute that
allows async functions within traits, without the possible runtime overhead of
wrapping everything in a `Box` and erasing the types. This is made possible
thanks to the unstable `generic_associated_types` and `type_alias_impl_trait`
nightly features.

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
