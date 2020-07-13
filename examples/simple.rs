#![feature(generic_associated_types, type_alias_impl_trait)]
extern crate real_async_trait;

use real_async_trait::real_async_trait;
use std::collections::BTreeSet;

pub type Errno = usize;
pub const ENOENT: usize = 1;
pub const EBADF: usize = 2;

#[real_async_trait]
pub trait AsyncScheme {
    async fn open<'a, 'b>(&'a mut self, path: &'b [u8], flags: usize) -> Result<usize, Errno>;
    async fn close<'a>(&'a mut self, fd: usize) -> Result<(), Errno>;
    async fn read<'a, 'b>(&'a mut self, num: &'b mut u64) -> Result<(), Errno>;
}

struct NumberScheme {
    handles: BTreeSet<u8>,
}

#[real_async_trait]
impl AsyncScheme for NumberScheme {
    async fn open(&mut self, path: &[u8], _flags: usize) -> Result<usize, Errno> {
        todo!()
    }
    async fn close(&mut self, fd: usize) -> Result<(), Errno> {
        todo!()
    }
    async fn read(&mut self, num: &mut u64) -> Result<(), Errno> {
        todo!()
    }
}

fn main() {}
