#![feature(generic_associated_types, min_type_alias_impl_trait)]
extern crate real_async_trait;

use std::collections::BTreeMap;
use std::str;

use real_async_trait::real_async_trait;

pub type Errno = usize;
pub const ENOENT: usize = 1;
pub const EBADF: usize = 2;

#[real_async_trait]
pub trait AsyncScheme {
    #[send]
    async fn open<'a>(&'a mut self, path: &'a [u8], flags: usize) -> Result<usize, Errno>;
    async fn close<'a>(&'a mut self, fd: usize) -> Result<(), Errno>;
    async fn read<'a>(&'a mut self, fd: usize, num: &'a mut u64) -> Result<(), Errno>;
    async fn write<'a>(&'a mut self, fd: usize, num: &'a u64) -> Result<(), Errno>;
}

struct NumberScheme {
    handles: BTreeMap<usize, u64>,
}

#[real_async_trait]
impl AsyncScheme for NumberScheme {
    #[send]
    async fn open<'a>(&'a mut self, path: &'a [u8], _flags: usize) -> Result<usize, Errno> {
        let path_str = str::from_utf8(path).or(Err(ENOENT))?;
        let num = path_str.parse::<usize>().or(Err(ENOENT))?;
        self.handles.insert(num, 0);
        Ok(num)
    }
    async fn close<'a>(&'a mut self, fd: usize) -> Result<(), Errno> {
        if self.handles.remove(&fd).is_none() {
            return Err(EBADF);
        }
        Ok(())
    }
    async fn read<'a>(&'a mut self, fd: usize, num: &'a mut u64) -> Result<(), Errno> {
        let handle = self.handles.get(&fd).ok_or(ENOENT)?;
        *num = *handle;
        Ok(())
    }
    async fn write<'a>(&'a mut self, fd: usize, num: &'a u64) -> Result<(), Errno> {
        let handle = self.handles.get_mut(&fd).ok_or(ENOENT)?;
        *handle = *num;
        Ok(())
    }
}

#[async_std::main]
async fn main() -> Result<(), Errno> {
    let mut numberscheme = NumberScheme {
        handles: BTreeMap::new(),
    };

    let mut number_buf = 0u64;
    let input_buf = 420u64;

    println!("Opening valuable number container `1337`");
    let fd = numberscheme.open(b"1337", 1).await?;
    println!("Opened number container `1337`");

    println!(
        "Initiating read from number container `1337` (fd {}), into buffer at {:p}",
        fd, &number_buf as *const _
    );
    numberscheme.read(fd, &mut number_buf).await?;
    println!(
        "Completed read from number container `1337` (fd {}), new value: {}",
        fd, number_buf
    );

    println!(
        "Initiating write into number container `1337` (fd {}), from buffer at {:p}",
        fd, &input_buf as *const _
    );
    numberscheme.write(fd, &input_buf).await?;
    println!(
        "Completed write into number container `1337` (fd {}), new value: {}",
        fd, input_buf
    );

    println!(
        "Initiating second read from number container `1337` (fd {}), into buffer at {:p}",
        fd, &number_buf as *const _
    );
    numberscheme.read(fd, &mut number_buf).await?;
    println!(
        "Completed second read from number container `1337` (fd {}), new value: {}",
        fd, number_buf
    );

    println!("Closing number container `1337` (fd {})", fd);
    numberscheme.close(fd).await?;
    println!("Number container `1337` (fd {}) closed", fd);

    Ok(())
}
