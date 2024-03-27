#![doc = include_str!("../README.md")]
#![no_std]

extern crate alloc;

pub mod parser;

#[cfg(debug_assertions)]
#[macro_export]
macro_rules! error {
    ($error_type:expr) => {{ panic!("{:?}", $error_type) }}
}

#[cfg(not(debug_assertions))]
#[macro_export]
macro_rules! error {
    ($error_type:expr) => {
        Err(Error {
            error_type: $error_type,
            file: file!(),
            line: line!(),
        })
    }
}
