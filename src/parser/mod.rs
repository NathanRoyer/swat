//! Parser for webassembly modules
//!
//! Vector (SIMD) instructions are currently not supported.

#![allow(unreachable_code)]

use core::str::from_utf8;
use alloc::{vec::Vec, string::String};
use crate::error;

pub mod types;
pub mod module;
pub mod instructions;

use types::{NumberType, ReferenceType, ValueType, FunctionType, Limits, TableType, GlobalType};
use instructions::Expression;
use module::Module;

use indices::*;
pub mod indices {
    pub type FunctionTypeIndex = usize;
    pub type FunctionIndex = usize;
    pub type TableIndex = usize;
    pub type MemoryIndex = usize;
    pub type GlobalIndex = usize;
    pub type ElemIndex = usize;
    pub type DataIndex = usize;
    pub type LocalIndex = usize;
    pub type LabelIndex = usize;
}

/// The various errors that can be found in a webassembly module
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorType {
    InvalidMagic,
    UnsupportedVersion,
    MissingBytes,
    Leb128Overflow,
    InvalidName,
    InvalidNumberType,
    InvalidReferenceType,
    InvalidVectorType,
    InvalidValueType,
    InvalidFunctionType,
    InvalidLimits,
    InvalidMutability,
    InvalidElseInBlock,
    UnknownInstruction,
    UnknownSectionId,
    InvalidSectionId,
    InvalidImportType,
    InvalidExportType,
    UnknownElementKind,
    InvalidElementKind,
    UnknownElementEncoding,
    /// the limit is currently at 8192
    ManyLocals,
    InvalidDataType,
}

/// Wrapper for [`ErrorType`]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ParsingError {
    error_type: ErrorType,
    file: &'static str,
    line: u32,
}

type Error = ParsingError;
type Result<T> = core::result::Result<T, Error>;

/// Reads a webassembly module from a byte slice
pub fn parse(slice: &[u8]) -> Result<Module> {
    Parser::new(slice).read_module()
}

struct Parser<'a> {
    slice: &'a [u8],
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(slice: &'a [u8]) -> Self {
        Self {
            slice,
            index: 0,
        }
    }

    fn byte(&mut self) -> Result<u8> {
        if let Some(byte) = self.slice.get(self.index) {
            self.index += 1;
            Ok(*byte)
        } else {
            error!(ErrorType::MissingBytes)
        }
    }

    fn read_unsigned(&mut self, size: u32) -> Result<u64> {
        let mut result = 0;
        let mut shift = 0;
        let mut keep_going = true;

        while keep_going {
            let byte = self.byte()?;
            let payload = byte & 0x7f;

            let msb = shift + (8 - payload.leading_zeros());
            if msb > size {
                return error!(ErrorType::Leb128Overflow);
            }

            result |= (payload as u64) << shift;
            shift += 7;
            keep_going = byte > 0x7f;
        }

        Ok(result)
    }

    /* unused
    fn read_u64(&mut self) -> Result<u64> {
        match u64::try_from(self.read_unsigned(64)?) {
            Ok(num) => Ok(num),
            Err(_) => error!(ErrorType::Leb128Overflow),
        }
    }
    */

    fn read_u32(&mut self) -> Result<u32> {
        match u32::try_from(self.read_unsigned(32)?) {
            Ok(num) => Ok(num),
            Err(_) => error!(ErrorType::Leb128Overflow),
        }
    }

    fn read_signed(&mut self) -> Result<i64> {
        let mut keep_going = true;
        let mut result = 0;
        let mut shift = 0;
        let mut byte = 0;

        while keep_going {
            byte = self.byte()?;
            result |= ((byte & 0x7f) as u64) << shift;
            shift += 7;
            keep_going = byte > 0x7f;
        }

        if (byte & 0x40) != 0 {
            result |= u64::MAX.wrapping_shl(shift);
        }

        let result = i64::from_ne_bytes(result.to_ne_bytes());

        // println!("read_signed: {:?}", result);

        Ok(result)
    }

    fn read_s64(&mut self) -> Result<i64> {
        self.read_signed()
    }

    fn read_s32(&mut self) -> Result<i32> {
        match i32::try_from(self.read_signed()?) {
            Ok(num) => Ok(num),
            Err(_) => error!(ErrorType::Leb128Overflow),
        }
    }

    fn read_f32(&mut self) -> Result<f32> {
        let b0 = self.byte()?;
        let b1 = self.byte()?;
        let b2 = self.byte()?;
        let b3 = self.byte()?;
        Ok(f32::from_le_bytes([b0, b1, b2, b3]))
    }

    fn read_f64(&mut self) -> Result<f64> {
        let b0 = self.byte()?;
        let b1 = self.byte()?;
        let b2 = self.byte()?;
        let b3 = self.byte()?;
        let b4 = self.byte()?;
        let b5 = self.byte()?;
        let b6 = self.byte()?;
        let b7 = self.byte()?;
        Ok(f64::from_le_bytes([b0, b1, b2, b3, b4, b5, b6, b7]))
    }

    fn read_index(&mut self) -> Result<usize> {
        Ok(self.read_u32()? as usize)
    }

    fn read_length(&mut self) -> Result<usize> {
        Ok(self.read_u32()? as usize)
    }

    fn read_name(&mut self) -> Result<String> {
        let length = self.read_length()?;

        let end = self.index + length;

        let slice = match self.slice.get(self.index..end) {
            Some(slice) => Ok(slice),
            None => error!(ErrorType::MissingBytes)
        }?;

        let name = match from_utf8(slice) {
            Ok(name) => Ok(name),
            Err(_) => error!(ErrorType::InvalidName)
        }?;

        self.index = end;

        Ok(name.into())
    }

    fn collect<T, F: Fn(&mut Self) -> Result<T>>(
        &mut self,
        vec: &mut Vec<T>,
        callback: F,
    ) -> Result<()> {
        let length = self.read_length()?;
        vec.reserve(length);

        for _i in 0..length {
            vec.push(callback(self)?);
        }

        Ok(())
    }
}
