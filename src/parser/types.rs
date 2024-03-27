use super::*;

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum NumberType {
    i32,
    i64,
    f32,
    f64,
}

/* unused
impl<'a> Parser<'a> {
    fn read_number_type(&mut self) -> Result<NumberType> {
        match self.byte()? {
            0x7f => Ok(NumberType::i32),
            0x7e => Ok(NumberType::i64),
            0x7d => Ok(NumberType::f32),
            0x7c => Ok(NumberType::f64),
            _ => error!(ErrorType::InvalidNumberType),
        }
    }
}
*/

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ReferenceType {
    FuncRef,
    ExternRef,
}

impl<'a> Parser<'a> {
    pub(super) fn read_ref_type(&mut self) -> Result<ReferenceType> {
        match self.byte()? {
            0x70 => Ok(ReferenceType::FuncRef),
            0x6f => Ok(ReferenceType::ExternRef),
            _ => error!(ErrorType::InvalidReferenceType),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VectorType {
    v128,
}

/* unused
impl<'a> Parser<'a> {
    fn read_vector_type(&mut self) -> Result<VectorType> {
        match self.byte()? {
            0x7b => Ok(VectorType::v128),
            _ => error!(ErrorType::InvalidVectorType),
        }
    }
}
*/

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ValueType {
    Number(NumberType),
    Reference(ReferenceType),
    Vector128,
}

impl<'a> Parser<'a> {
    pub(super) fn read_value_type(&mut self) -> Result<ValueType> {
        match self.byte()? {
            0x7f => Ok(ValueType::Number(NumberType::i32)),
            0x7e => Ok(ValueType::Number(NumberType::i64)),
            0x7d => Ok(ValueType::Number(NumberType::f32)),
            0x7c => Ok(ValueType::Number(NumberType::f64)),
            0x7b => Ok(ValueType::Vector128),
            0x70 => Ok(ValueType::Reference(ReferenceType::FuncRef)),
            0x6f => Ok(ValueType::Reference(ReferenceType::ExternRef)),
            _ => error!(ErrorType::InvalidValueType),
        }
    }

    pub(super) fn read_result_type(&mut self) -> Result<Vec<ValueType>> {
        let mut out = Vec::new();
        self.collect(&mut out, Self::read_value_type)?;
        Ok(out)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<ValueType>,
    pub returned: Vec<ValueType>,
}

impl<'a> Parser<'a> {
    pub(super) fn read_function_type(&mut self) -> Result<FunctionType> {
        if self.byte()? == 0x60 {

            let parameters = self.read_result_type()?;
            let returned = self.read_result_type()?;

            let func = FunctionType {
                parameters,
                returned,
            };

            Ok(func)

        } else {
            error!(ErrorType::InvalidFunctionType)
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

impl<'a> Parser<'a> {
    // NB: Memory types are encoded with their limits.
    pub(super) fn read_limits(&mut self) -> Result<Limits> {
        let (min, max) = match self.byte()? {
            0x00 => Ok((self.read_u32()?, None)),
            0x01 => {
                let min = self.read_u32()?;
                let max = self.read_u32()?;
                Ok((min, Some(max)))
            },
            _ => error!(ErrorType::InvalidLimits),
        }?;

        let limits = Limits {
            min, max,
        };

        Ok(limits)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TableType {
    pub element_type: ReferenceType,
    pub limits: Limits,
}

impl<'a> Parser<'a> {
    pub(super) fn read_table_type(&mut self) -> Result<TableType> {
        let element_type = self.read_ref_type()?;
        let limits = self.read_limits()?;

        let table_type = TableType {
            element_type,
            limits,
        };

        Ok(table_type)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GlobalType {
    pub value_type: ValueType,
    pub mutable: bool,
}

impl<'a> Parser<'a> {
    pub(super) fn read_global_type(&mut self) -> Result<GlobalType> {
        let value_type = self.read_value_type()?;
        let mutable = match self.byte()? {
            0x00 => Ok(false),
            0x01 => Ok(true),
            _ => error!(ErrorType::InvalidMutability),
        }?;

        let global_type = GlobalType {
            value_type,
            mutable,
        };

        Ok(global_type)
    }
}
