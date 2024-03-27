use super::*;

pub type Signed = bool;

#[derive(Clone, Debug, PartialEq)]
pub enum BlockType {
    EmptyType,
    Value(ValueType),
    FunctionTypeIndex(FunctionTypeIndex),
}

impl<'a> Parser<'a> {
    fn read_block_type(&mut self) -> Result<BlockType> {
        use BlockType::*;

        if self.byte()? == 0x40 {
            return Ok(EmptyType);
        }

        // go back
        self.index -= 1;

        match self.read_value_type() {
            Ok(value_type) => Ok(Value(value_type)),
            Err(Error {
                error_type: ErrorType::InvalidValueType,
                ..
            }) => self.read_s32().map(|i| FunctionTypeIndex(i as _)),
            Err(e) => Err(e),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub block_type: BlockType,
    pub main_blk: Expression,
    pub else_blk: Option<Expression>,
}

impl<'a> Parser<'a> {
    fn read_block(&mut self, allow_else: bool) -> Result<Block> {
        let block_type = self.read_block_type()?;
        let mut main_blk = Vec::new();
        let mut else_blk = None;

        loop {
            match self.read_instruction()? {
                OpCode::BlockEnd => break,
                OpCode::Else => match allow_else {
                    true => else_blk = Some(Vec::new()),
                    false => return error!(ErrorType::InvalidElseInBlock),
                },
                OpCode::Instruction(instruction) => {
                    let dst = else_blk.as_mut().unwrap_or(&mut main_blk);
                    dst.push(instruction);
                },
            }
        }

        let block = Block {
            block_type,
            main_blk,
            else_blk,
        };

        Ok(block)
    }
}

pub type Expression = Vec<Instruction>;

impl<'a> Parser<'a> {
    pub(super) fn read_expression(&mut self) -> Result<Expression> {
        let mut expression = Vec::new();

        loop {
            match self.read_instruction()? {
                OpCode::BlockEnd => break Ok(expression),
                OpCode::Else => break error!(ErrorType::InvalidElseInBlock),
                OpCode::Instruction(instruction) => expression.push(instruction),
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BranchTable {
    pub table: Vec<LabelIndex>,
    pub fallback: LabelIndex,
}

impl<'a> Parser<'a> {
    fn read_branch_table(&mut self) -> Result<BranchTable> {
        let mut table = Vec::new();
        self.collect(&mut table, Self::read_index)?;

        let fallback = self.read_index()?;
        let branch_table = BranchTable {
            table,
            fallback,
        };

        Ok(branch_table)
    }
}

#[derive(Clone, Debug, PartialEq)]
enum OpCode {
    BlockEnd,
    Else,
    Instruction(Instruction),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    // CONTROL INSTRUCTIONS
    Unreachable,
    Nop,
    Block(Block),
    Loop(Block),
    If(Block),
    Branch(LabelIndex),
    BranchIf(LabelIndex),
    BranchTable(BranchTable),
    Return,
    Call(FunctionIndex),
    CallIndirect(FunctionTypeIndex, TableIndex),

    // REFERENCE INSTRUCTIONS
    RefNull(ReferenceType),
    IsRefNull,
    FuncRef(FunctionIndex),

    // PARAMETRIC INSTRUCTIONS
    Drop,
    Select,
    SelectVec(Vec<ValueType>),

    // VARIABLE INSTRUCTIONS
    LocalGet(LocalIndex),
    LocalSet(LocalIndex),
    LocalTee(LocalIndex),
    GlobalSet(GlobalIndex),
    GlobalGet(GlobalIndex),

    // TABLE INSTRUCTIONS
    TableGet(TableIndex),
    TableSet(TableIndex),
    TableInit(TableIndex, ElemIndex),
    ElemDrop(ElemIndex),
    TableCopy(TableIndex, TableIndex),
    TableGrow(TableIndex),
    TableSize(TableIndex),
    TableFill(TableIndex),

    // MEMORY INSTRUCTIONS
    MemoryLoad(LoadInstruction),
    MemoryStore(StoreInstruction),
    MemorySize(MemoryIndex),
    MemoryGrow(MemoryIndex),
    MemoryInit(MemoryIndex, DataIndex),
    DataDrop(DataIndex),
    MemoryCopy(MemoryIndex, MemoryIndex),
    MemoryFill(MemoryIndex),

    // NUMERIC INSTRUCTIONS
    ConstI32(i32),
    ConstF32(f32),
    ConstI64(i64),
    ConstF64(f64),

    IntEqualToZero(OpInt),
    Equal(OpReg),
    NotEqual(OpReg),
    LowerThan(OpAll),
    GreaterThan(OpAll),
    LowerThanOrEqualTo(OpAll),
    GreaterThanOrEqualTo(OpAll),
    Add(OpReg),
    Sub(OpReg),
    Mul(OpReg),
    Div(OpAll),

    IntCountLeadingZeroBits(OpInt),
    IntCountTrailingZeroBits(OpInt),
    IntCountSetBits(OpInt),
    IntRem(OpInt, Signed),
    IntAnd(OpInt),
    IntOr(OpInt),
    IntXor(OpInt),
    IntShiftLeft(OpInt),
    IntShiftRight(OpInt, Signed),
    IntRotateLeft(OpInt),
    IntRotateRight(OpInt),

    FltAbsolute(OpFlt),
    FltNegate(OpFlt),
    FltCeil(OpFlt),
    FltFloor(OpFlt),
    FltTruncate(OpFlt),
    FltNearest(OpFlt),
    FltSquareRoot(OpFlt),
    FltMin(OpFlt),
    FltMax(OpFlt),
    FltCopySign(OpFlt),

    Wrap,
    IntTruncate(OpInt, Signed, OpFlt),
    SatTruncate(OpInt, Signed, OpFlt),
    IntExtend(OpInt, Signed, Bits),
    FltConvert(OpFlt, OpInt, Signed),
    FltDemote,
    FltPromote,
    IntReinterpret(OpFlt),
    FltReinterpret(OpInt),
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpAll {
    u32,
    u64,
    i32,
    i64,
    f32,
    f64,
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpInt {
    i32,
    i64,
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpFlt {
    f32,
    f64,
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpReg {
    i32,
    i64,
    f32,
    f64,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Bits {
    Eight,
    Sixteen,
    ThirtyTwo,
    SixtyFour,
}

impl<'a> Parser<'a> {
    fn read_instruction(&mut self) -> Result<OpCode> {
        use Instruction::*;

        let ins = match self.byte()? {
            // CONTROL INSTRUCTIONS
            0x00 => Unreachable,
            0x01 => Nop,
            0x02 => Block(self.read_block(false)?),
            0x03 => Loop(self.read_block(false)?),
            0x04 => If(self.read_block(true)?),
            0x05 => return Ok(OpCode::Else),
            0x0b => return Ok(OpCode::BlockEnd),
            0x0c => Branch(self.read_index()?),
            0x0d => BranchIf(self.read_index()?),
            0x0e => BranchTable(self.read_branch_table()?),
            0x0f => Return,
            0x10 => Call(self.read_index()?),
            0x11 => {
                let type_index = self.read_index()?;
                let table_index = self.read_index()?;
                CallIndirect(type_index, table_index)
            },

            // REFERENCE INSTRUCTIONS
            0xd0 => RefNull(self.read_ref_type()?),
            0xd1 => IsRefNull,
            0xd2 => FuncRef(self.read_index()?),

            // PARAMETRIC INSTRUCTIONS
            0x1a => Drop,
            0x1b => Select,
            0x1c => SelectVec(self.read_result_type()?),

            // VARIABLE INSTRUCTIONS
            0x20 => LocalGet(self.read_index()?),
            0x21 => LocalSet(self.read_index()?),
            0x22 => LocalTee(self.read_index()?),
            0x23 => GlobalSet(self.read_index()?),
            0x24 => GlobalGet(self.read_index()?),

            // TABLE INSTRUCTIONS
            0x25 => TableGet(self.read_index()?),
            0x26 => TableSet(self.read_index()?),
            0xfc => self.read_special_instruction()?,

            // MEMORY INSTRUCTIONS
            i @ 0x28..=0x35 => self.read_load_instruction(i)?,
            i @ 0x36..=0x3e => self.read_store_instruction(i)?,
            0x3f => MemorySize(self.read_index()?),
            0x40 => MemoryGrow(self.read_index()?),
            // special memory instructions:
            // 0xfc => self.read_special_instruction()?,

            // NUMERIC INSTRUCTIONS
            0x41 => ConstI32(self.read_s32()?),
            0x42 => ConstI64(self.read_s64()?),
            0x43 => ConstF32(self.read_f32()?),
            0x44 => ConstF64(self.read_f64()?),

            0x45 => IntEqualToZero(OpInt::i32),
            0x46 => Equal(OpReg::i32),
            0x47 => NotEqual(OpReg::i32),
            0x48 => LowerThan(OpAll::i32),
            0x49 => LowerThan(OpAll::u32),
            0x4a => GreaterThan(OpAll::i32),
            0x4b => GreaterThan(OpAll::u32),
            0x4c => LowerThanOrEqualTo(OpAll::i32),
            0x4d => LowerThanOrEqualTo(OpAll::u32),
            0x4e => GreaterThanOrEqualTo(OpAll::i32),
            0x4f => GreaterThanOrEqualTo(OpAll::u32),

            0x50 => IntEqualToZero(OpInt::i64),
            0x51 => Equal(OpReg::i64),
            0x52 => NotEqual(OpReg::i64),
            0x53 => LowerThan(OpAll::i64),
            0x54 => LowerThan(OpAll::u64),
            0x55 => GreaterThan(OpAll::i64),
            0x56 => GreaterThan(OpAll::u64),
            0x57 => LowerThanOrEqualTo(OpAll::i64),
            0x58 => LowerThanOrEqualTo(OpAll::u64),
            0x59 => GreaterThanOrEqualTo(OpAll::i64),
            0x5a => GreaterThanOrEqualTo(OpAll::u64),

            0x5b => Equal(OpReg::f32),
            0x5c => NotEqual(OpReg::f32),
            0x5d => LowerThan(OpAll::f32),
            0x5e => GreaterThan(OpAll::f32),
            0x5f => LowerThanOrEqualTo(OpAll::f32),
            0x60 => GreaterThanOrEqualTo(OpAll::f32),

            0x61 => Equal(OpReg::f64),
            0x62 => NotEqual(OpReg::f64),
            0x63 => LowerThan(OpAll::f64),
            0x64 => GreaterThan(OpAll::f64),
            0x65 => LowerThanOrEqualTo(OpAll::f64),
            0x66 => GreaterThanOrEqualTo(OpAll::f64),

            0x67 => IntCountLeadingZeroBits(OpInt::i32),
            0x68 => IntCountTrailingZeroBits(OpInt::i32),
            0x69 => IntCountSetBits(OpInt::i32),
            0x6a => Add(OpReg::i32),
            0x6b => Sub(OpReg::i32),
            0x6c => Mul(OpReg::i32),
            0x6d => Div(OpAll::i32),
            0x6e => Div(OpAll::u32),
            0x6f => IntRem(OpInt::i32, true),
            0x70 => IntRem(OpInt::i32, false),
            0x71 => IntAnd(OpInt::i32),
            0x72 => IntOr(OpInt::i32),
            0x73 => IntXor(OpInt::i32),
            0x74 => IntShiftLeft(OpInt::i32),
            0x75 => IntShiftRight(OpInt::i32, true),
            0x76 => IntShiftRight(OpInt::i32, false),
            0x77 => IntRotateLeft(OpInt::i32),
            0x78 => IntRotateRight(OpInt::i32),

            0x79 => IntCountLeadingZeroBits(OpInt::i64),
            0x7a => IntCountTrailingZeroBits(OpInt::i64),
            0x7b => IntCountSetBits(OpInt::i64),
            0x7c => Add(OpReg::i64),
            0x7d => Sub(OpReg::i64),
            0x7e => Mul(OpReg::i64),
            0x7f => Div(OpAll::i64),
            0x80 => Div(OpAll::u64),
            0x81 => IntRem(OpInt::i64, true),
            0x82 => IntRem(OpInt::i64, false),
            0x83 => IntAnd(OpInt::i64),
            0x84 => IntOr(OpInt::i64),
            0x85 => IntXor(OpInt::i64),
            0x86 => IntShiftLeft(OpInt::i64),
            0x87 => IntShiftRight(OpInt::i64, true),
            0x88 => IntShiftRight(OpInt::i64, false),
            0x89 => IntRotateLeft(OpInt::i64),
            0x8a => IntRotateRight(OpInt::i64),

            0x8b => FltAbsolute(OpFlt::f32),
            0x8c => FltNegate(OpFlt::f32),
            0x8d => FltCeil(OpFlt::f32),
            0x8e => FltFloor(OpFlt::f32),
            0x8f => FltTruncate(OpFlt::f32),
            0x90 => FltNearest(OpFlt::f32),
            0x91 => FltSquareRoot(OpFlt::f32),
            0x92 => Add(OpReg::f32),
            0x93 => Sub(OpReg::f32),
            0x94 => Mul(OpReg::f32),
            0x95 => Div(OpAll::f32),
            0x96 => FltMin(OpFlt::f32),
            0x97 => FltMax(OpFlt::f32),
            0x98 => FltCopySign(OpFlt::f32),

            0x99 => FltAbsolute(OpFlt::f64),
            0x9a => FltNegate(OpFlt::f64),
            0x9b => FltCeil(OpFlt::f64),
            0x9c => FltFloor(OpFlt::f64),
            0x9d => FltTruncate(OpFlt::f64),
            0x9e => FltNearest(OpFlt::f64),
            0x9f => FltSquareRoot(OpFlt::f64),
            0xa0 => Add(OpReg::f64),
            0xa1 => Sub(OpReg::f64),
            0xa2 => Mul(OpReg::f64),
            0xa3 => Div(OpAll::f64),
            0xa4 => FltMin(OpFlt::f64),
            0xa5 => FltMax(OpFlt::f64),
            0xa6 => FltCopySign(OpFlt::f64),

            0xa7 => Wrap,
            0xa8 => IntTruncate(OpInt::i32, true, OpFlt::f32),
            0xa9 => IntTruncate(OpInt::i32, false, OpFlt::f32),
            0xaa => IntTruncate(OpInt::i32, true, OpFlt::f64),
            0xab => IntTruncate(OpInt::i32, false, OpFlt::f64),
            0xac => IntExtend(OpInt::i64, true, Bits::ThirtyTwo),
            0xad => IntExtend(OpInt::i64, false, Bits::ThirtyTwo),
            0xae => IntTruncate(OpInt::i64, true, OpFlt::f32),
            0xaf => IntTruncate(OpInt::i64, false, OpFlt::f32),
            0xb0 => IntTruncate(OpInt::i64, true, OpFlt::f64),
            0xb1 => IntTruncate(OpInt::i64, false, OpFlt::f64),
            0xb2 => FltConvert(OpFlt::f32, OpInt::i32, true),
            0xb3 => FltConvert(OpFlt::f32, OpInt::i32, false),
            0xb4 => FltConvert(OpFlt::f32, OpInt::i64, true),
            0xb5 => FltConvert(OpFlt::f32, OpInt::i64, false),
            0xb6 => FltDemote,
            0xb7 => FltConvert(OpFlt::f64, OpInt::i32, true),
            0xb8 => FltConvert(OpFlt::f64, OpInt::i32, false),
            0xb9 => FltConvert(OpFlt::f64, OpInt::i64, true),
            0xba => FltConvert(OpFlt::f64, OpInt::i64, false),
            0xbb => FltPromote,
            0xbc => IntReinterpret(OpFlt::f32),
            0xbd => IntReinterpret(OpFlt::f64),
            0xbe => FltReinterpret(OpInt::i32),
            0xbf => FltReinterpret(OpInt::i64),

            0xc0 => IntExtend(OpInt::i32, true, Bits::Eight),
            0xc1 => IntExtend(OpInt::i32, true, Bits::Sixteen),
            0xc2 => IntExtend(OpInt::i64, true, Bits::Eight),
            0xc3 => IntExtend(OpInt::i64, true, Bits::Sixteen),
            0xc4 => IntExtend(OpInt::i64, true, Bits::ThirtyTwo),

            // saturating truncate:
            // 0xfc => self.read_special_instruction()?,

            _ => return error!(ErrorType::UnknownInstruction),
        };

        Ok(OpCode::Instruction(ins))
    }

    fn read_special_instruction(&mut self) -> Result<Instruction> {
        use Instruction::*;

        let special_opcode = self.read_u32()?;

        let arg = match special_opcode {
            8..=17 => self.read_index()?,
            _ => 0,
        };

        let ins = match special_opcode {
            // NUMERIC INSTRUCTIONS
            0 => SatTruncate(OpInt::i32, true, OpFlt::f32),
            1 => SatTruncate(OpInt::i32, false, OpFlt::f32),
            2 => SatTruncate(OpInt::i32, true, OpFlt::f64),
            3 => SatTruncate(OpInt::i32, false, OpFlt::f64),
            4 => SatTruncate(OpInt::i64, true, OpFlt::f32),
            5 => SatTruncate(OpInt::i64, false, OpFlt::f32),
            6 => SatTruncate(OpInt::i64, true, OpFlt::f64),
            7 => SatTruncate(OpInt::i64, false, OpFlt::f64),

            // MEMORY INSTRUCTIONS
            8 => MemoryInit(self.read_index()?, arg),
            9 => DataDrop(arg),
            10 => MemoryCopy(arg, self.read_index()?),
            11 => MemoryFill(arg),

            // TABLE INSTRUCTIONS
            12 => TableInit(self.read_index()?, arg),
            13 => ElemDrop(arg),
            14 => TableCopy(arg, self.read_index()?),
            15 => TableGrow(arg),
            16 => TableSize(arg),
            17 => TableFill(arg),

            _ => return error!(ErrorType::UnknownInstruction),
        };

        Ok(ins)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct LoadInstruction {
    pub alignment: u32,
    pub offset: u32,
    pub num_type: NumberType,
    pub signed: Signed,
    pub bits: Bits,
}

impl<'a> Parser<'a> {
    fn read_load_instruction(&mut self, opcode: u8) -> Result<Instruction> {
        let alignment = self.read_u32()?;
        let offset = self.read_u32()?;
        let n_a = false;

        let (num_type, signed, bits) = match opcode {
            // canonical
            0x28 => (NumberType::i32, n_a, Bits::ThirtyTwo),
            0x29 => (NumberType::i64, n_a, Bits::SixtyFour),
            0x2a => (NumberType::f32, n_a, Bits::ThirtyTwo),
            0x2b => (NumberType::f64, n_a, Bits::SixtyFour),

            // partial
            0x2c => (NumberType::i32, true, Bits::Eight),
            0x2d => (NumberType::i32, false, Bits::Eight),
            0x2e => (NumberType::i32, true, Bits::Sixteen),
            0x2f => (NumberType::i32, false, Bits::Sixteen),
            0x30 => (NumberType::i64, true, Bits::Eight),
            0x31 => (NumberType::i64, false, Bits::Eight),
            0x32 => (NumberType::i64, true, Bits::Sixteen),
            0x33 => (NumberType::i64, false, Bits::Sixteen),
            0x34 => (NumberType::i64, true, Bits::ThirtyTwo),
            0x35 => (NumberType::i64, false, Bits::ThirtyTwo),

            _ => unreachable!()
        };

        let load = LoadInstruction {
            alignment,
            offset,
            num_type,
            signed,
            bits,
        };

        Ok(Instruction::MemoryLoad(load))
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct StoreInstruction {
    pub alignment: u32,
    pub offset: u32,
    pub num_type: NumberType,
    pub bits: Bits,
}

impl<'a> Parser<'a> {
    fn read_store_instruction(&mut self, opcode: u8) -> Result<Instruction> {
        let alignment = self.read_u32()?;
        let offset = self.read_u32()?;

        let (num_type, bits) = match opcode {
            // canonical
            0x36 => (NumberType::i32, Bits::ThirtyTwo),
            0x37 => (NumberType::i64, Bits::SixtyFour),
            0x38 => (NumberType::f32, Bits::ThirtyTwo),
            0x39 => (NumberType::f64, Bits::SixtyFour),

            // partial
            0x3a => (NumberType::i32, Bits::Eight),
            0x3b => (NumberType::i32, Bits::Sixteen),
            0x3c => (NumberType::i64, Bits::Eight),
            0x3d => (NumberType::i64, Bits::Sixteen),
            0x3e => (NumberType::i64, Bits::ThirtyTwo),

            _ => unreachable!()
        };

        let store = StoreInstruction {
            alignment,
            offset,
            num_type,
            bits,
        };

        Ok(Instruction::MemoryStore(store))
    }
}
