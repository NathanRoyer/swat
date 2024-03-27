use super::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SectionId {
    Custom,
    Type,
    Import,
    Function,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    DataCount,
    Code,
    Data,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportItemType {
    Function(FunctionTypeIndex),
    Table(TableType),
    Memory(Limits),
    Global(GlobalType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    pub module_name: String,
    pub item_name: String,
    pub item_type: ImportItemType,
}

impl<'a> Parser<'a> {
    fn read_import(&mut self) -> Result<Import> {
        let module_name = self.read_name()?;
        let item_name = self.read_name()?;

        let item_type = match self.byte()? {
            0 => Ok(ImportItemType::Function(self.read_index()?)),
            1 => Ok(ImportItemType::Table(self.read_table_type()?)),
            2 => Ok(ImportItemType::Memory(self.read_limits()?)),
            3 => Ok(ImportItemType::Global(self.read_global_type()?)),
            _ => error!(ErrorType::InvalidImportType),
        }?;

        Ok(Import {
            module_name,
            item_name,
            item_type,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExportItemType {
    Function(FunctionIndex),
    Table(TableIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Export {
    pub item_name: String,
    pub item_type: ExportItemType,
}

impl<'a> Parser<'a> {
    fn read_export(&mut self) -> Result<Export> {
        let item_name = self.read_name()?;

        let item_type = match self.byte()? {
            0 => Ok(ExportItemType::Function(self.read_index()?)),
            1 => Ok(ExportItemType::Table(self.read_index()?)),
            2 => Ok(ExportItemType::Memory(self.read_index()?)),
            3 => Ok(ExportItemType::Global(self.read_index()?)),
            _ => error!(ErrorType::InvalidExportType),
        }?;

        Ok(Export {
            item_name,
            item_type,
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ElementKind {
    FuncRef,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ElementInit {
    FunctionIndices(Vec<FunctionIndex>),
    Expressions(Vec<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Element {
    Active {
        ref_type: ReferenceType,
        table_index: TableIndex,
        offset_expr: Expression,
        init: ElementInit,
    },
    Passive {
        ref_type: ReferenceType,
        init: ElementInit,
    },
    Declarative {
        ref_type: ReferenceType,
        init: ElementInit,
    }
}

impl<'a> Parser<'a> {
    fn read_element_kind(&mut self) -> Result<ElementKind> {
        match self.byte()? {
            0 => Ok(ElementKind::FuncRef),
            _ => error!(ErrorType::UnknownElementKind),
        }
    }

    fn read_element(&mut self) -> Result<Element> {
        match self.read_u32()? {
            0 => {
                let offset_expr = self.read_expression()?;
                let mut function_indices = Vec::new();
                self.collect(&mut function_indices, Self::read_index)?;

                Ok(Element::Active {
                    ref_type: ReferenceType::FuncRef,
                    table_index: 0,
                    offset_expr,
                    init: ElementInit::FunctionIndices(function_indices),
                })
            },
            1 => {
                let ElementKind::FuncRef = self.read_element_kind()?;
                let mut function_indices = Vec::new();
                self.collect(&mut function_indices, Self::read_index)?;

                Ok(Element::Passive {
                    ref_type: ReferenceType::FuncRef,
                    init: ElementInit::FunctionIndices(function_indices),
                })
            },
            2 => {
                let table_index = self.read_index()?;
                let offset_expr = self.read_expression()?;
                let ElementKind::FuncRef = self.read_element_kind()?;
                let mut function_indices = Vec::new();
                self.collect(&mut function_indices, Self::read_index)?;

                Ok(Element::Active {
                    ref_type: ReferenceType::FuncRef,
                    table_index,
                    offset_expr,
                    init: ElementInit::FunctionIndices(function_indices),
                })
            },
            3 => {
                let ElementKind::FuncRef = self.read_element_kind()?;
                let mut function_indices = Vec::new();
                self.collect(&mut function_indices, Self::read_index)?;

                Ok(Element::Declarative {
                    ref_type: ReferenceType::FuncRef,
                    init: ElementInit::FunctionIndices(function_indices),
                })
            },
            4 => {
                let offset_expr = self.read_expression()?;
                let mut init_exprs = Vec::new();
                self.collect(&mut init_exprs, Self::read_expression)?;

                Ok(Element::Active {
                    ref_type: ReferenceType::FuncRef,
                    table_index: 0,
                    offset_expr,
                    init: ElementInit::Expressions(init_exprs),
                })
            },
            5 => {
                let ref_type = self.read_ref_type()?;
                let mut init_exprs = Vec::new();
                self.collect(&mut init_exprs, Self::read_expression)?;

                Ok(Element::Passive {
                    ref_type,
                    init: ElementInit::Expressions(init_exprs),
                })
            },
            6 => {
                let table_index = self.read_index()?;
                let offset_expr = self.read_expression()?;
                let ref_type = self.read_ref_type()?;
                let mut init_exprs = Vec::new();
                self.collect(&mut init_exprs, Self::read_expression)?;

                Ok(Element::Active {
                    ref_type,
                    table_index,
                    offset_expr,
                    init: ElementInit::Expressions(init_exprs),
                })
            },
            7 => {
                let ref_type = self.read_ref_type()?;
                let mut init_exprs = Vec::new();
                self.collect(&mut init_exprs, Self::read_expression)?;

                Ok(Element::Declarative {
                    ref_type,
                    init: ElementInit::Expressions(init_exprs),
                })
            },
            _ => error!(ErrorType::UnknownElementEncoding),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionBody {
    pub locals: Vec<ValueType>,
    pub expression: Expression,
}

impl<'a> Parser<'a> {
    fn read_function_body(&mut self) -> Result<FunctionBody> {
        let _code_size = self.read_length()?;
        let length = self.read_length()?;
        // We don't know the full length ahead of time
        let mut locals = Vec::new();

        for _i in 0..length {
            let repeat = self.read_u32()?;
            let value_type = self.read_value_type()?;

            if repeat >= 8192 {
                return error!(ErrorType::ManyLocals);
            }

            for _j in 0..repeat {
                locals.push(value_type);
            }
        }

        let expression = self.read_expression()?;

        Ok(FunctionBody {
            locals,
            expression,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataSegment {
    Active {
        memory_index: usize,
        offset_expr: Expression,
        bytes: Vec<u8>,
    },
    Passive {
        bytes: Vec<u8>,
    },
}

impl<'a> Parser<'a> {
    fn read_data_segment(&mut self) -> Result<DataSegment> {
        match self.read_u32()? {
            0 => {
                let offset_expr = self.read_expression()?;
                let mut bytes = Vec::new();
                self.collect(&mut bytes, Self::byte)?;

                Ok(DataSegment::Active {
                    memory_index: 0,
                    offset_expr,
                    bytes,
                })
            },
            1 => {
                let mut bytes = Vec::new();
                self.collect(&mut bytes, Self::byte)?;

                Ok(DataSegment::Passive {
                    bytes,
                })
            },
            2 => {
                let memory_index = self.read_index()?;
                let offset_expr = self.read_expression()?;
                let mut bytes = Vec::new();
                self.collect(&mut bytes, Self::byte)?;

                Ok(DataSegment::Active {
                    memory_index,
                    offset_expr,
                    bytes,
                })
            },
            _ => error!(ErrorType::InvalidDataType),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Global {
    pub global_type: GlobalType,
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub function_types: Vec<FunctionType>,
    pub imports: Vec<Import>,
    pub function_signatures: Vec<FunctionTypeIndex>,
    pub tables: Vec<TableType>,
    pub memories: Vec<Limits>,
    pub globals: Vec<Global>,
    pub exports: Vec<Export>,
    pub start: Option<FunctionIndex>,
    pub elements: Vec<Element>,
    pub data_count: usize,
    pub function_bodies: Vec<FunctionBody>,
    pub data_segments: Vec<DataSegment>,
}

impl<'a> Parser<'a> {
    fn read_section(&mut self) -> Result<(SectionId, Self)> {
        let raw_section_id = self.byte()?;
        let size = self.read_u32()? as usize;

        let section_id = match raw_section_id {
            0 => Ok(SectionId::Custom),
            1 => Ok(SectionId::Type),
            2 => Ok(SectionId::Import),
            3 => Ok(SectionId::Function),
            4 => Ok(SectionId::Table),
            5 => Ok(SectionId::Memory),
            6 => Ok(SectionId::Global),
            7 => Ok(SectionId::Export),
            8 => Ok(SectionId::Start),
            9 => Ok(SectionId::Element),
            12 => Ok(SectionId::DataCount),
            10 => Ok(SectionId::Code),
            11 => Ok(SectionId::Data),
            _ => error!(ErrorType::UnknownSectionId),
        }?;

        let remaining_bytes = self.slice.get(self.index..)
            .expect("Invalid Parser State");

        let section_bytes = match remaining_bytes.get(..size) {
            Some(bytes) => Ok(bytes),
            None => error!(ErrorType::MissingBytes),
        }?;
        let section_parser = Self::new(section_bytes);

        self.index += size;

        Ok((section_id, section_parser))
    }

    fn read_custom_section(&mut self, _module: &mut Module) -> Result<()> {
        log::warn!("Ignoring Custom Section: {:?}", self.read_name()?);
        Ok(())
    }

    fn read_type_section(&mut self, module: &mut Module) -> Result<()> {
        self.collect(&mut module.function_types, Self::read_function_type)
    }

    fn read_import_section(&mut self, module: &mut Module) -> Result<()> {
        self.collect(&mut module.imports, Self::read_import)
    }

    fn read_function_section(&mut self, module: &mut Module) -> Result<()> {
        self.collect(&mut module.function_signatures, Self::read_index)
    }

    fn read_table_section(&mut self, module: &mut Module) -> Result<()> {
        self.collect(&mut module.tables, Self::read_table_type)
    }

    fn read_memory_section(&mut self, module: &mut Module) -> Result<()> {
        self.collect(&mut module.memories, Self::read_limits)
    }

    fn read_global_section(&mut self, module: &mut Module) -> Result<()> {
        let length = self.read_length()?;
        module.globals.reserve(length);

        for _i in 0..length {
            let global_type = self.read_global_type()?;
            let expression = self.read_expression()?;

            module.globals.push(Global {
                global_type,
                expression,
            });
        }

        Ok(())
    }

    fn read_export_section(&mut self, module: &mut Module) -> Result<()> {
        self.collect(&mut module.exports, Self::read_export)
    }

    fn read_start_section(&mut self, module: &mut Module) -> Result<()> {
        module.start = Some(self.read_index()?);
        Ok(())
    }

    fn read_element_section(&mut self, module: &mut Module) -> Result<()> {
        self.collect(&mut module.elements, Self::read_element)
    }

    fn read_data_count_section(&mut self, module: &mut Module) -> Result<()> {
        module.data_count = self.read_length()?;
        Ok(())
    }

    fn read_code_section(&mut self, module: &mut Module) -> Result<()> {
        self.collect(&mut module.function_bodies, Self::read_function_body)
    }

    fn read_data_section(&mut self, module: &mut Module) -> Result<()> {
        self.collect(&mut module.data_segments, Self::read_data_segment)
    }

    pub(super) fn read_module(&mut self) -> Result<Module> {
        let magic = [0x00, 0x61, 0x73, 0x6d];
        let version = [0x01, 0x00, 0x00, 0x00];

        if self.slice.get(0..4) != Some(&magic[..]) {
            return error!(ErrorType::InvalidMagic);
        }

        if self.slice.get(4..8) != Some(&version[..]) {
            return error!(ErrorType::UnsupportedVersion);
        }

        self.index += 8;
        let mut module = Module {
            function_types: Vec::new(),
            imports: Vec::new(),
            function_signatures: Vec::new(),
            tables: Vec::new(),
            memories: Vec::new(),
            globals: Vec::new(),
            exports: Vec::new(),
            start: None,
            elements: Vec::new(),
            data_count: 0,
            function_bodies: Vec::new(),
            data_segments: Vec::new(),
        };

        while self.index < self.slice.len() {
            use SectionId as Id;

            let (sec_id, mut section_parser) = self.read_section()?;

            let callback = match sec_id {
                Id::Custom => Self::read_custom_section,
                Id::Type => Self::read_type_section,
                Id::Import => Self::read_import_section,
                Id::Function => Self::read_function_section,
                Id::Table => Self::read_table_section,
                Id::Memory => Self::read_memory_section,
                Id::Global => Self::read_global_section,
                Id::Export => Self::read_export_section,
                Id::Start => Self::read_start_section,
                Id::Element => Self::read_element_section,
                Id::DataCount => Self::read_data_count_section,
                Id::Code => Self::read_code_section,
                Id::Data => Self::read_data_section,
            };

            callback(&mut section_parser, &mut module)?;
        }

        Ok(module)
    }
}
