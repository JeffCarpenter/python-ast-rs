use std::collections::VecDeque;

use anyhow::Result;
use log::{debug, error};
use pyo3::{
    prelude::*,
    types::{PyList},
    FromPyObject,
};
use serde::{Deserialize, Serialize};

use crate::{
    ast::dump::dump,
    pytypes::{ListLike},
};

use crate::ast::tree::{
    bool_ops::BoolOp,
    bin_ops::BinOp,
    compare::Compare,
    constant::Constant,
    name::Name,
    named_expression::NamedExpr,
    unary_op::UnaryOp,
    attribute::Attribute,
    call::Call,
};
use crate::ast::tree::statement::Statement;
use crate::ast::tree::keyword::Keyword;
use crate::ast::tree::comprehension::Comprehension;
use crate::ast::tree::arguments::Arguments;
use crate::ast::tree::parameters::Parameter;


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Container<T>(pub crate::pytypes::List<T>);

impl<T> Container<T> {
    pub fn new() -> Self {
        Self(VecDeque::new())
    }

    pub fn append(&mut self, value: T) {
        self.0.push_back(Box::new(value));
    }

    pub fn iter(&self) -> std::collections::vec_deque::Iter<'_, Box<T>> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.0.len() {
            self.0.get(index).map(|boxed_value| boxed_value.as_ref())
        } else {
            None
        }
    }
}


impl<'a, 'p> FromPyObject<'a> for Container<ExprType>
{
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let list = crate::pytypes::List::<ExprType>::new();

        log::debug!("pylist: {}", dump(ob, Some(4))?);
        // Check if the object is a PyList
        if let Ok(py_list) = ob.downcast::<PyList>() {
            for item in py_list.iter() {
                match ExprType::extract(item) {
                    Ok(expr_type) => list.append(expr_type),
                    Err(e) => {
                        error!("Failed to extract ExprType from PyList item: {}", e);
                        return Err(e);
                    }
                }
            }
            Ok(Self(list))
        } else {
            Err(pyo3::exceptions::PyTypeError::new_err(
                "Expected a PyList",
            ))
        }
    }
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Expr {
    #[serde(flatten)]
    pub value: ExprType,

    pub lineno: Option<usize>,
    pub col_offset: Option<usize>,
    pub end_lineno: Option<usize>,
    pub end_col_offset: Option<usize>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub enum ExprType {
    BoolOp(BoolOp),
    NamedExpr(NamedExpr),
    BinOp(BinOp),
    UnaryOp(UnaryOp),
    Constant(Constant),
    Attribute(Attribute),
    Call(Call),
    Compare(Compare),
    Name(Name),
    List(List),
    Tuple(Tuple),
    Await(Await),
    IfExp(IfExp),
    Starred(Starred),
    Yield(Yield),
    YieldFrom(YieldFrom),
    Dict(Dict),
    Set(Set),
    ListComp(ListComp),
    SetComp(SetComp),
    DictComp(DictComp),
    GeneratorExp(GeneratorExp),
    Class(Class),
    Function(Function),
    Lambda(Lambda),
    String(StringExpr),
    Bytes(BytesExpr),
    Number(NumberExpr),
    #[default]
    None,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct List {
    pub elts: Container<ExprType>,
    pub ctx: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Tuple {
    pub elts: Container<ExprType>,
    pub ctx: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Await {
    pub value: Box<ExprType>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct IfExp {
    pub test: Box<ExprType>,
    pub body: Box<ExprType>,
    pub orelse: Box<ExprType>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Starred {
    pub value: Box<ExprType>,
    pub ctx: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Yield {
    pub value: Option<Box<ExprType>>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct YieldFrom {
    pub value: Box<ExprType>,
}
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Dict {
    pub keys: Container<ExprType>,
    pub values: Container<ExprType>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Set {
    pub elts: Container<ExprType>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct ListComp {
    pub elt: Box<ExprType>,
    pub generators: Container<Comprehension>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct SetComp {
    pub elt: Box<ExprType>,
    pub generators: Container<Comprehension>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct DictComp {
    pub key: Box<ExprType>,
    pub value: Box<ExprType>,
    pub generators: Container<Comprehension>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct GeneratorExp {
    pub elt: Box<ExprType>,
    pub generators: Container<Comprehension>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Class {
    pub bases: Container<ExprType>,
    pub keywords: Container<Keyword>,
    pub body: Container<Statement>,
    pub decorator_list: Container<ExprType>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Function {
    pub args: Arguments,
    pub body: Container<Statement>,
    pub decorator_list: Container<ExprType>,
    pub returns: Option<Box<ExprType>>,
    pub type_comment: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Lambda {
    pub args: Arguments,
    pub body: Box<ExprType>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct StringExpr {
    pub value: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct BytesExpr {
    pub value: Vec<u8>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct NumberExpr {
    pub value: String, // Could be int or float, using String for simplicity
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Comprehension {
    pub target: Expr,
    pub iter: Expr,
    pub ifs: Container<ExprType>,
    pub is_async: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Keyword {
    pub arg: Option<String>,
    pub value: Expr,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Arguments {
    pub posonlyargs: Container<Parameter>,
    pub args: Container<Parameter>,
    pub vararg: Option<Parameter>,
    pub kwonlyargs: Container<Parameter>,
    pub kw_defaults: Container<ExprType>, // Changed from Option<Expr> to ExprType
    pub kwarg: Option<Parameter>,
    pub defaults: Container<ExprType>, // Changed from Option<Expr> to ExprType
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Parameter {
    pub arg: String,
    pub annotation: Option<Expr>,
    pub type_comment: Option<String>,
}


impl<'a> FromPyObject<'a> for ExprType {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        log::debug!("exprtype ob: {}", dump(ob, Some(4))?);

        let expr_type = ob.get_type().name().expect(
            ob.error_message(
                "<unknown>",
                format!("extracting type name {:?} in expression", dump(ob, None)),
            )
            .as_str(),
        );

        match expr_type.as_ref() {
            "BoolOp" => Ok(ExprType::BoolOp(BoolOp::extract(ob)?)),
            "NamedExpr" => Ok(ExprType::NamedExpr(NamedExpr::extract(ob)?)),
            "BinOp" => Ok(ExprType::BinOp(BinOp::extract(ob)?)),
            "UnaryOp" => Ok(ExprType::UnaryOp(UnaryOp::extract(ob)?)),
            "Constant" => Ok(ExprType::Constant(Constant::extract(ob)?)),
            "Attribute" => Ok(ExprType::Attribute(Attribute::extract(ob)?)),
            "Call" => Ok(ExprType::Call(Call::extract(ob)?)),
            "Compare" => Ok(ExprType::Compare(Compare::extract(ob)?)),
            "Name" => Ok(ExprType::Name(Name::extract(ob)?)),
            "List" => Ok(ExprType::List(List::extract(ob)?)),
            "Tuple" => Ok(ExprType::Tuple(Tuple::extract(ob)?)),
            "Await" => Ok(ExprType::Await(Await::extract(ob)?)),
            "IfExp" => Ok(ExprType::IfExp(IfExp::extract(ob)?)),
            "Starred" => Ok(ExprType::Starred(Starred::extract(ob)?)),
            "Yield" => Ok(ExprType::Yield(Yield::extract(ob)?)),
            "YieldFrom" => Ok(ExprType::YieldFrom(YieldFrom::extract(ob)?)),
            "Dict" => Ok(ExprType::Dict(Dict::extract(ob)?)),
            "Set" => Ok(ExprType::Set(Set::extract(ob)?)),
            "ListComp" => Ok(ExprType::ListComp(ListComp::extract(ob)?)),
            "SetComp" => Ok(ExprType::SetComp(SetComp::extract(ob)?)),
            "DictComp" => Ok(ExprType::DictComp(DictComp::extract(ob)?)),
            "GeneratorExp" => Ok(ExprType::GeneratorExp(GeneratorExp::extract(ob)?)),
            "ClassDef" => Ok(ExprType::Class(Class::extract(ob)?)), // Assuming ClassDef maps to Class
            "FunctionDef" => Ok(ExprType::Function(Function::extract(ob)?)), // Assuming FunctionDef maps to Function
            "Lambda" => Ok(ExprType::Lambda(Lambda::extract(ob)?)),
            "str" => {
                let value: String = ob.extract()?;
                Ok(ExprType::String(StringExpr{value}))
            },
            "bytes" => {
                let value: Vec<u8> = ob.extract()?;
                Ok(ExprType::Bytes(BytesExpr{value}))
            },
            "int" | "float" => {
                let value: String = ob.str()?.extract()?;
                Ok(ExprType::Number(NumberExpr{value}))
            },
            "NoneType" => Ok(ExprType::None),
            _ => {
                let err_msg = format!("Unimplemented expression type: {}", expr_type);
                Err(pyo3::exceptions::PyValueError::new_err(
                    ob.error_message("<unknown>", err_msg),
                ))
            }
        }
    }
}

impl<'a> FromPyObject<'a> for Expr {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let err_msg = format!("extracting object value {} in expression", dump(ob, None)?);

        let ob_value = ob
            .getattr("value")
            .expect(ob.error_message("<unknown>", err_msg.as_str()).as_str());
        log::debug!("ob_value: {}", dump(ob_value, None)?);

        // The context is Load, Store, etc. For some types of expressions such as Constants, it doesn't exist.
        let ctx: Option<String> = if let Ok(pyany) = ob_value.getattr("ctx") {
            pyany.get_type().name().ok().map(|s| s.to_string())
        } else {
            None
        };

        Ok(Self {
            value: ExprType::extract(ob_value)?,
            lineno: ob.lineno(),
            col_offset: ob.col_offset(),
            end_lineno: ob.end_lineno(),
            end_col_offset: ob.end_col_offset(),
        })
    }
}

use crate::codegen::CodeGen;
use crate::codegen::python_options::PythonOptions;
use crate::codegen::CodeGenContext;
use crate::symbols::SymbolTableScopes;
use proc_macro2::TokenStream;

impl CodeGen for Expr {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        ctx: Self::Context,
        options: Self::Options,
        symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        self.value.to_rust(ctx, options, symbols)
    }

    fn find_symbols(self, symbols: Self::SymbolTable) -> Self::SymbolTable {
        self.value.find_symbols(symbols)
    }
}


impl CodeGen for ExprType {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        ctx: Self::Context,
        options: Self::Options,
        symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        match self {
            ExprType::BoolOp(op) => op.to_rust(ctx, options, symbols),
            ExprType::NamedExpr(named_expr) => named_expr.to_rust(ctx, options, symbols),
            ExprType::BinOp(op) => op.to_rust(ctx, options, symbols),
            ExprType::UnaryOp(op) => op.to_rust(ctx, options, symbols),
            ExprType::Constant(constant) => constant.to_rust(ctx, options, symbols),
            ExprType::Attribute(attribute) => attribute.to_rust(ctx, options, symbols),
            ExprType::Call(call) => call.to_rust(ctx, options, symbols),
            ExprType::Compare(compare) => compare.to_rust(ctx, options, symbols),
            ExprType::Name(name) => name.to_rust(ctx, options, symbols),
            ExprType::List(list) => list.to_rust(ctx, options, symbols),
            ExprType::Tuple(tuple) => tuple.to_rust(ctx, options, symbols),
            ExprType::Await(await_expr) => await_expr.to_rust(ctx, options, symbols),
            ExprType::IfExp(if_exp) => if_exp.to_rust(ctx, options, symbols),
            ExprType::Starred(starred) => starred.to_rust(ctx, options, symbols),
            ExprType::Yield(yield_expr) => yield_expr.to_rust(ctx, options, symbols),
            ExprType::YieldFrom(yield_from) => yield_from.to_rust(ctx, options, symbols),
            ExprType::Dict(dict) => dict.to_rust(ctx, options, symbols),
            ExprType::Set(set) => set.to_rust(ctx, options, symbols),
            ExprType::ListComp(list_comp) => list_comp.to_rust(ctx, options, symbols),
            ExprType::SetComp(set_comp) => set_comp.to_rust(ctx, options, symbols),
            ExprType::DictComp(dict_comp) => dict_comp.to_rust(ctx, options, symbols),
            ExprType::GeneratorExp(generator_exp) => generator_exp.to_rust(ctx, options, symbols),
            ExprType::Class(class_def) => class_def.to_rust(ctx, options, symbols),
            ExprType::Function(function_def) => function_def.to_rust(ctx, options, symbols),
            ExprType::Lambda(lambda) => lambda.to_rust(ctx, options, symbols),
            ExprType::String(string_expr) => string_expr.to_rust(ctx, options, symbols),
            ExprType::Bytes(bytes_expr) => bytes_expr.to_rust(ctx, options, symbols),
            ExprType::Number(number_expr) => number_expr.to_rust(ctx, options, symbols),
            ExprType::None => Ok(quote! { None }), // Handle None explicitly
        }
    }

    fn find_symbols(self, symbols: Self::SymbolTable) -> Self::SymbolTable {
        match self {
            ExprType::BoolOp(op) => op.find_symbols(symbols),
            ExprType::NamedExpr(named_expr) => named_expr.find_symbols(symbols),
            ExprType::BinOp(op) => op.find_symbols(symbols),
            ExprType::UnaryOp(op) => op.find_symbols(symbols),
            ExprType::Constant(_) => symbols, // Constants don't introduce new symbols
            ExprType::Attribute(_) => symbols, // Attributes don't introduce new symbols
            ExprType::Call(call) => call.find_symbols(symbols),
            ExprType::Compare(compare) => compare.find_symbols(symbols),
            ExprType::Name(_) => symbols,    // Names don't introduce new symbols at this level
            ExprType::List(_) => symbols,    // Lists don't introduce new symbols at this level
            ExprType::Tuple(_) => symbols,   // Tuples don't introduce new symbols at this level
            ExprType::Await(_) => symbols,   // Await doesn't introduce new symbols at this level
            ExprType::IfExp(_) => symbols,   // IfExp doesn't introduce new symbols at this level
            ExprType::Starred(_) => symbols, // Starred doesn't introduce new symbols at this level
            ExprType::Yield(_) => symbols,   // Yield doesn't introduce new symbols at this level
            ExprType::YieldFrom(_) => symbols, // YieldFrom doesn't introduce new symbols at this level
            ExprType::Dict(_) => symbols,    // Dict doesn't introduce new symbols at this level
            ExprType::Set(_) => symbols,     // Set doesn't introduce new symbols at this level
            ExprType::ListComp(_) => symbols, // ListComp doesn't introduce new symbols at this level
            ExprType::SetComp(_) => symbols,  // SetComp doesn't introduce new symbols at this level
            ExprType::DictComp(_) => symbols, // DictComp doesn't introduce new symbols at this level
            ExprType::GeneratorExp(_) => symbols, // GeneratorExp doesn't introduce new symbols at this level
            ExprType::Class(_) => symbols,    // ClassDef doesn't introduce new symbols at this level (ClassDef statement does)
            ExprType::Function(_) => symbols, // FunctionDef doesn't introduce new symbols at this level (FunctionDef statement does)
            ExprType::Lambda(_) => symbols,  // Lambda doesn't introduce new symbols at this level
            ExprType::String(_) => symbols,  // StringExpr doesn't introduce new symbols
            ExprType::Bytes(_) => symbols,   // BytesExpr doesn't introduce new symbols
            ExprType::Number(_) => symbols,  // NumberExpr doesn't introduce new symbols
            ExprType::None => symbols,       // None doesn't introduce new symbols
        }
    }
}
