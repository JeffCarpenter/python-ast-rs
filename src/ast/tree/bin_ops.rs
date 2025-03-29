use anyhow::Result;
use pyo3::prelude::*;
use serde::{Deserialize, Serialize};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    ast::dump::dump,
    codegen::{CodeGen, CodeGenContext, python_options::PythonOptions},
    symbols::SymbolTableScopes,
};

use super::expression::ExprType;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Ops {
    Add,
    Sub,
    Mult,
    MatMult,
    Div,
    Mod,
    Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
    FloorDiv,
}

impl<'a> FromPyObject<'a> for Ops {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let err_msg = format!("Unimplemented unary op {}", dump(ob, None)?);
        Err(pyo3::exceptions::PyValueError::new_err(
            ob.error_message("<unknown>", err_msg),
        ))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct BinOp {
    pub left: Box<ExprType>,
    pub op: String,
    pub right: Box<ExprType>,
}

impl<'a> FromPyObject<'a> for BinOp {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        log::debug!("ob: {}", dump(ob, None)?);
        let op = ob.getattr("op").expect(
            ob.error_message("<unknown>", "error getting unary operator")
                .as_str(),
        );

        let op_type = op.get_type().name().expect(
            ob.error_message(
                "<unknown>",
                format!("extracting type name {:?} in BinOp", dump(ob, None)),
            )
            .as_str(),
        );
        log::debug!("op_type: {}", op_type);

        let left = ob
            .getattr("left")
            .expect(
                ob.error_message("<unknown>", "error getting unary operator")
                    .as_str(),
            )
            .extract()
            .expect("3");
        let right = ob
            .getattr("right")
            .expect(
                ob.error_message("<unknown>", "error getting unary operator")
                    .as_str(),
            )
            .extract()
            .expect("4");

        Ok(BinOp {
            left: Box::new(left),
            op: op_type.to_string(), // Capture the operation type as a string
            right: Box::new(right),
        })
    }
}

impl CodeGen for BinOp {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        ctx: Self::Context,
        options: Self::Options,
        symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        let op = self.op;
        let left = self
            .left
            .clone()
            .to_rust(ctx.clone(), options.clone(), symbols.clone())?;
        let right = self
            .right
            .clone()
            .to_rust(ctx.clone(), options.clone(), symbols.clone())?;

        match op.as_str() {
            "Add" => Ok(quote! { #left + #right }),
            "Sub" => Ok(quote! { #left - #right }),
            "Mult" => Ok(quote! { #left * #right }),
            "Div" => Ok(quote! { #left / #right }),
            "Mod" => Ok(quote! { #left % #right }),
            "BitAnd" => Ok(quote! { #left & #right }),
            "BitOr" => Ok(quote! { #left | #right }),
            "BitXor" => Ok(quote! { #left ^ #right }),
            "LShift" => Ok(quote! { #left << #right }),
            "RShift" => Ok(quote! { #left >> #right }),
            _ => {
                let err_msg = format!("Unimplemented binary operator: {}", op);
                Err(anyhow::anyhow!(err_msg))
            }
        }
    }
}
