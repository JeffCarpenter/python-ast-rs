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
    Invert,
    Not,
    UAdd,
    USub,
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
pub struct UnaryOp {
    pub op: String,
    pub operand: Box<ExprType>,
}

impl<'a> FromPyObject<'a> for UnaryOp {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        log::debug!("ob: {}", dump(ob, None)?);
        let op = ob.getattr("op").expect(
            ob.error_message("<unknown>", "error getting unary operator")
                .as_str(),
        );

        let op_type = op.get_type().name().expect(
            ob.error_message(
                "<unknown>",
                format!("extracting type name {:?} in UnaryOp", dump(ob, None)),
            )
            .as_str(),
        );
        log::debug!("op_type: {}", op_type);

        let operand = ob
            .getattr("operand")
            .expect(
                ob.error_message("<unknown>", "error getting unary operator")
                    .as_str(),
            )
            .extract()
            .expect("8");

        Ok(UnaryOp {
            op: op_type.to_string(), // Capture the operation type as a string
            operand: Box::new(operand),
        })
    }
}

impl CodeGen for UnaryOp {
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
        let operand = self
            .operand
            .clone()
            .to_rust(ctx, options, symbols)?;

        match op.as_str() {
            "Not" => Ok(quote! { !#operand }),
            "USub" => Ok(quote! { -#operand }),
            "UAdd" => Ok(quote! { +#operand }),
            "Invert" => Ok(quote! { !#operand }), // Assuming bitwise NOT is also ! in Rust
            _ => {
                let err_msg = format!("Unimplemented unary operator: {}", op);
                Err(anyhow::anyhow!(err_msg))
            }
        }
    }
}
