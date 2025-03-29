use anyhow::Result;
use log::debug;
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
pub enum Compares {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
    Is,
    IsNot,
    In,
    NotIn,
}


impl<'a> FromPyObject<'a> for Compares {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let err_msg = format!("Unimplemented unary op {}", dump(ob, None)?);
        Err(pyo3::exceptions::PyValueError::new_err(
            ob.error_message("<unknown>", err_msg),
        ))
    }
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Compare {
    pub left: Box<ExprType>,
    pub ops: Vec<String>, // Using String to store the op type
    pub comparators: Vec<ExprType>,
}

impl<'a> FromPyObject<'a> for Compare {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        log::debug!("ob: {}", dump(ob, None)?);

        // Python allows for multiple comparators, rust we only supports one, so we have to rewrite
        let ops: Vec<&PyAny> = ob
            .getattr("ops")
            .expect(
                ob.error_message("<unknown>", "error getting unary operator")
                    .as_str(),
            )
            .extract()
            .expect("5");

        let mut ops_str: Vec<String> = Vec::new();
        for op in ops.iter() {
            let op_type = op.get_type().name().expect(
                ob.error_message(
                    "<unknown>",
                    format!("extracting type name {:?} in Compare", dump(ob, None)),
                )
                .as_str(),
            );
            ops_str.push(op_type.to_string());
        }


        let left = ob
            .getattr("left")
            .expect(
                ob.error_message("<unknown>", "error getting unary operator")
                    .as_str(),
            )
            .extract()
            .expect("6");
        let comparators: Vec<ExprType> = ob
            .getattr("comparators")
            .expect(
                ob.error_message("<unknown>", "error getting unary operator")
                    .as_str(),
            )
            .extract()
            .expect("7");

        Ok(Compare {
            left: Box::new(left),
            ops: ops_str,
            comparators,
        })
    }
}

impl CodeGen for Compare {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        ctx: Self::Context,
        options: Self::Options,
        symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        let op = self.ops[0].clone(); // For now, just handle the first op
        let left = self
            .left
            .clone()
            .to_rust(ctx.clone(), options.clone(), symbols.clone())?;

        // Assuming only one comparator for now, handle multiple later if needed
        let comparator = self.comparators
            .get(0)
            .expect("getting comparator")
            .clone()
            .to_rust(ctx.clone(), options.clone(), symbols.clone())?;

        match op.as_str() {
            "Eq" => Ok(quote! { #left == #comparator }),
            "NotEq" => Ok(quote! { #left != #comparator }),
            "Lt" => Ok(quote! { #left < #comparator }),
            "LtE" => Ok(quote! { #left <= #comparator }),
            "Gt" => Ok(quote! { #left > #comparator }),
            "GtE" => Ok(quote! { #left >= #comparator }),
            _ => {
                let err_msg = format!("Unimplemented compare operator: {}", op);
                Err(anyhow::anyhow!(err_msg))
            }
        }
    }
}
