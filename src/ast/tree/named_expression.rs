use anyhow::Result;
use pyo3::prelude::*;
use serde::{Deserialize, Serialize};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    codegen::{CodeGen, CodeGenContext, python_options::PythonOptions},
    symbols::SymbolTableScopes,
};

use super::expression::ExprType;

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub struct NamedExpr {
    pub left: Box<ExprType>,
    pub right: Box<ExprType>,
}

impl<'a> FromPyObject<'a> for NamedExpr {
    fn extract(ob: &pyo3::PyAny) -> pyo3::PyResult<Self> {
        let left = ob.getattr("left")?.extract::<ExprType>()?;
        let right = ob.getattr("right")?.extract::<ExprType>()?;
        Ok(NamedExpr {
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

impl CodeGen for NamedExpr {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        ctx: Self::Context,
        options: Self::Options,
        symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        let left = self
            .left
            .to_rust(ctx.clone(), options.clone(), symbols.clone())?;
        let right = self
            .right
            .to_rust(ctx.clone(), options.clone(), symbols.clone())?;
        Ok(quote! { #left = #right })
    }
}
