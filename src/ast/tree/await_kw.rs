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

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Await {
    pub value: Box<ExprType>,
}

impl FromPyObject<'_> for Await {
    fn extract(ob: &pyo3::PyAny) -> pyo3::PyResult<Self> {
        let value = ob.getattr("value").expect("Await.value");
        Ok(Await {
            value: Box::new(value.extract().expect("Await.value")),
        })
    }
}

impl CodeGen for Await {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        _ctx: Self::Context,
        _options: Self::Options,
        _symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        let value = self
            .value
            .to_rust(_ctx, _options, _symbols)?;
        Ok(quote! { #value.await })
    }
}
