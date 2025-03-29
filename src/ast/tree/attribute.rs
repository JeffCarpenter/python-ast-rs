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
pub struct Attribute {
    pub value: Box<ExprType>,
    pub attr: String,
    pub ctx: Option<String>,
}

impl<'a> FromPyObject<'a> for Attribute {
    fn extract(ob: &pyo3::PyAny) -> pyo3::PyResult<Self> {
        let value = ob.getattr("value").expect("Attribute.value");
        let attr = ob.getattr("attr").expect("Attribute.attr");
        let ctx = ob
            .getattr("ctx")
            .expect("getting attribute context")
            .get_type()
            .name()
            .expect(
                ob.error_message(
                    "<unknown>",
                    format!("getting attribute context {:?}", dump(ob, None)),
                )
                .as_str(),
            );

        Ok(Attribute {
            value: Box::new(ExprType::extract(&value).expect("Attribute.value")),
            attr: attr.extract().expect("Attribute.attr"),
            ctx: Some(ctx),
        })
    }
}

impl CodeGen for Attribute {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        _ctx: Self::Context,
        _options: Self::Options,
        _symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        let name = self
            .value
            .to_rust(_ctx, _options, _symbols)?;
        let attr = self.attr;
        Ok(quote! { #name.#attr })
    }
}
