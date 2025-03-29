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
use super::keyword::Keyword;

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub struct Call {
    pub func: Box<ExprType>,
    pub args: Vec<ExprType>,
    pub keywords: Vec<Keyword>,
}

impl<'a> FromPyObject<'a> for Call {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let func = ob.getattr("func").expect("Call.func");
        let args = ob.getattr("args").expect("Call.args");
        let keywords = ob.getattr("keywords").expect("Call.keywords");
        Ok(Call {
            func: Box::new(func.extract().expect("Call.func")),
            args: args.extract().expect("Call.args"),
            keywords: keywords.extract().expect("Call.keywords"),
        })
    }
}

impl CodeGen for Call {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        ctx: Self::Context,
        options: Self::Options,
        symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        let name = self
            .func
            .to_rust(ctx.clone(), options.clone(), symbols.clone())?;
        let args_code = self
            .args
            .into_iter()
            .map(|arg| arg.to_rust(ctx.clone(), options.clone(), symbols.clone()))
            .collect::<Result<Vec<_>>>()?;
        let args_tokens = quote! {
            #(#args_code),*
        };
        Ok(quote! { #name(#args_tokens) })
    }
}
