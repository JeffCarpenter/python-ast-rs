use anyhow::Result;
use pyo3::prelude::*;
use serde::{Deserialize, Serialize};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    ast::dump::dump,
    codegen::{CodeGen, CodeGenContext, python_options::PythonOptions},
    symbols::{SymbolTableScopes, SymbolTableNode},
};

use super::{expression::{ExprType, Expr}, name::Name};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Assign {
    pub targets: Vec<Name>,
    pub value: ExprType,
    pub type_comment: Option<String>,
}

impl<'a> FromPyObject<'a> for Assign {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let targets: Vec<Name> = ob
            .getattr("targets")
            .expect(
                ob.error_message("<unknown>", "error getting unary operator")
                    .as_str(),
            )
            .extract()
            .expect("1");

        let value = ob
            .getattr("value")
            .expect(
                ob.error_message("<unknown>", "error getting unary operator")
                    .as_str(),
            )
            .extract()
            .expect("2");

        Ok(Assign {
            targets,
            value,
            type_comment: None,
        })
    }
}

impl CodeGen for Assign {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn find_symbols(self, symbols: Self::SymbolTable) -> Self::SymbolTable {
        let mut symbols = symbols;
        let mut position = 0;
        for target in self.targets {
            symbols.insert(
                target.id,
                SymbolTableNode::Assign {
                    position: position,
                    value: self.value.clone(),
                },
            );
            position += 1;
        }
        symbols
    }

    fn to_rust(
        self,
        ctx: Self::Context,
        options: Self::Options,
        symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        let mut code = Vec::new();
        for target in self.targets {
            let target_str = target.id;
            let value = self.value.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
            code.push(quote! { let #target_str = #value; });
        }
        Ok(quote! { #(#code)* })
    }
}
