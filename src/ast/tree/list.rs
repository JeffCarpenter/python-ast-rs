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

use super::expression::{ExprType, Expr, Container};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct List {
    pub elts: Container<ExprType>,
    pub ctx: Option<String>,
}

impl<'a> FromPyObject<'a> for List {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let elts = ob.getattr("elts").expect("List.elts");
        let ctx = ob
            .getattr("ctx")
            .expect("getting list context")
            .get_type()
            .name()
            .expect(
                ob.error_message(
                    "<unknown>",
                    format!("getting list context {:?}", dump(ob, None)),
                )
                .as_str(),
            );

        Ok(List {
            elts: elts.extract().expect("List.elts"),
            ctx: Some(ctx),
        })
    }
}

impl CodeGen for List {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        _ctx: Self::Context,
        _options: Self::Options,
        _symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        let elements = self.elts;
        let element_codes = elements.iter()
            .map(|element| element.to_rust(_ctx.clone(), _options.clone(), _symbols.clone()))
            .collect::<Result<Vec<_>>>()?;
        Ok(quote! { vec![#(#element_codes),*] })
    }
}

#[cfg(test)]
mod tests {
    use pyo3::{Python, types::PyList};
    use crate::parser::parse;
    use super::*;

    #[test]
    fn test_list_extraction() {
        Python::with_gil(|py| {
            let code = "[1, 2, 3]";
            let module = parse(code, "test_list.py").unwrap();
            let statement = module.raw.body.get(0).unwrap();
            if let crate::ast::tree::statement::StatementType::Expr(expr_stmt) = &statement.statement {
                if let ExprType::List(list) = &expr_stmt.value {
                    let elements = &list.elts;
                    assert_eq!(elements.len(), 3);
                    assert!(matches!(elements.get(0).unwrap(), ExprType::Constant(_)));
                    assert!(matches!(elements.get(1).unwrap(), ExprType::Constant(_)));
                    assert!(matches!(elements.get(2).unwrap(), ExprType::Constant(_)));

                    if let ExprType::Constant(c) = elements.get(0).unwrap() {
                        assert_eq!(c.to_string(), "1");
                    }
                    if let ExprType::Constant(c) = elements.get(1).unwrap() {
                        assert_eq!(c.to_string(), "2");
                    }
                    if let ExprType::Constant(c) = elements.get(2).unwrap() {
                        assert_eq!(c.to_string(), "3");
                    }
                } else {
                    panic!("Expected ExprType::List, got {:?}", expr_stmt.value);
                }
            } else {
                panic!("Expected StatementType::Expr");
            }
        });
    }


    #[test]
    fn test_nested_list_extraction() {
        Python::with_gil(|py| {
            let code = "[[1, 2, 3], [4, 5, 6], [7, 8, 9]]";
            let module = parse(code, "test_list.py").unwrap();
            let statement = module.raw.body.get(0).unwrap();
            if let crate::ast::tree::statement::StatementType::Expr(expr_stmt) = &statement.statement {
                if let ExprType::List(list) = &expr_stmt.value {
                    let elements = &list.elts;
                    assert_eq!(elements.len(), 3);
                    assert!(matches!(elements.get(0).unwrap(), ExprType::List(_)));
                    assert!(matches!(elements.get(1).unwrap(), ExprType::List(_)));
                    assert!(matches!(elements.get(2).unwrap(), ExprType::List(_)));

                    if let ExprType::List(l) = elements.get(0).unwrap() {
                        assert_eq!(l.elts.len(), 3);
                    }
                    if let ExprType::List(l) = elements.get(1).unwrap() {
                        assert_eq!(l.elts.len(), 3);
                    }
                    if let ExprType::List(l) = elements.get(2).unwrap() {
                        assert_eq!(l.elts.len(), 3);
                    }
                } else {
                    panic!("Expected ExprType::List, got {:?}", expr_stmt.value);
                }
            } else {
                panic!("Expected StatementType::Expr");
            }
        });
    }
}
