use proc_macro2::TokenStream;
use pyo3::{FromPyObject, PyAny, PyResult};
use quote::quote;
use serde::{Deserialize, Serialize}; // Added serde

use crate::{
    ast::{
        dump::dump, // Corrected import path
        node::Node, // Corrected import path
        tree::{ // Corrected import path
            assign::Assign,
            call::Call,
            class_def::ClassDef,
            expression::{Expr, ExprType}, // Corrected import path
            function_def::FunctionDef,
            import::Import,
            import_from::ImportFrom,
        },
    },
    codegen::{CodeGen, CodeGenContext, PythonOptions}, // Corrected import path
    error::Error, // Corrected import path
    symbols::SymbolTableScopes, // Corrected import path
};

use log::debug;


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Statement {
    pub lineno: Option<usize>,
    pub col_offset: Option<usize>,
    pub end_lineno: Option<usize>,
    pub end_col_offset: Option<usize>,
    pub statement: StatementType,
}

impl<'a> FromPyObject<'a> for Statement {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        Ok(Self {
            lineno: ob.lineno(),
            col_offset: ob.col_offset(),
            end_lineno: ob.end_lineno(),
            end_col_offset: ob.end_col_offset(),
            statement: StatementType::extract(ob)?,
        })
    }
}

impl Node for Statement {
    fn lineno(&self) -> Option<usize> {
        self.lineno
    }
    fn col_offset(&self) -> Option<usize> {
        self.col_offset
    }
    fn end_lineno(&self) -> Option<usize> {
        self.end_lineno
    }
    fn end_col_offset(&self) -> Option<usize> {
        self.end_col_offset
    }
}

impl CodeGen for Statement {
    fn to_rust(
        &self,
        ctx: &CodeGenContext,
        options: &PythonOptions,
        symbols: &mut SymbolTableScopes,
    ) -> Result<TokenStream, anyhow::Error> { // Changed error type
        self
            .statement
            .to_rust(ctx, options, symbols)
            .map_err(|e| anyhow::anyhow!("Failed to compile statement {:?}: {}", self, e))

    }

    fn find_symbols(&self, symbols: &mut SymbolTableScopes) {
        self.statement.find_symbols(symbols)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum StatementType {
    AsyncFunctionDef(FunctionDef),
    Assign(Assign),
    Break,
    Continue,
    ClassDef(ClassDef),
    // Call is an expression, should be wrapped in Expr statement
    Pass,
    Return(Option<Expr>),
    Import(Import),
    ImportFrom(ImportFrom),
    Expr(Expr), // Represents expression statements (like standalone calls or constants)
    FunctionDef(FunctionDef),

    Unimplemented(String), // Keep for now
}

impl<'a> FromPyObject<'a> for StatementType {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let type_name = ob.get_type().name()?;
        debug!("Extracting StatementType for: {} - {}", type_name, dump(ob, Some(4))?);

        match type_name.as_ref() {
            "AsyncFunctionDef" => Ok(StatementType::AsyncFunctionDef(FunctionDef::extract(ob)?)),
            "Assign" => Ok(StatementType::Assign(Assign::extract(ob)?)),
            "Pass" => Ok(StatementType::Pass),
            "ClassDef" => Ok(StatementType::ClassDef(ClassDef::extract(ob)?)),
            "Continue" => Ok(StatementType::Continue),
            "Break" => Ok(StatementType::Break),
            "FunctionDef" => Ok(StatementType::FunctionDef(FunctionDef::extract(ob)?)),
            "Import" => Ok(StatementType::Import(Import::extract(ob)?)),
            "ImportFrom" => Ok(StatementType::ImportFrom(ImportFrom::extract(ob)?)),
            "Expr" => {
                // This is an expression used as a statement (e.g., a function call, a constant)
                let expr_node = ob.getattr("value")?;
                Ok(StatementType::Expr(Expr::extract(expr_node)?))
            }
            "Return" => {
                let value_attr = ob.getattr("value")?;
                // Check if the return value is None (Python None)
                let return_expr = if value_attr.is_none() {
                    None // Represents `return` without a value
                } else {
                    Some(Expr::extract(value_attr)?) // Extract the expression
                };
                Ok(StatementType::Return(return_expr))
            }
            _ => Err(pyo3::exceptions::PyNotImplementedError::new_err(format!(
                "Unimplemented statement type {}, {}",
                type_name,
                dump(ob, None)?
            ))),
        }
    }
}

impl CodeGen for StatementType {

    fn find_symbols(&self, symbols: &mut SymbolTableScopes) {
        match self {
            StatementType::Assign(a) => a.find_symbols(symbols),
            StatementType::ClassDef(c) => c.find_symbols(symbols),
            StatementType::FunctionDef(f) | StatementType::AsyncFunctionDef(f) => f.find_symbols(symbols),
            StatementType::Import(i) => i.find_symbols(symbols),
            StatementType::ImportFrom(i) => i.find_symbols(symbols),
            StatementType::Expr(e) => e.find_symbols(symbols),
            // Return, Pass, Break, Continue don't typically introduce symbols at this level
            _ => {}
        }
    }

    fn to_rust(
        &self,
        ctx: &CodeGenContext,
        options: &PythonOptions,
        symbols: &mut SymbolTableScopes,
    ) -> Result<TokenStream, anyhow::Error> { // Changed error type
        match self {
            StatementType::AsyncFunctionDef(s) => {
                let async_ctx = CodeGenContext::Async(Box::new(ctx.clone()));
                s.to_rust(&async_ctx, options, symbols)
            }
            StatementType::Assign(a) => a.to_rust(ctx, options, symbols),
            StatementType::Break => Ok(quote! { break; }),
            StatementType::ClassDef(c) => c.to_rust(ctx, options, symbols),
            StatementType::Continue => Ok(quote! { continue; }),
            StatementType::Pass => Ok(quote! { /* Pass translates to nothing */ }),
            StatementType::FunctionDef(s) => s.to_rust(ctx, options, symbols),
            StatementType::Import(s) => s.to_rust(ctx, options, symbols),
            StatementType::ImportFrom(s) => s.to_rust(ctx, options, symbols),
            StatementType::Expr(s) => {
                // Generate code for the expression, but add a semicolon for statement context
                let expr_code = s.to_rust(ctx, options, symbols)?;
                Ok(quote! { #expr_code; })
            }
            StatementType::Return(None) => Ok(quote!(return;)), // Return without value
            StatementType::Return(Some(e)) => {
                let exp = e.to_rust(ctx, options, symbols)?;
                Ok(quote!(return #exp;))
            }
            StatementType::Unimplemented(s) => {
                 Err(Error::StatementNotYetImplemented(format!("Unimplemented statement type: {}", s)).into())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::tree::constant::Constant; // Import Constant
    use crate::parse; // Import parse
    use test_log::test; // Use test-log macro

    #[test]
    fn check_pass_statement() {
        let statement = StatementType::Pass;
        let options = PythonOptions::default();
        let mut symbols = SymbolTableScopes::new();
        let tokens = statement.to_rust(
            &CodeGenContext::Module("".to_string()),
            &options,
            &mut symbols,
        );

        debug!("statement: {:?}, tokens: {:?}", statement, tokens);
        assert!(tokens.is_ok());
        assert!(tokens.unwrap().to_string().trim().is_empty()); // Pass should generate no code or only comments
    }

    #[test]
    fn check_break_statement() {
        let statement = StatementType::Break;
        let options = PythonOptions::default();
        let mut symbols = SymbolTableScopes::new();
        let tokens = statement.to_rust(
            &CodeGenContext::Module("".to_string()),
            &options,
            &mut symbols,
        );

        debug!("statement: {:?}, tokens: {:?}", statement, tokens);
        assert!(tokens.is_ok());
        assert_eq!(tokens.unwrap().to_string(), "break ;");
    }

    #[test]
    fn check_continue_statement() {
        let statement = StatementType::Continue;
        let options = PythonOptions::default();
        let mut symbols = SymbolTableScopes::new();
        let tokens = statement.to_rust(
            &CodeGenContext::Module("".to_string()),
            &options,
            &mut symbols,
        );

        debug!("statement: {:?}, tokens: {:?}", statement, tokens);
        assert!(tokens.is_ok());
        assert_eq!(tokens.unwrap().to_string(), "continue ;");
    }

    #[test]
    fn return_with_nothing() {
        // Python's `return` implicitly returns None if no value is specified.
        // However, the AST node for `return` will have `value=None` (Python None object).
        // Our `extract` handles this, resulting in `StatementType::Return(None)`.
        let tree = parse("return", "<none>").unwrap();
        assert_eq!(tree.raw.body.len(), 1);
        assert_eq!(tree.raw.body[0].statement, StatementType::Return(None)); // Should be None after extraction logic
    }

    #[test]
    fn return_with_expr() {
        // The literal parsing needs to be correct for the comparison
        let lit = litrs::Literal::parse("8").expect("Parsing literal 8"); // Use litrs directly
        let tree = parse("return 8", "<none>").unwrap();
        assert_eq!(tree.raw.body.len(), 1);

        // We expect a Return statement containing an Expr statement,
        // which in turn contains a Constant expression.
        match &tree.raw.body[0].statement {
            StatementType::Return(Some(expr)) => match &expr.value {
                ExprType::Constant(Constant(Some(inner_lit))) => {
                    // Compare the parsed literal with the expected one
                    // Note: Direct comparison of litrs::Literal might be tricky due to spans.
                    // Comparing the string representation is often sufficient for tests.
                    assert_eq!(inner_lit.to_string(), lit.to_string());
                }
                _ => panic!("Inner expression is not a Constant"),
            },
            _ => panic!("Statement is not Return(Some(Expr))"),
        }
         // Add checks for line/col numbers if necessary
         assert_eq!(tree.raw.body[0].lineno, Some(1));
         assert_eq!(tree.raw.body[0].col_offset, Some(0));
    }

    #[test]
    fn does_module_compile() {
        let options = PythonOptions::default();
        let result = parse(
            "#test comment\ndef foo():\n    continue\n    pass\n",
            "test_case.py", // Give it a .py extension
        )
        .expect("Parsing failed");

        log::info!("Parsed Module: {:?}", result);
        let mut symbols = SymbolTableScopes::new();
        // First pass: find symbols
        result.find_symbols(&mut symbols);
        log::info!("Symbols after find_symbols: {:?}", symbols);

        // Second pass: generate code
        let code_result = result.to_rust(
            &CodeGenContext::Module("test_case".to_string()), // Use module name from filename
            &options,
            &mut symbols,
        );

        assert!(code_result.is_ok(), "CodeGen failed: {:?}", code_result.err());
        let code = code_result.unwrap();
        log::info!("Generated Rust Code:\n{}", code.to_string());
        // Add basic assertions about the generated code if possible
        assert!(!code.to_string().is_empty());
        assert!(code.to_string().contains("fn foo"));
        assert!(code.to_string().contains("continue ;"));
        // Pass generates no code, so no specific check needed unless comments are preserved
    }
}
