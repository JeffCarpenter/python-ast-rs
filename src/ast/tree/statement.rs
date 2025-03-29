use anyhow::Result;
use log::debug;
use pyo3::{prelude::*, FromPyObject};
use serde::{Deserialize, Serialize};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    ast::dump::dump,
    codegen::{CodeGen, CodeGenContext, python_options::PythonOptions},
    symbols::SymbolTableScopes,
};

use super::{
    expression::{Expr, ExprType, Container},
    function_def::FunctionDef,
    class_def::ClassDef,
    assign::Assign,
    import::Import,
    import_from::ImportFrom,
    call::Call,
};
use crate::ast::node::Node;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Statement {
    #[serde(flatten)]
    pub statement: StatementType,

    pub lineno: Option<usize>,
    pub col_offset: Option<usize>,
    pub end_lineno: Option<usize>,
    pub end_col_offset: Option<usize>,
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
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn to_rust(
        self,
        ctx: Self::Context,
        options: Self::Options,
        symbols: Self::SymbolTable,
    ) -> Result<TokenStream> {
        self
            .statement
            .to_rust(ctx, options, symbols)
            .map_err(|e| anyhow::anyhow!("Failed to compile statement {:?}: {}", self, e))
    }

    fn find_symbols(self, symbols: Self::SymbolTable) -> Self::SymbolTable {
        self.statement.find_symbols(symbols);
        symbols
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
    Expr(Expr), // Represents expression statements (like standalone calls or constants)
    Pass,
    Return(Option<Expr>),
    Import(Import),
    ImportFrom(ImportFrom),
    FunctionDef(FunctionDef),
    If(If),
    While(While),
    For(For),
    With(With),
    Raise(Raise),
    Try(Try),
    Assert(Assert),
    Delete(Delete),
    AnnAssign(AnnAssign),
    AugAssign(AugAssign),
    Global(Global),
    Nonlocal(Nonlocal),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct If {
    pub test: Expr,
    pub body: Container<Statement>,
    pub orelse: Container<Statement>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct While {
    pub test: Expr,
    pub body: Container<Statement>,
    pub orelse: Container<Statement>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct For {
    pub target: Expr,
    pub iter: Expr,
    pub body: Container<Statement>,
    pub orelse: Container<Statement>,
    pub type_comment: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct With {
    pub items: Container<WithItem>,
    pub body: Container<Statement>,
    pub type_comment: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct WithItem {
    pub context_expr: Expr,
    pub optional_vars: Option<Expr>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Raise {
    pub exc: Option<Expr>,
    pub cause: Option<Expr>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Try {
    pub body: Container<Statement>,
    pub handlers: Container<ExceptionHandler>,
    pub orelse: Container<Statement>,
    pub finalbody: Container<Statement>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Assert {
    pub test: Expr,
    pub msg: Option<Expr>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Delete {
    pub targets: Container<ExprType>, // Changed from Expr to ExprType
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct ExceptionHandler {
    pub r#type: Option<Expr>,
    pub name: Option<String>,
    pub body: Container<Statement>,
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct AnnAssign {
    pub target: Expr,
    pub annotation: Expr,
    pub value: Option<Expr>,
    pub simple: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct AugAssign {
    pub target: Expr,
    pub op: String, // Op type?
    pub value: Expr,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Global {
    pub names: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Default)]
pub struct Nonlocal {
    pub names: Vec<String>,
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
            "Expr" => Ok(StatementType::Expr(Expr::extract(ob)?)),
            "Return" => {
                let value = ob.getattr("value")?;
                let expr_option: Option<Expr> = if value.is_none() {
                    None
                } else {
                    Some(value.extract()?)
                };
                Ok(StatementType::Return(expr_option))
            },
            "Import" => Ok(StatementType::Import(Import::extract(ob)?)),
            "ImportFrom" => Ok(StatementType::ImportFrom(ImportFrom::extract(ob)?)),
            "FunctionDef" => Ok(StatementType::FunctionDef(FunctionDef::extract(ob)?)),
            "If" => Ok(StatementType::If(If::extract(ob)?)),
            "While" => Ok(StatementType::While(While::extract(ob)?)),
            "For" => Ok(StatementType::For(For::extract(ob)?)),
            "With" => Ok(StatementType::With(With::extract(ob)?)),
            "Raise" => Ok(StatementType::Raise(Raise::extract(ob)?)),
            "Try" => Ok(StatementType::Try(Try::extract(ob)?)),
            "Assert" => Ok(StatementType::Assert(Assert::extract(ob)?)),
            "Delete" => Ok(StatementType::Delete(Delete::extract(ob)?)),
            "AnnAssign" => Ok(StatementType::AnnAssign(AnnAssign::extract(ob)?)),
            "AugAssign" => Ok(StatementType::AugAssign(AugAssign::extract(ob)?)),
            "Global" => Ok(StatementType::Global(Global::extract(ob)?)),
            "Nonlocal" => Ok(StatementType::Nonlocal(Nonlocal::extract(ob)?)),

            _ => {
                let err_msg = format!("Unimplemented statement type: {}", type_name);
                Err(pyo3::exceptions::PyValueError::new_err(
                    ob.error_message("<unknown>", err_msg),
                ))
            }
        }
    }
}

impl CodeGen for StatementType {
    type Context = CodeGenContext;
    type Options = PythonOptions;
    type SymbolTable = SymbolTableScopes;

    fn find_symbols(&self, symbols: &mut SymbolTableScopes) {
        match self {
            StatementType::Assign(a) => a.find_symbols(symbols),
            StatementType::ClassDef(c) => c.find_symbols(symbols),
            StatementType::FunctionDef(f) | StatementType::AsyncFunctionDef(f) => f.find_symbols(symbols),
            StatementType::Import(i) => i.find_symbols(symbols),
            StatementType::ImportFrom(i) => i.find_symbols(symbols),
            StatementType::Expr(e) => e.find_symbols(symbols),
            StatementType::If(if_stmt) => {
                for s in if_stmt.body.iter() {
                    s.statement.find_symbols(symbols);
                }
                for s in if_stmt.orelse.iter() {
                    s.statement.find_symbols(symbols);
                }
            },
            StatementType::While(while_stmt) => {
                for s in while_stmt.body.iter() {
                    s.statement.find_symbols(symbols);
                }
                for s in while_stmt.orelse.iter() {
                    s.statement.find_symbols(symbols);
                }
            },
            StatementType::For(for_stmt) => {
                for s in for_stmt.body.iter() {
                    s.statement.find_symbols(symbols);
                }
                for s in for_stmt.orelse.iter() {
                    s.statement.find_symbols(symbols);
                }
            },
            StatementType::With(with_stmt) => {
                for s in with_stmt.body.iter() {
                    s.statement.find_symbols(symbols);
                }
            },
            StatementType::Try(try_stmt) => {
                for s in try_stmt.body.iter() {
                    s.statement.find_symbols(symbols);
                }
                for handler in try_stmt.handlers.iter() {
                    for s in handler.body.iter() {
                        s.statement.find_symbols(symbols);
                    }
                }
                for s in try_stmt.orelse.iter() {
                    s.statement.find_symbols(symbols);
                }
                for s in try_stmt.finalbody.iter() {
                    s.statement.find_symbols(symbols);
                }
            },
            // Return, Pass, Break, Continue, Raise, Assert, Delete, AnnAssign, AugAssign, Global, Nonlocal don't typically introduce symbols at this level
            _ => {}
        }
    }

    fn to_rust(
        &self,
        ctx: &CodeGenContext,
        options: &PythonOptions,
        symbols: &mut SymbolTableScopes,
    ) -> Result<TokenStream> {
        match self {
            StatementType::AsyncFunctionDef(s) => {
                let async_ctx = CodeGenContext::Async(Box::new(ctx.clone()));
                s.to_rust(&async_ctx, options, symbols)
            },
            StatementType::Assign(a) => a.to_rust(ctx, options, symbols),
            StatementType::Break => Ok(quote! { break; }),
            StatementType::Continue => Ok(quote! { continue; }),
            StatementType::ClassDef(c) => c.to_rust(ctx, options, symbols),
            StatementType::Expr(e) => {
                let expr_code = e.to_rust(ctx, options, symbols)?;
                Ok(quote! { #expr_code; })
            },
            StatementType::Pass => Ok(quote! { () }),
            StatementType::Return(e) => {
                match e {
                    Some(exp) => {
                        let exp_code = exp.to_rust(ctx, options, symbols)?;
                        Ok(quote! { return #exp_code; })
                    },
                    None => Ok(quote! { return; }),
                }
            },
            StatementType::Import(s) => s.to_rust(ctx, options, symbols),
            StatementType::ImportFrom(s) => s.to_rust(ctx, options, symbols),
            StatementType::FunctionDef(s) => s.to_rust(ctx, options, symbols),
            StatementType::If(if_stmt) => {
                let test_code = if_stmt.test.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
                let body_code = if_stmt.body.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;
                let orelse_code = if_stmt.orelse.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;

                Ok(quote! {
                    if #test_code {
                        #(#body_code)*
                    } else {
                        #(#orelse_code)*
                    }
                })
            },
            StatementType::While(while_stmt) => {
                let test_code = while_stmt.test.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
                let body_code = while_stmt.body.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;
                let orelse_code = while_stmt.orelse.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;

                Ok(quote! {
                    while #test_code {
                        #(#body_code)*
                    } else {
                        #(#orelse_code)*
                    }
                })
            },
            StatementType::For(for_stmt) => {
                let target_code = for_stmt.target.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
                let iter_code = for_stmt.iter.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
                let body_code = for_stmt.body.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;
                let orelse_code = for_stmt.orelse.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;

                Ok(quote! {
                    for #target_code in #iter_code {
                        #(#body_code)*
                    } else {
                        #(#orelse_code)*
                    }
                })
            },
            StatementType::With(with_stmt) => {
                // This is a placeholder, needs proper implementation for 'with' statements
                let body_code = with_stmt.body.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;
                Ok(quote! {
                    {
                        // with statement logic here
                        #(#body_code)*
                    }
                })
            },
            StatementType::Raise(raise_stmt) => {
                let exc_code = raise_stmt.exc.as_ref().map(|e| e.to_rust(ctx.clone(), options.clone(), symbols.clone())).transpose()?;
                let cause_code = raise_stmt.cause.as_ref().map(|c| c.to_rust(ctx.clone(), options.clone(), symbols.clone())).transpose()?;

                match (exc_code, cause_code) {
                    (Some(exc), Some(cause)) => Ok(quote! { panic!("raise {} from {}", exc, cause) }),
                    (Some(exc), None) => Ok(quote! { panic!("raise {}", exc) }),
                    (None, _) => Ok(quote! { panic!("raise") }), // Bare raise
                }
            },
            StatementType::Try(try_stmt) => {
                // Placeholder for try-except-finally. Needs more complex logic
                let body_code = try_stmt.body.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;
                let handler_code = try_stmt.handlers.iter().map(|handler| {
                    let handler_body_code = handler.body.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;
                    let exception_type = handler.r#type.as_ref().map(|t| t.to_rust(ctx.clone(), options.clone(), symbols.clone())).transpose()?;
                    let exception_name = handler.name.clone();

                    Ok(quote! {
                        // Exception handler for type: #exception_type, name: #exception_name
                        #(#handler_body_code)*
                    })
                }).collect::<Result<Vec<_>>>()?;
                let orelse_code = try_stmt.orelse.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;
                let finalbody_code = try_stmt.finalbody.iter().map(|s| s.statement.to_rust(ctx, options, symbols)).collect::<Result<Vec<_>>>()?;


                Ok(quote! {
                    {
                        // try block
                        #(#body_code)*

                        // except blocks
                        #(#handler_code)*

                        // else block
                        #(#orelse_code)*

                        // finally block
                        #(#finalbody_code)*
                    }
                })
            },
            StatementType::Assert(assert_stmt) => {
                let test_code = assert_stmt.test.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
                let msg_code = assert_stmt.msg.as_ref().map(|m| m.to_rust(ctx.clone(), options.clone(), symbols.clone())).transpose()?;
                match msg_code {
                    Some(msg) => Ok(quote! { assert!(#test_code, #msg); }),
                    None => Ok(quote! { assert!(#test_code); }),
                }
            },
            StatementType::Delete(delete_stmt) => {
                // Placeholder for delete statements, needs proper implementation
                Ok(quote! {
                    // delete statement logic here
                })
            },
            StatementType::AnnAssign(ann_assign) => {
                let target_code = ann_assign.target.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
                let annotation_code = ann_assign.annotation.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
                let value_code = ann_assign.value.as_ref().map(|v| v.to_rust(ctx.clone(), options.clone(), symbols.clone())).transpose()?;

                match value_code {
                    Some(value) => Ok(quote! { let #target_code: #annotation_code = #value; }),
                    None => Ok(quote! { let #target_code: #annotation_code; }),
                }
            },
            StatementType::AugAssign(aug_assign) => {
                let target_code = aug_assign.target.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
                let value_code = aug_assign.value.to_rust(ctx.clone(), options.clone(), symbols.clone())?;
                let op = &aug_assign.op; // Operation needs to be mapped to Rust equivalent

                // Placeholder, needs proper operation mapping
                Ok(quote! { #target_code #op= #value_code; })
            },
            StatementType::Global(global_stmt) => {
                // Global statements are hints to the compiler and don't produce code directly
                Ok(quote! { /* global variables: #(#global_stmt.names),* */ })
            },
            StatementType::Nonlocal(nonlocal_stmt) => {
                // Nonlocal statements are hints and don't produce code directly
                Ok(quote! { /* nonlocal variables: #(#nonlocal_stmt.names),* */ })
            },
        }
    }
}
