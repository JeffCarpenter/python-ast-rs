// This file might become obsolete or significantly changed if List/Tuple
// are primarily handled within ExprType and don't need their own AST node struct.
// For now, we'll keep the test file but adapt the test.

// Removed unused imports:
// use proc_macro2::TokenStream;
// use pyo3::{FromPyObject, PyAny};
// use quote::quote;
// use crate::{dump, CodeGen, CodeGenContext, PythonOptions, SymbolTableScopes};

// Removed the List struct and its CodeGen impl as it's handled in ExprType now.

#[cfg(test)]
mod tests {
    use crate::ast::tree::{expression::ExprType, statement::StatementType}; // Corrected import paths
    use crate::parse; // Import parse
    use test_log::test; // Use test-log macro

    #[test]
    fn parse_list() {
        // Parse a Python list expression used as a statement
        let module = parse("[1, 2, 3]", "list_test.py").expect("Parsing failed");
        assert_eq!(module.raw.body.len(), 1); // Should be one statement

        let statement = &module.raw.body[0].statement;
        match statement {
            // The list literal becomes an Expr statement
            StatementType::Expr(e) => match &e.value {
                // The value of the Expr is an ExprType::List
                ExprType::List(elements) => {
                    log::debug!("Parsed list elements: {:#?}", elements);
                    assert_eq!(elements.len(), 3); // Check the number of elements directly on the Vec

                    // Optionally, check the type/value of elements
                    assert!(matches!(elements[0], ExprType::Constant(_)));
                    assert!(matches!(elements[1], ExprType::Constant(_)));
                    assert!(matches!(elements[2], ExprType::Constant(_)));

                    // Example of checking a specific value (requires Constant::to_string or similar)
                    if let ExprType::Constant(c) = &elements[0] {
                         assert_eq!(c.to_string(), "1");
                    } else {
                         panic!("First element is not a Constant");
                    }
                }
                _ => panic!("Inner expression value is not ExprType::List, found {:?}", e.value),
            },
            _ => panic!("Statement is not StatementType::Expr, found {:?}", statement),
        }
    }

    #[test]
    fn parse_tuple() {
        // Parse a Python tuple expression used as a statement
        let module = parse("(1, 2, 3)", "tuple_test.py").expect("Parsing failed");
        assert_eq!(module.raw.body.len(), 1); // Should be one statement

        let statement = &module.raw.body[0].statement;
        match statement {
            // The tuple literal becomes an Expr statement
            StatementType::Expr(e) => match &e.value {
                // The value of the Expr is an ExprType::Tuple
                ExprType::Tuple(elements) => {
                    log::debug!("Parsed tuple elements: {:#?}", elements);
                    assert_eq!(elements.len(), 3); // Check the number of elements directly on the Vec

                    // Optionally, check the type/value of elements
                    assert!(matches!(elements[0], ExprType::Constant(_)));
                    assert!(matches!(elements[1], ExprType::Constant(_)));
                    assert!(matches!(elements[2], ExprType::Constant(_)));

                    if let ExprType::Constant(c) = &elements[0] {
                         assert_eq!(c.to_string(), "1");
                    } else {
                         panic!("First element is not a Constant");
                    }
                }
                _ => panic!("Inner expression value is not ExprType::Tuple, found {:?}", e.value),
            },
            _ => panic!("Statement is not StatementType::Expr, found {:?}", statement),
        }
    }
}
