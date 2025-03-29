use std::collections::VecDeque;

use log::debug;
use pyo3::{
    prelude::*,
    types::{PyList, PyString},
    PyResult,
};

use crate::{
    ast::dump::dump,
    ast::node::Node,
    parser::parse,
    pytypes::{Literal, PyListLike},
};

use super::{
    bool_ops::{BoolOp, BoolOps},
    bin_ops::{BinOp, BinOps},
    compare::{Compare, Compares},
    constant::{Constant, try_bool, try_bytes, try_float, try_int, try_option, try_string},
    name::{Name, Identifier},
    named_expression::NamedExpr,
    unary_op::{Ops, UnaryOp},
    call::Call,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Container<T>(pub crate::pytypes::List<T>);

impl<T> Container<T> {
    pub fn new() -> Self {
        Self(VecDeque::new())
    }
}


impl<'a, 'p, T> FromPyObject<'a> for Container<crate::pytypes::List<ExprType>>
where
    T: FromPyObject<'a> + std::fmt::Debug,
{
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let list = crate::pytypes::List::<ExprType>::new();

        log::debug!("pylist: {}", dump(ob, Some(4))?);
        // Check if the object is a PyList
        if let Ok(py_list) = ob.downcast::<PyList>() {
            for item in py_list.iter() {
                match ExprType::extract(item) {
                    Ok(expr_type) => list.append(expr_type),
                    Err(e) => {
                        log::error!("Failed to extract ExprType from list item: {}", e);
                        return Err(e); // Or handle the error as needed
                    }
                }
            }
        } else {
            return Err(pyo3::exceptions::PyTypeError::new_err(format!(
                "Expected PyList, but got {:?}",
                ob
            )));
        }


        Ok(Self(list))
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum ExprType {
    BoolOp(BoolOp),
    NamedExpr(NamedExpr),
    BinOp(BinOp),
    UnaryOp(UnaryOp),
    Constant(Constant),
    Attribute(Attribute),
    Call(Call),
    Compare(Compare),
    Name(Name),
    List(Container<crate::pytypes::List<ExprType>>), // Changed to use Container
    Tuple(Container<crate::pytypes::List<ExprType>>), // Changed to use Container
    // Starred(Starred),
    // ListComp(ListComp),
    // TupleComp(TupleComp),
    // GeneratorExp(GeneratorExp),
    // Await(Await),
    // Yield(Yield),
    // YieldFrom(YieldFrom),
    // Lambda(Lamda),
    // IfExp(IfExp),
    // Dict(Dict),
    // Set(Set),
    // ListComp(ListComp),
    // SetComp(SetComp),
    // DictComp(DictComp),
    // ClassDef(ClassDef),
    // FunctionDef(FunctionDef),
    // AsyncFunctionDef(AsyncFunctionDef),
    // AnnAssign(AnnAssign),
    // Assert(Assert),
    // Assign(Assign),
    // AugAssign(AugAssign),
    // Break(Break),
    // ClassDef(ClassDef),
    // Continue(Continue),
    // Delete(Delete),
    // ExceptHandler(ExceptHandler),
    // For(For),
    // AsyncFor(AsyncFor),
    // FunctionDef(FunctionDef),
    // AsyncFunctionDef(AsyncFunctionDef),
    // Global(Global),
    // If(If),
    // Import(Import),
    // ImportFrom(ImportFrom),
    // Nonlocal(Nonlocal),
    // Pass(Pass),
    // Raise(Raise),
    // Return(Return),
    // Try(Try),
    // While(While),
    // With(With),
    // AsyncWith(AsyncWith),
    // match_(Match),
    // match_case(MatchCase),
    // type_ignore(TypeIgnore),
    // interactive(Interactive),
    // module(Module),
    // expression(Expression),
    // function_type(FunctionType),
    // comment(Comment),
    // module(Module),
    // alias(Alias),
    // withitem(WithItem),
    // comprehension(Comprehension),
    // excepthandler(Excepthandler),
    // arguments(Arguments),
    // arg(Arg),
    // keyword(Keyword),
    // cmpop(CmpOp),
    // unaryop(UnaryOp),
    // boolop(BoolOp),
    // operator(Operator),
    // mod(Mod),
    // stmt(Stmt),
    // expr_context(ExprContext),
    // slice(Slice),
    // boolop(Boolop),
    // excepthandler(Excepthandler),
    // mod(Mod),
    // stmt(Stmt),
    // identifier(Identifier),
    // string(String),
    // bytes(Bytes),
    // object(Object),
    // singleton(Singleton),
    // constant(Constant),
    // attribute(Attribute),
    // value(Value),
    // target(Target),
    // context_expr(ContextExpr),
    // iterable(Iterable),
    // body(Body),
    // orelse(Orelse),
    // finalbody(Finalbody),
    // decorator_list(DecoratorList),
    // returns(Returns),
    // type_comment(TypeComment),
    // type_params(TypeParams),
    // Param(Param),
    // Pattern(Pattern),
    // MatchValue(MatchValue),
    // MatchSingleton(MatchSingleton),
    // MatchSequence(MatchSequence),
    // MatchMapping(MatchMapping),
    // MatchClass(MatchClass),
    // MatchStar(MatchStar),
    // MatchAs(MatchAs),
    // MatchOr(MatchOr),
    // Kwarghandler(Kwarghandler),
    // Vararg(Vararg),
    // Kwarg(Kwarg),
}

impl<'a> FromPyObject<'a> for ExprType {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        log::debug!("exprtype ob: {}", dump(ob, Some(4))?);

        let expr_type = ob.get_type().name().expect(
            ob.error_message(
                "<unknown>",
                format!("extracting type name {:?} in expression", dump(ob, None)),
            )
            .as_str(),
        );

        log::debug!("expr_type: {}", expr_type);

        match expr_type.as_ref() {
            "BoolOp" => Ok(ExprType::BoolOp(BoolOp::extract(ob)?)),
            "NamedExpr" => Ok(ExprType::NamedExpr(NamedExpr::extract(ob)?)),
            "BinOp" => Ok(ExprType::BinOp(BinOp::extract(ob)?)),
            "UnaryOp" => Ok(ExprType::UnaryOp(UnaryOp::extract(ob)?)),
            "Constant" => Ok(ExprType::Constant(Constant::extract(ob)?)),
            "Attribute" => Ok(ExprType::Attribute(Attribute::extract(ob)?)),
            "Call" => Ok(ExprType::Call(Call::extract(ob)?)),
            "Compare" => Ok(ExprType::Compare(Compare::extract(ob)?)),
            "Name" => Ok(ExprType::Name(Name::extract(ob)?)),
            "List" => Ok(ExprType::List(Container::<crate::pytypes::List<ExprType>>::extract(ob)?)), // Use Container here
            "Tuple" => Ok(ExprType::Tuple(Container::<crate::pytypes::List<ExprType>>::extract(ob)?)), // Use Container here

            // ... handle other ExprType variants
            _ => {
                let err_msg = format!("Unimplemented ExprType: {} - {}", expr_type, dump(ob, None)?);
                Err(pyo3::exceptions::PyValueError::new_err(
                    ob.error_message("<unknown>", err_msg),
                ))
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub value: ExprType,
    pub ctx: Option<String>,
    pub lineno: Option<usize>,
    pub col_offset: Option<usize>,
    pub end_lineno: Option<usize>,
    pub end_col_offset: Option<usize>,
}

impl<'a> FromPyObject<'a> for Expr {
    fn extract(ob: &'a PyAny) -> PyResult<Self> {
        let err_msg = format!("extracting object value {} in expression", dump(ob, None)?);

        let ob_value = ob
            .getattr("value")
            .expect(ob.error_message("<unknown>", err_msg.as_str()).as_str());
        log::debug!("ob_value: {}", dump(ob_value, None)?);

        // The context is Load, Store, etc. For some types of expressions such as Constants, it does not exist.
        let ctx: Option<String> = if let Ok(pyany) = ob_value.getattr("ctx") {
            let ctx_type = pyany.get_type().name().expect(
                ob.error_message(
                    "<unknown>",
                    format!("extracting type name {:?} in expression context", dump(ob, None)),
                )
                .as_str(),
            );
            Some(ctx_type.to_string())
        } else {
            None
        };


        let value = ExprType::extract(ob_value)?;

        let lineno = ob.getattr("lineno").ok().and_then(|ln| ln.extract().ok());
        let col_offset = ob.getattr("col_offset").ok().and_then(|co| co.extract().ok());
        let end_lineno = ob.getattr("end_lineno").ok().and_then(|eln| eln.extract().ok());
        let end_col_offset = ob.getattr("end_col_offset").ok().and_then(|eco| eco.extract().ok());


        Ok(Expr {
            value,
            ctx,
            lineno,
            col_offset,
            end_lineno,
            end_col_offset,
        })
    }
}


impl Node for Expr {
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
