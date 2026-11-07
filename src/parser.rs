// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
//
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT
#![allow(clippy::redundant_closure_call)]

use super::*;
use peg::parser;
use std::sync::{Arc, Mutex};
parser! {
/// Doc comment
pub grammar arithmetic() for str {
    rule quoted_part() -> &'input str = "." "\"" s:string() "\"" {
        s
        }
    rule pointed_part() -> &'input str = "." p:part() {
        p
    }
    rule part() -> &'input str = quiet!{$(!keyword() ['a'..='z' | 'A'..='Z'| '0'..='9' | '_' |'-']+)}/expected!("Variable")
    rule path() -> Vec<&'input str> = first:$("$"/  (['a'..='z' | 'A'..='Z' ]+['a'..='z' | 'A'..='Z'| '0'..='9' | '_' |'-']*))rest:(quoted_part() / pointed_part())* {
        let mut r = rest.clone();
        if(first != "$") {
            r.insert(0, first);
        }
        r
    }
    rule var() -> Vec<String> = !keyword() p:path() {
        p.iter().map(|a| a.to_string()).collect::<_>()
    }
    rule var_checked(stmts: &Arc<Mutex<Vec<Statement>>>, offset: usize)-> &'input str =  var_name:$(!keyword() quiet!{['a'..='z' | 'A'..='Z']+[ '.' | 'a'..='z' | 'A'..='Z' |'0'..='9' | '_' |'-']*}) {?
        if is_defined(stmts, var_name, Some(offset)) {
            Err("variable already defined")
        } else {
            Ok(var_name)
        }
    }
    rule number() -> &'input str = quiet!{$("-"?['0'..='9']+)} / expected!("Number")
    rule string() -> &'input str = quiet!{$([^'"']*)} / expected!("String")
    rule float() -> &'input str = quiet!{$(['0'..='9']+"."['0'..='9']+)} / expected!("Float")
    rule date() -> &'input str = $(['0'..='9']*<4>"-"['0'..='9']*<2>"-"['0'..='9']*<2>)
    rule time() -> &'input str = $(['0'..='9']*<2>":"['0'..='9']*<2>(":"['0'..='9']*<2>("."['0'..='9']+)?)?)
    rule offset() -> &'input str = $((("+"/"-")['0'..='9']*<2>(":"['0'..='9']*<2>)?)/"Z")
    rule dateTime() -> &'input str = $(date()("T" time()(offset())?)?)
    rule time_interval() -> &'input str = quiet!{$(_ "#" _ ("years" / "year" / "months" / "month" / "days" / "day" / "hours" / "hour"))} / expected!("Time interval (year,month,hour).")
    rule bool() -> &'input str = quiet!{$("true" / "false")} /expected!("Boolean")
    rule _ = quiet!{[' ' | '\n']*}
    rule plus() = _ ("+"/ "plus")  _
    rule times() = _ ("*"/ "times")  _
    rule minus() = _ ("-" /"minus") _
    rule modulo() = _ ("%" / "mod") _
    rule and() = _ ("&&" / "and") _
    rule or() = _ ("||" / "or") _
    rule lt() = _ ("<" ) _
    rule before() = _ "is before" _
    rule notBefore() = _ ("is not before" / "not before") _
    rule after() = _ "is after" _
    rule notAfter() = _ ("is not after" / "not after") _
    rule lte() = _ ("<=" ) _
    rule gt() = _ (">") _
    rule gte() = _ (">=") _
    rule eq() = _ "==" _
    rule eeq() = _ "===" _
    rule ne() = _ "!=" _
    rule ene() = _ "!==" _
    rule not() = _ ("!" / "not") _
    rule null_coercion() = _ "??" _
    rule now() = _ quiet!{"now" _ "(" _ ")"} _
    rule min() = _ "min" _
    rule max() = _ "max" _
    rule dateString() = _ "\"" d:dateTime() "\""
    rule comment() -> Statement = _ "/*" _ comment:($((!("*/")['\0'..='\x7f'])*)) _ "*/" _ {?
        Ok(Statement::Comment(comment.to_string()))
    }
    rule import_comment() -> Import = _ "/*" _ comment:($((!("*/")['\0'..='\x7f'])*)) _ "*/" _ {?
        Ok(Import::None)
    }
    rule importname() -> &'input str  = quiet!{$(!keyword() [ '/'| '.' | 'a'..='z' | 'A'..='Z' |'0'..='9' | '_' |'-']+)}/expected!("import name")

    rule import() -> Import = _ "import" _ "\"" v:importname() "\"" _ ";" {
        Import::Path(v.to_string())
    }

    rule as_cast() = _ "as" _

    rule cast(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = precedence!{
        x:(@) as_cast() ty:var() {

            Expression::Cast(Box::new(x), ty.join("").to_string())
        }
        --
        _ "(" _ e:expression_with_level(stmts) _ ")" _ { e }
    }

    rule timeOperation(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = precedence!{
        x:@ as_cast() ty:var() {
            Expression::Cast(Box::new(x), ty.join("").to_string())
        }
        --
        x:(@) as_cast() ty:var() {
            Expression::Cast(Box::new(x), ty.join("").to_string())
        }
        --
          x:(@) plus() y:@ {
            Expression::Operation(Operation::PlusTime(Box::new(x), Box::new(y)))

        }
        x:(@) minus() y:@ {
             Expression::Operation(Operation::MinusTime(Box::new(x), Box::new(y)))
        }
        --
        x:(@) before() y:@ { Expression::Comparison(Comparison::Before(Box::new(x), Box::new(y))) }
        x:(@) notBefore() y:@ { Expression::Comparison(Comparison::NotBefore(Box::new(x), Box::new(y))) }
        x:(@) after() y:@ { Expression::Comparison(Comparison::After(Box::new(x), Box::new(y))) }
        x:(@) notAfter() y:@ { Expression::Comparison(Comparison::NotAfter(Box::new(x), Box::new(y))) }
        --
        _ now() _ {Expression::Var(vec!["external".to_owned(), "validationClock".to_owned()])}
        _ now() _ "as DateTime" _ {
            let unit = Expression::TimeInterval(Box::new(Expression::Atomic(Value::Int(0))), "day".to_string());
             Expression::Operation(Operation::PlusTime(Box::new(Expression::Var(vec!["external".to_owned(), "validationClock".to_owned()])), Box::new(unit)))
        }
        --
         x:(@) or() y:@ {
            Expression::Not(
                Box::new(
                    Expression::Operation(
                        Operation::And(Box::new(Expression::Not(Box::new(x))), Box::new(Expression::Not(Box::new(y))))
                    )
                )
            )
        }
        --
        x:(@) and() y:@ { Expression::Operation(Operation::And(Box::new(x), Box::new(y))) }
        --
        _ v:var() l:time_interval() _ {
            let l = l.trim().strip_prefix('#').unwrap();
            let l = if let Some(l) = l.strip_suffix('s') { l } else { l };
            let ex =  replace_variable(stmts, v.join(".").as_str(), Expression::Var(v));
            Expression::TimeInterval(Box::new(ex), l.trim().to_owned())
        }
        _ v:number() l:time_interval() {
            let l = l.trim().strip_prefix('#').unwrap();
            let l = if let Some(l) = l.strip_suffix('s') { l } else { l };
            Expression::TimeInterval(Box::new(Expression::Atomic(Value::Int(v.parse::<i128>().unwrap()))), l.trim().to_owned())
        }
        --
         _ "\"" d:dateTime() "\"" _ "as String" _ {
             Expression::Atomic(Value::String(d.to_string()))
         }
        --
        _ "\"" d:dateTime() "\"" {
            let date = Expression::Atomic(Value::String(d.to_owned()));
            let unit = Expression::TimeInterval(Box::new(Expression::Atomic(Value::Int(0))), "day".to_string());
            Expression::Operation(Operation::PlusTime(Box::new(date), Box::new(unit)))
        }
        --
        _ v:var() _ "as DateTime" _ {
            replace_variable(stmts, v.join(".").as_str(), Expression::Var(v))
        }
        --
        _ "(" _ e:expression_with_level(stmts) _ ")" _ { e }
        x:@ as_cast() ty:var() {
            Expression::Cast(Box::new(x), ty.join("").to_string())
        }
    }

    rule booleanCast(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ v:var() _ "as Boolean" _ {
        let ex =  replace_variable(stmts, v.join(".").as_str(), Expression::Var(v));
        Expression::Not(Box::new(Expression::Not(Box::new(ex))))
    }

    rule operation(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = precedence!{
        v:var() null_coercion() e:expression_with_level(stmts) {
            if is_defined(stmts, v.join(".").as_str(), None) {
                replace_variable(stmts, v.join(".").as_str(), Expression::Var(v))
            } else {
                Expression::VarWithDefault(v,Box::new(e))
            }
        }
        --
        x:(@) or() y:@ {
            Expression::Not(
                Box::new(
                    Expression::Operation(
                        Operation::And(Box::new(Expression::Not(Box::new(x))), Box::new(Expression::Not(Box::new(y))))
                    )
                )
            )
        }
        --
        x:(@) and() y:@ { Expression::Operation(Operation::And(Box::new(x), Box::new(y))) }
        --
        not() y:(@) {
            Expression::Not(Box::new(y))
        }
        x:(@) plus() y:@ {
            match y {
                Expression::TimeInterval(..) => {
                    Expression::Operation(Operation::PlusTime(Box::new(x), Box::new(y)))
                }
                _ => {
                    Expression::Operation(Operation::Plus(Box::new(x), Box::new(y)))
                }
            }
        }
        x:(@) minus() y:@ {
           match y {
                Expression::TimeInterval(..) => {
                    Expression::Operation(Operation::MinusTime(Box::new(x), Box::new(y)))
                }
                _ => {
                    Expression::Operation(Operation::Minus(Box::new(x), Box::new(y)))
                }
            }
        }
        --
        x:(@) times() y:@ {
            Expression::Operation(Operation::Times(Box::new(x), Box::new(y)))
        }
        --
           _ v:varUnary(stmts) _ "in" _ a:(array(stmts) / varUnary(stmts)) {
            Expression::Function("in".to_string(),vec![v,a])
        }
        --
        _ f:function(stmts) _ {f}
        _ a:arrayOperation(stmts) _ {a}
        _ a:arrayOperationWithArguments(stmts) _ {a}

        --
        x:(@) lt() y:@ { Expression::Comparison(Comparison::LessThan(Box::new(x), Box::new(y))) }
        x:(@) lte() y:@ { Expression::Comparison(Comparison::LessThanEqual(Box::new(x), Box::new(y))) }
        x:(@) gt() y:@ { Expression::Comparison(Comparison::GreaterThan(Box::new(x), Box::new(y))) }
        x:(@) gte() y:@ { Expression::Comparison(Comparison::GreaterThanEqual(Box::new(x), Box::new(y))) }
        x:(@) eq() y:@ { Expression::Comparison(Comparison::Equal(Box::new(x), Box::new(y))) }
        x:(@) eeq() y:@ { Expression::Comparison(Comparison::ExactEqual(Box::new(x), Box::new(y))) }
        x:(@) ne() y:@ { Expression::Comparison(Comparison::NotEqual(Box::new(x), Box::new(y))) }
        x:(@) ene() y:@ { Expression::Comparison(Comparison::NotExactEqual(Box::new(x), Box::new(y))) }
        --
           x:(@) modulo() y:@ {
            Expression::Operation(Operation::Modulo(Box::new(x), Box::new(y)))
        }
        --
         _ t:this() _ {t}
        _ l:bool() _ {Expression::Atomic(Value::Bool(l.parse::<bool>().unwrap()))}
        --
        _ v:var() _ {
            replace_variable(stmts, v.join(".").as_str(),  Expression::Var(v.to_owned()))
        }
        --
        _ f:float() _ {Expression::Atomic(Value::Float(f.parse::<f64>().unwrap()))}
        _ l:number() _ {Expression::Atomic(Value::Int(l.parse::<i128>().unwrap()))}
        _ "\"" s:string() "\"" _ {

            Expression::Atomic(Value::String(s.to_owned()))
        }
        --
         _ "[" _ e:expression_with_level(stmts)** _ "," _ "]" _ { Expression::Array(e)}
        --
         _ "(" _ e:expression_with_level(stmts) _ ")" _ { e }
         x:@ as_cast() ty:var() {
             Expression::Cast(Box::new(x), ty.join("").to_string())
         }
    }
    rule switch_if(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ _ ":" _ "if" second:expression_with_level(stmts) {
        second
    }

    rule switch_block(stmts: &Arc<Mutex<Vec<Statement>>>) -> (Expression,Expression, Option<Expression>) = _ label:expression_with_level(stmts) second:switch_if(stmts)? _"=>" _ "{" _ e:expression_with_level(stmts) _"}" _ {
       (label, e, second)
    }
    rule default_switch_block(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ "_" _ "=>" _ "{" _ e:expression_with_level(stmts)  _ "}" _ {
        e
    }
    rule switch(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ quiet!{"switch"} _ "(" _ e:expression_with_level(stmts) _ ")" _ "{" _ switch_statements:switch_block(stmts)++ _ default_block:(quiet!{default_switch_block(stmts)}/ expected!("Exhaustive Switch")) _ "}" _ {
        let mut switch_statements = switch_statements;
        let expressions = switch_statements.pop().unwrap();

        let comparison = if matches!(expressions.0, Expression::Array(..) | Expression::ArrayOperation( ..) | Expression::ArrayOperationWithArguments(..)) {
            Expression::Function("in".into(), vec![e.clone(), expressions.0])
        } else {
            Expression::Comparison(Comparison::ExactEqual(Box::new(e.clone()), Box::new(expressions.0)))
       };
       let comparison = if let Some(expression) = expressions.2 {
            Expression::Operation(Operation::And(Box::new(comparison), Box::new(expression)))
       } else {
           comparison
       };
       let last = Expression::Conditional{condition: Box::new(comparison), inner: Box::new(expressions.1), other: Some(Box::new(default_block))};
        let mut final_element = last;
        while let Some((cond, inner, second)) = switch_statements.pop() {
             let comparison = if matches!(cond, Expression::Array(..) | Expression::ArrayOperation( ..) | Expression::ArrayOperationWithArguments(..)) {
                Box::new(Expression::Function("in".into(), vec![e.clone(), cond]))
            } else {
                let comparison = Comparison::ExactEqual(Box::new(e.clone()), Box::new(cond));
                Box::new(Expression::Comparison(comparison))
            };
            let condition = if let Some(second) = second {
                Box::new(Expression::Operation(Operation::And(comparison, Box::new(second))))
            } else {
                comparison
            };
            let next = Expression::Conditional{
                condition,
                inner: Box::new(inner),
                other: Some(Box::new(final_element))
            };
            final_element = next;
        }
        final_element
    }
    rule conditionalWithElse(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _  quiet!{"if"} _ "(" _ e:expression_with_level(stmts) _ ")" _ "{" _ i:expression_with_level(stmts) _ "}" _ "else" _ "{" _ o:expression_with_level(stmts) _ "}" _ {
        Expression::Conditional{condition: Box::new(e), inner: Box::new(i), other: Some(Box::new(o))}
    }
    rule unary(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ now() _ {
        Expression::Var(vec!["external".to_owned(), "validationClock".to_owned()])
    }

    rule keyword() = "if"/"switch"/"else"/"this"/ "true" / "false"

    rule array(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression =  _ "[" _ e:expression_with_level(stmts)** "," _ "]" _ { Expression::Array(e)}

    rule varUnary(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ v:var() _ {
        replace_variable(stmts, v.join(".").as_str(), Expression::Var(v.to_owned()))
    }

    rule arrayOperation(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ (!varOperation(stmts)) expr:(array(stmts) / varUnary(stmts)) _ "::" _ function:var() _ "{" _ inner:expression_with_level(stmts) _"}" {
        Expression::ArrayOperation(Box::new(expr), function.join(".").as_str().to_owned(), Box::new(inner))
    }
      rule arrayOperationWithArguments(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ (!varOperation(stmts)) expr:(array(stmts) / varUnary(stmts)) _ "::" _ function:var() _ "(" args:expression_with_level(stmts)** "," _ ")" _ "{" _ inner:expression_with_level(stmts) _"}" {
        Expression::ArrayOperationWithArguments(Box::new(expr), function.join(".").as_str().to_owned(), Box::new(inner), args)
    }
    rule booleanExpression(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = b:bool() {
        Expression::Atomic(Value::Bool(b.parse().unwrap()))
    }
    rule this() -> Expression = _ quiet!{"this"} _ {Expression::Var(vec!["".to_owned()])}
    rule varOperation(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ v:var() _ ":" _ f:function(stmts) {
        let Expression::Function(name, mut args) = f else {
            panic!("")
        };
        args.insert(0, Expression::Var(v));
        Expression::Function(name, args)
    }
    rule function(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = _ (!keyword()) func_name:var() _ "(" _ args:expression_with_level(stmts)++ "," _ ")" _ {
        let func_name = func_name.join(".").to_owned();
        match func_name.as_str() {
            "min" => {

                match args.len() {
                    0 => {
                         Expression::Function(func_name, args)
                    },
                    1 => {
                        args[0].clone()
                    },
                    2 => {
                        if matches!(args[0], Expression::Operation(Operation::PlusTime(..) | Operation::MinusTime(..)))
                            | matches!(args[1], Expression::Operation(Operation::PlusTime(..) | Operation::MinusTime(..))) {
                            Expression::Conditional {
                                condition: Box::new( Expression::Comparison(Comparison::Before(Box::new(args[0].clone()), Box::new(args[1].clone())))),
                                inner: Box::new(args[0].clone()),
                                other: Some(Box::new(args[1].clone()))
                            }
                        } else {
                            Expression::Conditional{
                                condition: Box::new( Expression::Comparison(Comparison::LessThan(Box::new(args[0].clone()), Box::new(args[1].clone())))),
                                inner: Box::new(args[0].clone()),
                                other: Some(Box::new(args[1].clone()))
                            }
                        }
                    },
                    _ => {
                        let (a,b) = (args[0].clone(), args[1].clone());
                          if matches!(a, Expression::Operation(Operation::PlusTime(..) | Operation::MinusTime(..)))
                            | matches!(b, Expression::Operation(Operation::PlusTime(..) | Operation::MinusTime(..))) {
                                args.iter().fold(
                                Expression::Conditional {
                                    condition: Box::new(Expression::Comparison(Comparison::Before(Box::new(a.clone()), Box::new(b.clone())))),
                                    inner: Box::new(a),
                                    other: Some(Box::new(b))
                                }, |prev, next| {
                                    let (a,b) = (prev, next.clone());
                                    Expression::Conditional {
                                        condition: Box::new(Expression::Comparison(Comparison::Before(Box::new(a.clone()), Box::new(b.clone())))),
                                        inner: Box::new(a),
                                        other: Some(Box::new(b))
                                    }
                                }
                            )
                        } else {
                            args.iter().fold(
                                Expression::Conditional {
                                    condition: Box::new(Expression::Comparison(Comparison::LessThan(Box::new(a.clone()), Box::new(b.clone())))),
                                    inner: Box::new(a),
                                    other: Some(Box::new(b))
                                }, |prev, next| {
                                    let (a,b) = (prev, next.clone());
                                    Expression::Conditional {
                                        condition: Box::new(Expression::Comparison(Comparison::LessThan(Box::new(a.clone()), Box::new(b.clone())))),
                                        inner: Box::new(a),
                                        other: Some(Box::new(b))
                                    }
                                }
                            )
                        }

                    }
                }
            },
            "max" => {
                match args.len() {
                    0 => {
                         Expression::Function(func_name.to_owned(), args)
                    },
                    1 => {
                        args[0].clone()
                    },
                    2 => {
                        Expression::Conditional{condition: Box::new( Expression::Comparison(Comparison::GreaterThan(Box::new(args[0].clone()), Box::new(args[1].clone())))),
                        inner: Box::new(args[0].clone()),
                        other: Some(Box::new(args[1].clone()))
                    }
                    },
                    _ => {
                        let (a,b) = (args[0].clone(), args[1].clone());
                        args.iter().fold(
                            Expression::Conditional {
                                condition: Box::new(Expression::Comparison(Comparison::GreaterThan(Box::new(a.clone()), Box::new(b.clone())))),
                                inner: Box::new(a),
                                other: Some(Box::new(b))
                            }, |prev, next| {
                                let (a,b) = (prev, next.clone());
                                Expression::Conditional {
                                    condition: Box::new(Expression::Comparison(Comparison::GreaterThan(Box::new(a.clone()), Box::new(b.clone())))),
                                    inner: Box::new(a),
                                    other: Some(Box::new(b))
                                }
                            }
                        )
                    }
                }
            },
            _ => {
                Expression::Function(func_name.to_owned(), args)
            }
        }

    }
    rule variable_assignment(stmts: &Arc<Mutex<Vec<Statement>>>) -> Statement = o:position!() "let" _ v:var_checked(stmts, o) _ "=" _ e:expression_with_level(stmts) _ ";" {
        let assignment = Statement::VariableAssignment{offset: o, name: v.to_string(), expression: Box::new(e)};
        let mut stmts = stmts.lock().unwrap();
        stmts.push(assignment.clone());
        assignment
    }

    rule statement(stmts: &Arc<Mutex<Vec<Statement>>>)  = _ c:(comment() / variable_assignment(stmts))  _

    rule new_frame(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression =  ((statement(stmts))*) e:(booleanCast(stmts) / (switch(stmts) / expected!("Switch")) / (conditionalWithElse(stmts) / expected!("Conditional with else"))  / (timeOperation(stmts) / operation(stmts) /expected!("Binary operator"))/ (arrayOperationWithArguments(stmts) / arrayOperation(stmts) / array(stmts)/expected!("Array expression"))  / (unary((stmts)) / function(stmts)/expected!("Function"))) {?
        Ok(e)
    }
    rule expression_with_level(stmts: &Arc<Mutex<Vec<Statement>>>) -> Expression = e:new_frame(&new_frame(stmts)) {
        e
    }

    pub rule expression() -> Expression = e:expression_with_level((&Arc::new(Mutex::new(vec![])))) {
        e
    }
    pub rule resolve_imports() -> (String, Vec<Import>) = i:((import()/import_comment())*) rest:$([_]*) {
        (rest.to_string(),i)
    }
}}
fn new_frame(parent_frame: &Arc<Mutex<Vec<Statement>>>) -> Arc<Mutex<Vec<Statement>>> {
    let parent_frame = parent_frame.lock().unwrap().clone();
    Arc::new(Mutex::new(parent_frame))
}
fn replace_variable(
    stmts: &Arc<Mutex<Vec<Statement>>>,
    var_name: &str,
    expr: Expression,
) -> Expression {
    if let Some(variable_replacement) = stmts.lock().unwrap().iter().find_map(|a| match a {
        Statement::VariableAssignment {
            offset: _offset,
            name,
            expression,
        } if name == var_name => Some(expression),
        _ => None,
    }) {
        let v = variable_replacement.clone();
        *v
    } else {
        expr
    }
}
fn is_defined(stmts: &Arc<Mutex<Vec<Statement>>>, var_name: &str, offset: Option<usize>) -> bool {
    matches!(
        stmts.lock().unwrap().iter().find_map(|a| match a {
            Statement::VariableAssignment {
                offset: off,
                name,
                expression,
            } if name == var_name => {
                if let Some(other_offset) = offset {
                    if off == &other_offset {
                        None
                    } else {
                        Some(expression)
                    }
                } else {
                    Some(expression)
                }
            }
            _ => None,
        }),
        Some(_)
    )
}

#[cfg(test)]
mod tests {

    use serde_json::{json, Value};

    use crate::{to_json_logic, Expression, Import};

    #[test]
    fn test_import() {
        let logic = r#"import "globals.aifc";import "other.aifc";

        let test = "1234";
        let a = test;
        if (a === b) {
            true
        } else {
            false
        }
        "#;
        let (logic, imports): (String, Vec<Import>) =
            super::arithmetic::resolve_imports(logic).unwrap();
        let logic: Expression = super::arithmetic::expression(&logic).unwrap();
        println!("{}", logic.to_json_logic());
        assert!(!imports.is_empty());
        for import in imports {
            println!("{:?}", import);
        }
    }

    #[test]
    fn test_vars_in_vars_replacement() {
        let logic = r#"
        let test = "1234";
        let a = test;
       a"#;
        let logic = super::arithmetic::expression(logic).unwrap();
        println!("{}", logic.to_json_logic());
    }
    #[test]
    fn if_without_else_fails() {
        let logic = r#"
        let a = "test";
        if (a == "test") {
           a
        }"#;
        let logic = super::arithmetic::expression(logic).unwrap_err();
        let output = format!("{:?}", logic);
        assert!(output.contains("else"));
    }
    #[test]
    fn multiple_var_bail() {
        let logic = r#"
        let test = "1234";
        let a = test;
        let a = a;
        if (a == "test") {
           a
        } else { undefined }"#;
        let logic = super::arithmetic::expression(logic).unwrap_err();
        let output = format!("{:?}", logic);
        assert!(output.contains("variable already defined"));
    }
    #[test]
    fn multiple_var_top_and_nested_fails() {
        let logic = r#"
        let test = "1234";
        let a = "test";
        if (a == "test") {
            let a = test;
            a
        } else {
            let a = a;
            undefined
        }"#;
        let logic = super::arithmetic::expression(logic).unwrap_err();
        let output = format!("{:?}", logic);
        assert!(output.contains("variable already defined"));
    }
    #[test]
    fn multiple_var_in_different_scopes() {
        let logic = r#"
        let test = "1234";
        if (a == "test") {
            let a = test;
            a
        } else {
            let a = test;
            undefined
        }"#;
        let logic = super::arithmetic::expression(logic).unwrap();
        println!("{:?}", logic);
    }

    #[test]
    fn string_operations() {
        let logic = r#"contains(input, "yes") and startsWith(input, "yes")"#;
        let logic = super::arithmetic::expression(logic).unwrap();
        println!("{:?}", logic);
        let result = logic
            .eval(&json!({
                "input" : "yes sir"
            }))
            .unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
    }
    #[test]
    fn cast_opperations() {
        let logic = r#"let integer_cast = "2";
            if(integer_cast as Integer
                < (p as Integer)) {
                true
            } else {
                false
            }
            "#;
        let logic = super::arithmetic::expression(logic).unwrap();
        println!("{:?}", logic);
        let result = logic
            .eval(&json!({
                "p" : "10"
            }))
            .unwrap();
        assert_eq!(result.as_bool().unwrap(), true);

        let logic = r#"let float_cast = "2.0" as Float;
            if(float_cast
                < (p as Float)) {
                true
            } else {
                false
            }
            "#;
        let logic = super::arithmetic::expression(logic).unwrap();
        println!("{:?}", logic);
        let result = logic
            .eval(&json!({
                "p" : "2.1"
            }))
            .unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
    }

    #[test]
    fn test_substring() {
        let logic = r#"let a = "a124";
            if(substring(a, 2) as Integer > 12) {
                true
            } else {
            false
            }
            "#;
        let logic = super::arithmetic::expression(logic).unwrap();
        let result = logic.eval(&json!({})).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);

        let logic = r#"let a = "a124";
            if(substring(a, 1,3) as Integer <= 12) {
                true
            } else {
            false
            }
            "#;
        let logic = super::arithmetic::expression(logic).unwrap();
        let result = logic.eval(&json!({})).unwrap();
        assert_eq!(result.as_bool().unwrap(), true);

        let logic = r#"let a = "a124";
           substring(a, 1,3) as Integer"#;
        let logic = super::arithmetic::expression(logic).unwrap();
        let result = logic.eval(&json!({})).unwrap();
        assert_eq!(result, Expression::Atomic(crate::Value::Int(12)));
    }
    #[test]
    fn test_jsonpath() {
        let logic = r#"
            if($."ch.ubique.test"."$['trest.a'][]" == 1) {
                true
            } else {
                false
            }
            "#;
        let logic = super::arithmetic::expression(logic).unwrap();
        println!("{:?}", logic);
        let result = logic
            .eval(&json!({
                "ch.ubique.test" : {
                    "$['trest.a'][]": 1
                }
            }))
            .unwrap();
        assert_eq!(result.as_bool().unwrap(), true);
        println!("{:?}", logic);
    }

    #[test]
    fn multiple_var_in_same_nested_scope_fails() {
        let logic = r#"
        let test = "1234";
        if (a == "test") {
            let a = test;
            let a = test;
            a
        } else {
            let a = test;
            undefined
        }"#;
        let logic = super::arithmetic::expression(logic).unwrap_err();
        let output = format!("{:?}", logic);
        assert!(output.contains("variable already defined"));
    }
    #[cfg(feature = "jsonlogic")]
    mod jsonlogic_tests {
        use serde_json::{json, Value};

        use crate::{arithmetic, Expression};
        #[test]
        fn test_null_coercion() {
            let logic = "a ?? true";
            let logic = arithmetic::expression(logic).unwrap();
            let json_logic = logic.to_json_logic();
            let res = ::jsonlogic::apply(&json_logic, &serde_json::Value::Null)
                .unwrap()
                .as_bool()
                .unwrap();
            assert!(res);
        }

        #[test]
        fn test_statements() {
            let stuff = r#"
        /* This is a comment */
        let two_dose_vaccines = [
            "a",
            "b"
        ];
        if (payload.v.0.mp in two_dose_vaccines) {
            true
        } else {
            false
        }
        "#;
            let expression: Expression = arithmetic::expression(stuff).unwrap();
            let mp_a = json!({
                "payload" : {
                    "v" : [
                        {
                            "mp": "a"
                        }
                    ]
                }
            });
            let mp_b = json!({
                "payload" : {
                    "v" : [
                        {
                            "mp": "b"
                        }
                    ]
                }
            });
            let mp_c = json!({
                "payload" : {
                    "v" : [
                        {
                            "mp": "c"
                        }
                    ]
                }
            });
            let logic = expression.to_json_logic();
            println!("{logic}");
            assert!(::jsonlogic::apply(&logic, &mp_a).unwrap().as_bool().unwrap());
            assert!(::jsonlogic::apply(&logic, &mp_b).unwrap().as_bool().unwrap());
            // this should be false
            assert!(!::jsonlogic::apply(&logic, &mp_c).unwrap().as_bool().unwrap());
        }
        #[test]
        fn test_assignment_replacement() {
            let stuff = r#"
            let a = if( 1 < 2 ) { "test" }  else { "other" };
            if (b === "test") {
                let c = a;
                c
            } else {
                "nope"
            }
        "#;
            let expression: Expression = arithmetic::expression(stuff).unwrap();
            let logic = expression.to_json_logic();
            let test_data = json!({"b" : "test"});
            let other_data = json!({"b": "other"});
            let result = ::jsonlogic::apply(&logic, &test_data).unwrap();
            assert_eq!("test", result.as_str().unwrap());
            let result = ::jsonlogic::apply(&logic, &other_data).unwrap();
            assert_eq!("nope", result.as_str().unwrap());
        }
        #[test]
        fn or_test() {
            let expression = arithmetic::expression("true or false").unwrap();
            println!("{}", expression.to_json_logic());
            let json_logic = expression.to_json_logic();
            let res = ::jsonlogic::apply(&json_logic, &serde_json::Value::Null)
                .unwrap()
                .as_bool()
                .unwrap();
            assert!(res);

            let expression = arithmetic::expression("false or true").unwrap();
            let json_logic = expression.to_json_logic();
            let res = ::jsonlogic::apply(&json_logic, &serde_json::Value::Null)
                .unwrap()
                .as_bool()
                .unwrap();
            assert!(res);

            let expression = arithmetic::expression("true or true").unwrap();
            let json_logic = expression.to_json_logic();
            let res = ::jsonlogic::apply(&json_logic, &serde_json::Value::Null)
                .unwrap()
                .as_bool()
                .unwrap();
            assert!(res);

            let expression = arithmetic::expression("false or false").unwrap();
            let json_logic = expression.to_json_logic();
            let res = ::jsonlogic::apply(&json_logic, &serde_json::Value::Null)
                .unwrap()
                .as_bool()
                .unwrap();
            assert!(!res);
        }
        #[test]
        fn test() {
            let expression: Expression = arithmetic::expression("now() + 3#days").unwrap();
            let validation_clock = json!({
                "external" : {
                    "validationClock" : "2022-01-01"
                }
            });
            let logic = expression.to_json_logic();
            let result = ::jsonlogic::apply(&logic, &validation_clock).unwrap();
            assert_eq!("2022-01-04T00:00:00+00:00", result.as_str().unwrap());
        }
        #[test]
        fn array_test() {
            let expression =
                arithmetic::expression("[now(), a, 3, now() + 3#years, [now() + 6#days]]")
                    .unwrap();
            println!("{}", expression.to_json_logic());
        }

        #[test]
        fn array_expr_test() {
            let array_expression =
                arithmetic::expression("[1,2,3,4,5]::filter { this % 2 == 0 }").unwrap();
            let logic = array_expression.to_json_logic();
            let result = ::jsonlogic::apply(&logic, &serde_json::Value::Null).unwrap();
            for v in result.as_array().unwrap() {
                let i = v.as_i64().unwrap();
                assert!(i % 2 == 0);
            }
        }

        #[test]
        fn test_in() {
            let in_expression: Expression =
                arithmetic::expression("a in [1,2,3,4]").unwrap();
            let val_1 = json!({"a": 1});
            let logic = in_expression.to_json_logic();
            let result = ::jsonlogic::apply(&logic, &val_1).unwrap();
            assert!(result.as_bool().unwrap());
            let val_2 = json!({"a": 5});
            let result = ::jsonlogic::apply(&logic, &val_2).unwrap();
            assert!(!result.as_bool().unwrap());
        }
        #[test]
        fn test_switch() {
            let switch_expression: Expression = arithmetic::expression(
                r#"
        switch(a.test) {
            ["my", "and", "case"] : if a.mp === "test" => {
                e
            }
            "test" => {
                b
            }

            "other" => {
                c
            }

            "third" => {
                d
            }
            ["fourth", "fifth", "sixth"] => {
                e
            }
            _ => {
                undefined
            }
        }
        "#,
            )
            .unwrap();
            let logic = switch_expression.to_json_logic();
            let test_case = json!({
                "a" : {
                    "test" : "case",
                    "mp" : "test"
                },
                "e" : true
            });
            let test_third = json!({
                "a" : {
                    "test" : "third",
                },
                "d" : true
            });
            let test_sixth = json!({
                "a" : {
                    "test" : "sixth",
                },
                "e" : true
            });
            let test_seventh = json!({
                "a" : {
                    "test" : "seventh",
                },
            });
            assert!(::jsonlogic::apply(&logic, &test_case)
                .unwrap()
                .as_bool()
                .unwrap());
            assert!(::jsonlogic::apply(&logic, &test_third)
                .unwrap()
                .as_bool()
                .unwrap());
            assert!(::jsonlogic::apply(&logic, &test_sixth)
                .unwrap()
                .as_bool()
                .unwrap());
            let undefined = ::jsonlogic::apply(&logic, &test_seventh).unwrap();
            assert_eq!(Value::Null, undefined);
        }

        #[test]
        fn test_min_desugar() {
            let min_desugar = arithmetic::expression("if (a < b) {a} else {b}").unwrap();
            let min = arithmetic::expression("min(a,b)").unwrap();
            assert_eq!(min, min_desugar);

            let min_desugared = arithmetic::expression("min(6,1,3,7)").unwrap();
            let logic = min_desugared.to_json_logic();
            let result = ::jsonlogic::apply(&logic, &serde_json::Value::Null)
                .unwrap()
                .as_i64()
                .unwrap();
            assert_eq!(result, 1);

            let min_desugared = arithmetic::expression("min(-10,200,1,87)").unwrap();
            let logic = min_desugared.to_json_logic();
            let result = ::jsonlogic::apply(&logic, &serde_json::Value::Null)
                .unwrap()
                .as_i64()
                .unwrap();
            assert_eq!(result, -10);
        }
        #[test]
        fn test_max_desugar() {
            let min_desugar = arithmetic::expression("if (a > b) {a} else {b}").unwrap();
            let min = arithmetic::expression("max(a,b)").unwrap();
            assert_eq!(min, min_desugar);

            let max_desugared = arithmetic::expression("max(6,1,3,7)").unwrap();
            let logic = max_desugared.to_json_logic();
            let result = ::jsonlogic::apply(&logic, &serde_json::Value::Null)
                .unwrap()
                .as_i64()
                .unwrap();
            assert_eq!(result, 7);

            let max_desugared = arithmetic::expression("max(-10,200,1,87)").unwrap();
            let logic = max_desugared.to_json_logic();
            let result = ::jsonlogic::apply(&logic, &serde_json::Value::Null)
                .unwrap()
                .as_i64()
                .unwrap();
            assert_eq!(result, 200);

            let max_desugared = arithmetic::expression("max(1000,200,1,87)").unwrap();
            let logic = max_desugared.to_json_logic();
            let result = ::jsonlogic::apply(&logic, &serde_json::Value::Null)
                .unwrap()
                .as_i64()
                .unwrap();
            assert_eq!(result, 1000);
        }
        #[test]
        fn test_time() {
            let time = arithmetic::expression(
                r#""2020-01-01" is not before "2020-02-02T00:00:00""#,
            )
            .unwrap();
            assert!(!::jsonlogic::apply(&time.to_json_logic(), &Value::Null)
                .unwrap()
                .as_bool()
                .unwrap());
            let time = arithmetic::expression(
                r#""2020-01-01" is not before "2020-02-02T00:00:00.000""#,
            )
            .unwrap();
            assert!(!::jsonlogic::apply(&time.to_json_logic(), &Value::Null)
                .unwrap()
                .as_bool()
                .unwrap());

            let time = arithmetic::expression(
                r#""2020-01-01" is not before "2020-02-02T00:00:00Z""#,
            )
            .unwrap();
            assert!(!::jsonlogic::apply(&time.to_json_logic(), &Value::Null)
                .unwrap()
                .as_bool()
                .unwrap());

            let time = arithmetic::expression(
                r#""2020-01-01" is not before "2020-02-02T00:00:00.999+03:00""#,
            )
            .unwrap();
            assert!(!::jsonlogic::apply(&time.to_json_logic(), &Value::Null)
                .unwrap()
                .as_bool()
                .unwrap());
            let time = arithmetic::expression(
                r#"(a as DateTime) is not before "2020-02-02T00:00:00.999+03:00""#,
            )
            .unwrap();
            assert!(
                !::jsonlogic::apply(&time.to_json_logic(), &json!({"a" : "2020-01-01"}))
                    .unwrap()
                    .as_bool()
                    .unwrap()
            );

            assert!(
                ::jsonlogic::apply(&time.to_json_logic(), &json!({"a" : "2020-02-03"}))
                    .unwrap()
                    .as_bool()
                    .unwrap()
            );
        }
    }

    #[test]
    fn test_comment() {
        let _ = to_json_logic! {{/* This is a comment */
            /* This is a comment */
            if (a < b) {
                /*comment*/
                if (c < b) {
                    /*
                        This is a multiline comment
                    */
                    c
                } else {
                    undefined
                }
            } else { undefined }
        }};
    }

    #[test]
    fn test_boolean_cast() {
        let logic = super::arithmetic::expression("a as Boolean").unwrap();
        let expr = super::arithmetic::expression("not not a").unwrap();
        assert_eq!(expr.to_json_logic(), logic.to_json_logic());
    }

    #[test]
    fn test_macro() {
        let logic = to_json_logic! ({
            if (payload.v.0) {
                if (payload.v.0.mp in ["EU/1/20/1525"]
                    && payload.v.0.dn === 1) {
                    (payload.v.0.dt as DateTime) + 386#days
                } else {
                    if(payload.v.0.mp in ["BBIBP-CorV_T","CoronaVac_T","Covaxin_T"]) {
                        if (payload.h.iat) {
                                min((payload.v.0.dt as DateTime) + 364#days, (payload.h.iat as DateTime) + 29#days)
                        } else {
                            undefined
                        }
                    }  else {
                            (payload.v.0.dt as DateTime) + 364#days
                    }
                }
            } else {
                if (payload.t.0) {
                    if (payload.t.0.tt === "LP6464-4") {
                        (payload.t.0.sc as DateTime) + 72#hours
                    }else {
                        if (payload.t.0.tt === "LP217198-3") {
                            (payload.t.0.sc as DateTime) + 48#hours
                        } else {
                            if(payload.t.0.tt === "94504-8"){
                                (payload.t.0.sc as DateTime) + 89#days
                            }  else {
                                    undefined
                            }
                        }
                    }
                } else {
                    if (payload.r.0) {
                        (payload.r.0.fr as DateTime) + 364#days
                    } else {
                        undefined
                    }
                }
            }
        });
        println!("{}", logic);
    }
    #[test]
    fn test_int_conversion() {
        let logic_input = r#""26" > 25"#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        assert!(r.eval(&Value::Null).unwrap().as_bool().unwrap());
        let logic_input = r#"25 < "26""#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        assert!(r.eval(&Value::Null).unwrap().as_bool().unwrap());
    }
    #[test]
    fn test_eval_time_operation() {
        let logic_input = r#""2024-01-01" is before "2024-02-02""#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        assert!(r.eval(&Value::Null).unwrap().as_bool().unwrap());

        let logic_input = r#""2024-01-01" is not after "2024-02-02""#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        assert!(r.eval(&Value::Null).unwrap().as_bool().unwrap());

        let logic_input = r#""2024-02-02" is after "2024-01-01""#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        assert!(r.eval(&Value::Null).unwrap().as_bool().unwrap());
        let logic_input = r#""2024-02-02" is not before "2024-01-01""#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        assert!(r.eval(&Value::Null).unwrap().as_bool().unwrap());

        let logic_input = r#""2024-01-01" is before ("2024-01-01" + 180#days)"#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        println!("{:?}", r.eval(&Value::Null));
        assert!(r.eval(&Value::Null).unwrap().as_bool().unwrap());

        let logic_input = r#""2024-01-01T00:00:00Z" is before "2024-01-01T01:00:01+01:00""#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        println!("{:?}", r.eval(&Value::Null));
        assert!(r.eval(&Value::Null).unwrap().as_bool().unwrap());

        let logic_input = r#""2024-01-01T00:00:00Z" is before "2024-01-01T01:00:00+01:00""#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        println!("{:?}", r.eval(&Value::Null));
        assert!(!r.eval(&Value::Null).unwrap().as_bool().unwrap());

        let logic_input = r#"("2024-01-01T00:00:00Z" is not before "2024-01-01T00:00:00") and ("2024-01-01T00:00:00Z" is not after "2024-01-01T00:00:00")"#;
        let r = super::arithmetic::expression(logic_input).unwrap();
        println!("{:?}", r.eval(&Value::Null));
        assert!(r.eval(&Value::Null).unwrap().as_bool().unwrap());
    }
}

#[macro_export]
macro_rules! to_json_logic {
    ($the_code:tt) => {{
        println!("{}", stringify!($the_code));
        let block_string = stringify!($the_code)
            .strip_prefix("{")
            .unwrap()
            .strip_suffix("}")
            .unwrap();
        let block_string = block_string.replace("== =", "===");
        $crate::arithmetic::expression(&block_string)
            .unwrap()
            .to_json_logic()
    }};
}
