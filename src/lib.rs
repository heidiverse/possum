use std::collections::HashMap;
use std::convert::TryFrom;

use chrono::Utc;
use serde_json::json;

pub mod parser;
pub use parser::arithmetic;
// Copyright (c) 2021 Patrick Amrein <amrein@ubique.ch>
//
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    VariableAssignment {
        offset: usize,
        name: String,
        expression: Box<Expression>,
    },
    Comment(String),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Import {
    Path(String),
    Name(String),
    None,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Conditional {
        condition: Box<Expression>,
        inner: Box<Expression>,
        other: Option<Box<Expression>>,
    },
    Not(Box<Expression>),
    Var(Vec<String>),
    VarWithDefault(Vec<String>, Box<Expression>),
    Operation(Operation),
    Comparison(Comparison),
    Atomic(Value),
    TimeInterval(Box<Expression>, String),
    Array(Vec<Expression>),
    ArrayOperation(Box<Expression>, String, Box<Expression>),
    ArrayOperationWithArguments(Box<Expression>, String, Box<Expression>, Vec<Expression>),
    Function(String, Vec<Expression>),
    Comment(String),
    Cast(Box<Expression>, String),
}

fn get(orig_value: &serde_json::Value, path: &Vec<String>) -> Option<Expression> {
    if path.is_empty() || path[0].is_empty() {
        return Some(Expression::Atomic(Value::from(orig_value)));
    }

    let mut value = orig_value.to_owned();
    let mut expression = Expression::Atomic(Value::Null);
    for s in path {
        if let Ok(index) = s.parse::<usize>() {
            if let Some(a) = value.as_array().and_then(|a| a.get(index)) {
                expression = Expression::Atomic(Value::from(a));
                value = a.to_owned();
            } else {
                return None;
            }
        } else {
            if let Some(a) = value.get(s) {
                expression = Expression::Atomic(Value::from(a));
                value = a.to_owned();
            } else {
                return None;
            }
        };
    }
    Some(expression)
}
impl From<&serde_json::Value> for Value {
    fn from(v: &serde_json::Value) -> Self {
        match v {
            serde_json::Value::Null => Self::Null,
            serde_json::Value::Bool(b) => Self::Bool(*b),
            serde_json::Value::Number(n) if n.as_i64().is_some() => {
                Self::Int(n.as_i64().unwrap() as i128)
            }
            serde_json::Value::Number(n) if n.as_u64().is_some() => {
                Self::Int(n.as_u64().unwrap() as i128)
            }
            serde_json::Value::Number(n) if n.as_f64().is_some() => {
                Self::Float(n.as_f64().unwrap())
            }
            serde_json::Value::Number(_) => Self::Null,
            serde_json::Value::String(s) => Self::String(s.to_string()),
            serde_json::Value::Array(a) => {
                let mut inner = Vec::with_capacity(a.len());
                for v in a {
                    inner.push(Self::from(v));
                }
                Self::Array(inner)
            }
            serde_json::Value::Object(v) => {
                let mut inner = HashMap::with_capacity(v.len());
                for (key, value) in v {
                    inner.insert(key.to_string(), Self::from(value));
                }
                Self::Object(inner)
            }
        }
    }
}

impl Expression {
    /// Whether the value counts as `true` in a boolean position.
    ///
    /// Missing values, the empty string, zero and the empty array are falsy;
    /// every other value is truthy.
    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Expression::Atomic(Value::Bool(b)) => Ok(*b),
            Expression::Atomic(Value::Array(b)) => Ok(!b.is_empty()),
            Expression::Atomic(Value::String(b)) => Ok(!b.is_empty()),
            Expression::Atomic(Value::Int(i)) => Ok(*i != 0),
            Expression::Atomic(Value::Float(f)) => Ok(*f != 0.0 && !f.is_nan()),
            Expression::Atomic(Value::TemporalAmount(_)) => Ok(true),
            Expression::Atomic(Value::Object(_)) => Ok(true),
            Expression::Atomic(Value::Null) => Ok(false),
            other => Err(format!("{:?} is not a value", other)),
        }
    }
    pub fn eval(&self, data: &serde_json::Value) -> Result<Expression, String> {
        match self {
            Expression::Conditional {
                condition,
                inner,
                other,
            } => {
                let condition = condition.eval(data)?.as_bool()?;
                if condition {
                    inner.eval(data)
                } else if let Some(other) = other {
                    other.eval(data)
                } else {
                    Err(String::from("Non exhaustive if"))
                }
            }
            Expression::Not(expr) => {
                let bool_expr = expr.eval(data)?.as_bool()?;
                Ok(Expression::Atomic(Value::Bool(!bool_expr)))
            }
            Expression::Var(key) => {
                if let Some(val) = get(data, key) {
                    Ok(val)
                } else {
                    Ok(Expression::Atomic(Value::Null))
                }
            }
            Expression::VarWithDefault(key, def) => {
                if let Some(v) = get(data, key) {
                    Ok(v)
                } else {
                    def.eval(data)
                }
            }
            Expression::Cast(e, ty) => {
                let var = e.eval(data)?;
                match ty.as_str() {
                    "Integer" => {
                        if let Expression::Atomic(v) = &var {
                            match v {
                                Value::String(s) => match s.parse::<i128>() {
                                    Ok(n) => Ok(Expression::Atomic(Value::Int(n))),
                                    _ => Err(format!("cannot cast {:?} to {ty}", var)),
                                },
                                Value::Int(i) => Ok(Expression::Atomic(Value::Int(*i))),
                                Value::Float(f) => Ok(Expression::Atomic(Value::Int(*f as i128))),
                                _ => Err(format!("cannot cast {:?} to {ty}", var)),
                            }
                        } else {
                            Err(format!("cannot cast {:?} to {ty}", var))
                        }
                    }
                    "Float" => {
                        if let Expression::Atomic(v) = &var {
                            match v {
                                Value::String(s) => match s.parse::<f64>() {
                                    Ok(n) => Ok(Expression::Atomic(Value::Float(n))),
                                    _ => Err(format!("cannot cast {:?} to {ty}", var)),
                                },
                                Value::Int(i) => Ok(Expression::Atomic(Value::Float(*i as f64))),
                                Value::Float(f) => Ok(Expression::Atomic(Value::Float(*f))),
                                _ => Err(format!("cannot cast {:?} to {ty}", var)),
                            }
                        } else {
                            Err(format!("cannot cast {:?} to {ty}", var))
                        }
                    }
                    "DateTime" => match &var {
                        Expression::Atomic(Value::TemporalAmount(millis)) => {
                            Ok(Expression::Atomic(Value::TemporalAmount(*millis)))
                        }
                        // A missing or empty value stays missing instead of
                        // becoming a date nobody wrote down.
                        Expression::Atomic(Value::Null) => Ok(Expression::Atomic(Value::Null)),
                        Expression::Atomic(Value::String(s)) if s.is_empty() => {
                            Ok(Expression::Atomic(Value::Null))
                        }
                        Expression::Atomic(Value::String(s)) => {
                            match parse_date_time_to_timestamp(s) {
                                Ok(millis) => {
                                    Ok(Expression::Atomic(Value::TemporalAmount(millis as i128)))
                                }
                                Err(_) => Err(format!("cannot cast {:?} to {ty}", var)),
                            }
                        }
                        _ => Err(format!("cannot cast {:?} to {ty}", var)),
                    },
                    "String" => match &var {
                        Expression::Atomic(Value::Null) => Ok(Expression::Atomic(Value::Null)),
                        Expression::Atomic(Value::String(s)) => {
                            Ok(Expression::Atomic(Value::String(s.to_owned())))
                        }
                        Expression::Atomic(v) => Ok(Expression::Atomic(Value::String(
                            match v.to_serde_json() {
                                serde_json::Value::String(s) => s,
                                other => other.to_string(),
                            },
                        ))),
                        _ => Err(format!("cannot cast {:?} to {ty}", var)),
                    },
                    "Boolean" => Ok(Expression::Atomic(Value::Bool(var.as_bool()?))),
                    _ => Err(format!("cannot cast {:?} to unknown type {ty}", var)),
                }
            }
            Expression::Operation(op) => op.eval(data),
            Expression::Comparison(comp) => comp.eval(data),
            Expression::Atomic(_) => Ok(self.to_owned()),
            Expression::TimeInterval(amount, unit) => {
                let amount = amount.eval(data)?;
                let value_in_seconds = match unit.as_str() {
                    "hour" | "hours" => Operation::Times(
                        Box::new(Expression::Atomic(Value::Int(3600))),
                        Box::new(amount.clone()),
                    )
                    .eval(data)?,
                    "minute" | "minutes" => Operation::Times(
                        Box::new(Expression::Atomic(Value::Int(60))),
                        Box::new(amount.clone()),
                    )
                    .eval(data)?,
                    "day" | "days" => Operation::Times(
                        Box::new(Expression::Atomic(Value::Int(86400))),
                        Box::new(amount.clone()),
                    )
                    .eval(data)?,
                    _ => amount,
                };
                Ok(value_in_seconds)
            }
            Expression::Array(arr) => {
                let mut new_array = vec![];
                for ele in arr {
                    let result = ele.eval(data)?;
                    if let Expression::Atomic(atomic_value) = result {
                        new_array.push(atomic_value);
                    } else {
                        new_array.push(Value::Null);
                    }
                }
                Ok(Expression::Atomic(Value::Array(new_array)))
            }
            Expression::ArrayOperation(array, function, block) => {
                let array = array.eval(data)?;
                match function.as_str() {
                    "filter" => match array {
                        Expression::Atomic(Value::Array(arr)) => {
                            let new_arr: Vec<_> = arr
                                .into_iter()
                                .filter(|a| {
                                    block
                                        .eval(&a.to_serde_json())
                                        .and_then(|a| a.as_bool())
                                        .unwrap_or(false)
                                })
                                .collect();
                            Ok(Expression::Atomic(Value::Array(new_arr)))
                        }
                        _ => Err("Array operation can only be applied to an array".into()),
                    },
                    _ => Err("Function not implemented".into()),
                }
            }
            Expression::ArrayOperationWithArguments(..) => Err("Not implemented".to_string()),
            Expression::Function(function_name, arguments) => match function_name.as_str() {
                "startsWith" => {
                    if !arguments.len() == 2 {
                        return Err(format!(
                            "In function needs two arguments. {} were given",
                            arguments.len()
                        ));
                    }
                    let haystack = arguments[0].eval(data)?;
                    let needle = arguments[1].eval(data)?;
                    match (&haystack, &needle) {
                        (
                            Expression::Atomic(Value::String(h)),
                            Expression::Atomic(Value::String(n)),
                        ) => Ok(Self::Atomic(Value::Bool(h.starts_with(n)))),
                        _ => Err(format!("Cannot compare {:?} with {:?}", haystack, needle)),
                    }
                }
                "substring" => {
                    if arguments.len() < 2 {
                        return Err(format!(
                            "Substring function needs at least two arguments. {} were given",
                            arguments.len()
                        ));
                    }
                    let input = arguments[0].eval(data)?;
                    if arguments.len() == 2 {
                        let start = arguments[1].eval(data)?;
                        if let (
                            Expression::Atomic(Value::String(s)),
                            Expression::Atomic(Value::Int(start)),
                        ) = (&input, &start)
                        {
                            if s.len() <= *start as usize {
                                return Err(format!(
                                    "start index {} is out of bounds {} [{}]",
                                    start,
                                    s.len(),
                                    s
                                ));
                            }
                            let slice = &s[*start as usize..];
                            Ok(Expression::Atomic(Value::String(slice.to_string())))
                        } else {
                            return Err(format!(
                                "Substring arguments were wrong. ({:?}, {:?}) was given",
                                input, start
                            ));
                        }
                    } else {
                        let start = arguments[1].eval(data)?;
                        let end = arguments[2].eval(data)?;

                        if let (
                            Expression::Atomic(Value::String(s)),
                            Expression::Atomic(Value::Int(start)),
                            Expression::Atomic(Value::Int(end)),
                        ) = (&input, &start, &end)
                        {
                            if s.len() <= *start as usize
                                || *start as usize >= *end as usize
                                || s.len() <= *end as usize
                            {
                                return Err(format!(
                                    "start index {} is out of bounds {} [{}]",
                                    start,
                                    s.len(),
                                    s
                                ));
                            }
                            let slice = &s[*start as usize..*end as usize];
                            Ok(Expression::Atomic(Value::String(slice.to_string())))
                        } else {
                            return Err(format!(
                                "Substring arguments were wrong. ({:?}, {:?}) was given",
                                input, start
                            ));
                        }
                    }
                }

                "contains" => {
                    if !arguments.len() == 2 {
                        return Err(format!(
                            "In function needs two arguments. {} were given",
                            arguments.len()
                        ));
                    }
                    let haystack = arguments[0].eval(data)?;
                    let needle = arguments[1].eval(data)?;
                    match (&haystack, &needle) {
                        (
                            Expression::Atomic(Value::String(h)),
                            Expression::Atomic(Value::String(n)),
                        ) => Ok(Self::Atomic(Value::Bool(h.contains(n)))),
                        _ => Err(format!("Cannot compare {:?} with {:?}", haystack, needle)),
                    }
                }
                "in" => {
                    if !arguments.len() == 2 {
                        return Err(format!(
                            "In function needs two arguments. {} were given",
                            arguments.len()
                        ));
                    }
                    let what = arguments[0].eval(data)?;
                    let array = arguments[1].eval(data)?;

                    if let Expression::Atomic(Value::Array(arr)) = array {
                        for exp in &arr {
                            if Expression::Atomic(exp.to_owned()) == what {
                                return Ok(Expression::Atomic(Value::Bool(true)));
                            }
                        }
                    }
                    Ok(Expression::Atomic(Value::Bool(false)))
                }
                _ => Err("Not implemented".to_string()),
            },
            Expression::Comment(_) => Ok(self.to_owned()),
        }
    }
    pub fn to_json_logic(&self) -> serde_json::Value {
        match self {
            Expression::Conditional {
                condition,
                inner,
                other,
            } => {
                let cond = condition.to_json_logic();
                let inner = inner.to_json_logic();
                if let Some(other) = other.as_ref() {
                    let other = other.to_json_logic();
                    json!({

                        "if" : [
                            cond,
                            inner,
                            other
                        ]
                    })
                } else {
                    json!({
                        "if" : [
                            cond,
                            inner
                        ]
                    })
                }
            }
            Expression::Var(v) => {
                json!({ "var": v.join(".").to_string() })
            }
            Expression::VarWithDefault(v, fall_back) => {
                let value = fall_back.to_json_logic();
                json!({"var" : [v.join(".").to_string(), value] })
            }
            Expression::Operation(a) => a.to_json_logic(),
            Expression::Not(i) => {
                let inner = i.to_json_logic();
                json!({ "!": [inner] })
            }
            Expression::Comparison(comp) => comp.to_json_logic(),
            Expression::Atomic(inner) => inner.to_serde_json(),
            Expression::Array(expressions) => {
                let mut json_logic_expressions = vec![];
                for exp in expressions {
                    json_logic_expressions.push(exp.to_json_logic());
                }
                json!(json_logic_expressions)
            }
            Expression::Function(func_name, args) => {
                let mut the_args = vec![];
                for arg in args {
                    the_args.push(arg.to_json_logic());
                }
                json!({ func_name: the_args })
            }
            Expression::ArrayOperation(var, func_name, inner_expr) => {
                let array = var.to_json_logic();
                let inner_expr = inner_expr.to_json_logic();

                json!({
                    func_name : [
                        array,
                        inner_expr
                    ]
                })
            }
            Expression::ArrayOperationWithArguments(var, func_name, inner_expr, args) => {
                let array = var.to_json_logic();
                let inner_expr = inner_expr.to_json_logic();
                let mut operands = vec![array, inner_expr];

                for arg in args {
                    let arg = arg.to_json_logic();
                    operands.push(arg);
                }
                json!({ func_name: operands })
            }
            Expression::TimeInterval(a, b) => {
                let inner = a.to_json_logic();
                json!({
                    "timeSpan" : [
                        inner,
                        b
                    ]
                })
            }
            _ => serde_json::Value::Null,
        }
    }
}
impl std::fmt::Display for Expression {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let exp =
            serde_json::to_string_pretty(&self.to_json_logic()).unwrap_or_else(|_| "".to_string());
        let _ = fmt.write_str(&exp);
        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Bool(bool),
    String(String),
    Int(i128),
    Float(f64),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
    TemporalAmount(i128),
    Null,
}

impl Value {
    pub fn to_serde_json(&self) -> serde_json::Value {
        match self {
            &Self::Bool(b) => serde_json::Value::Bool(b),
            Self::String(s) => serde_json::Value::String(s.to_owned()),
            &Self::Int(i) => i64::try_from(i)
                .map(|i| serde_json::Value::Number(i.into()))
                .unwrap_or(serde_json::Value::Null),
            &Self::Float(f) => serde_json::value::Number::from_f64(f)
                .map(serde_json::Value::Number)
                .unwrap_or(serde_json::Value::Null),
            // A point in time reads back as the date it stands for.
            &Self::TemporalAmount(millis) => i64::try_from(millis)
                .ok()
                .and_then(chrono::DateTime::from_timestamp_millis)
                .map(|d: chrono::DateTime<Utc>| {
                    serde_json::Value::String(
                        d.to_rfc3339_opts(chrono::SecondsFormat::Secs, false),
                    )
                })
                .unwrap_or(serde_json::Value::Null),
            Self::Array(array) => {
                let value_array = array.iter().map(|a| a.to_serde_json()).collect::<Vec<_>>();
                serde_json::Value::Array(value_array)
            }
            Self::Object(obj) => {
                // Sorted on the way out: objects live in a `HashMap`, and with
                // serde_json's `preserve_order` its map keeps insertion order,
                // so walking the `HashMap` directly would leak its random
                // order into the JSON and change it from one run to the next.
                let json_dict = obj
                    .iter()
                    .map(|(key, value)| (key.to_string(), value.to_serde_json()))
                    .collect::<std::collections::BTreeMap<_, _>>();
                serde_json::Value::Object(json_dict.into_iter().collect())
            }
            _ => serde_json::Value::Null,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Comparison {
    LessThan(Box<Expression>, Box<Expression>),
    LessThanEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    GreaterThanEqual(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    ExactEqual(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    NotExactEqual(Box<Expression>, Box<Expression>),
    Before(Box<Expression>, Box<Expression>),
    NotBefore(Box<Expression>, Box<Expression>),
    After(Box<Expression>, Box<Expression>),
    NotAfter(Box<Expression>, Box<Expression>),
}

impl std::fmt::Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Comparison::LessThan(_, _) => f.write_str("<"),
            Comparison::LessThanEqual(_, _) => f.write_str("<="),
            Comparison::GreaterThan(_, _) => f.write_str(">"),
            Comparison::GreaterThanEqual(_, _) => f.write_str(">="),
            Comparison::Equal(_, _) => f.write_str("=="),
            Comparison::ExactEqual(_, _) => f.write_str("==="),
            Comparison::NotEqual(_, _) => f.write_str("!="),
            Comparison::NotExactEqual(_, _) => f.write_str("!=="),
            Comparison::Before(_, _) => f.write_str("before"),
            Comparison::NotBefore(_, _) => f.write_str("not-before"),
            Comparison::After(_, _) => f.write_str("after"),
            Comparison::NotAfter(_, _) => f.write_str("not-after"),
        }
    }
}

impl Comparison {
    pub fn eval(&self, data: &serde_json::Value) -> Result<Expression, String> {
        match self {
            Comparison::LessThan(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a < b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a < b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) < *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a < (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a < b))),

                    (Expression::Atomic(Value::String(a)), Expression::Atomic(Value::Int(b))) => {
                        if let Ok(a_num) = a.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a_num < *b)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::String(b))) => {
                        if let Ok(b_num) = b.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a < &b_num)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }

                    (Expression::Atomic(Value::Null), Expression::Atomic(Value::Null)) => {
                        Ok(Expression::Atomic(Value::Bool(true)))
                    }
                    (Expression::Atomic(Value::Null), _) => {
                        Ok(Expression::Atomic(Value::Bool(false)))
                    }
                    (_, Expression::Atomic(Value::Null)) => {
                        Ok(Expression::Atomic(Value::Bool(false)))
                    }

                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::LessThanEqual(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a <= b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a <= b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) <= *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a <= (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a <= b))),

                    (Expression::Atomic(Value::String(a)), Expression::Atomic(Value::Int(b))) => {
                        if let Ok(a_num) = a.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a_num <= *b)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::String(b))) => {
                        if let Ok(b_num) = b.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a <= &b_num)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::GreaterThan(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a > b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a > b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) > *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a > (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a > b))),

                    (Expression::Atomic(Value::String(a)), Expression::Atomic(Value::Int(b))) => {
                        if let Ok(a_num) = a.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a_num > *b)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::String(b))) => {
                        if let Ok(b_num) = b.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a > &b_num)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::GreaterThanEqual(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a >= b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a >= b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) >= *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a >= (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a >= b))),
                    (Expression::Atomic(Value::String(a)), Expression::Atomic(Value::Int(b))) => {
                        if let Ok(a_num) = a.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a_num >= *b)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::String(b))) => {
                        if let Ok(b_num) = b.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a >= &b_num)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::Equal(a, b) | Comparison::ExactEqual(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a == b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a == b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) == *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a == (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a == b))),
                    (Expression::Atomic(Value::String(a)), Expression::Atomic(Value::Int(b)))
                    | (Expression::Atomic(Value::Int(b)), Expression::Atomic(Value::String(a))) => {
                        if let Ok(a_num) = a.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a_num == *b)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }
                    (Expression::Atomic(Value::Bool(a)), Expression::Atomic(Value::Bool(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a == b)))
                    }
                    (
                        Expression::Atomic(Value::TemporalAmount(a)),
                        Expression::Atomic(Value::TemporalAmount(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a == b))),
                    (Expression::Atomic(Value::Array(a)), Expression::Atomic(Value::Array(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a == b)))
                    }
                    (
                        Expression::Atomic(Value::Object(a)),
                        Expression::Atomic(Value::Object(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a == b))),
                    // Two missing values are equal; a missing value equals
                    // nothing else.
                    (Expression::Atomic(Value::Null), Expression::Atomic(Value::Null)) => {
                        Ok(Expression::Atomic(Value::Bool(true)))
                    }
                    (Expression::Atomic(Value::Null), Expression::Atomic(_))
                    | (Expression::Atomic(_), Expression::Atomic(Value::Null)) => {
                        Ok(Expression::Atomic(Value::Bool(false)))
                    }
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::NotEqual(a, b) | Comparison::NotExactEqual(a, b) => {
                let a = a.eval(data)?;
                let b = b.eval(data)?;
                match (&a, &b) {
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a != b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a != b)))
                    }
                    (Expression::Atomic(Value::Int(a)), Expression::Atomic(Value::Float(b))) => {
                        Ok(Expression::Atomic(Value::Bool((*a as f64) != *b)))
                    }
                    (Expression::Atomic(Value::Float(a)), Expression::Atomic(Value::Int(b))) => {
                        Ok(Expression::Atomic(Value::Bool(*a != (*b as f64))))
                    }
                    (
                        Expression::Atomic(Value::String(a)),
                        Expression::Atomic(Value::String(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a != b))),

                    (Expression::Atomic(Value::String(a)), Expression::Atomic(Value::Int(b)))
                    | (Expression::Atomic(Value::Int(b)), Expression::Atomic(Value::String(a))) => {
                        if let Ok(a_num) = a.parse::<i128>() {
                            Ok(Expression::Atomic(Value::Bool(a_num != *b)))
                        } else {
                            Err(String::from("Could not parse string to number"))
                        }
                    }
                    (Expression::Atomic(Value::Bool(a)), Expression::Atomic(Value::Bool(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a != b)))
                    }
                    (
                        Expression::Atomic(Value::TemporalAmount(a)),
                        Expression::Atomic(Value::TemporalAmount(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a != b))),
                    (Expression::Atomic(Value::Array(a)), Expression::Atomic(Value::Array(b))) => {
                        Ok(Expression::Atomic(Value::Bool(a != b)))
                    }
                    (
                        Expression::Atomic(Value::Object(a)),
                        Expression::Atomic(Value::Object(b)),
                    ) => Ok(Expression::Atomic(Value::Bool(a != b))),
                    // Two missing values are equal; a missing value equals
                    // nothing else.
                    (Expression::Atomic(Value::Null), Expression::Atomic(Value::Null)) => {
                        Ok(Expression::Atomic(Value::Bool(false)))
                    }
                    (Expression::Atomic(Value::Null), Expression::Atomic(_))
                    | (Expression::Atomic(_), Expression::Atomic(Value::Null)) => {
                        Ok(Expression::Atomic(Value::Bool(true)))
                    }
                    _ => Err(format!("cannot compare {:?} {:?}", a, b)),
                }
            }
            Comparison::Before(a, b) => {
                compare_dates(&a.eval(data)?, &b.eval(data)?, |a, b| a < b)
            }
            Comparison::NotBefore(a, b) => {
                compare_dates(&a.eval(data)?, &b.eval(data)?, |a, b| a >= b)
            }
            Comparison::After(a, b) => {
                compare_dates(&a.eval(data)?, &b.eval(data)?, |a, b| a > b)
            }
            Comparison::NotAfter(a, b) => {
                compare_dates(&a.eval(data)?, &b.eval(data)?, |a, b| a <= b)
            }
        }
    }
    pub fn to_json_logic(&self) -> serde_json::Value {
        match self {
            Comparison::LessThan(a, b)
            | Comparison::LessThanEqual(a, b)
            | Comparison::GreaterThan(a, b)
            | Comparison::GreaterThanEqual(a, b)
            | Comparison::Equal(a, b)
            | Comparison::ExactEqual(a, b)
            | Comparison::NotEqual(a, b)
            | Comparison::NotExactEqual(a, b)
            | Comparison::After(a, b)
            | Comparison::NotAfter(a, b)
            | Comparison::Before(a, b)
            | Comparison::NotBefore(a, b) => {
                let a = a.to_json_logic();
                let b = b.to_json_logic();
                let token = self.to_string();
                json!({
                    token: [
                        a,
                        b
                    ]
                }

                )
            }
        }
    }
}
#[derive(Clone, PartialEq, Debug)]
pub enum Operation {
    Times(Box<Expression>, Box<Expression>),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    PlusTime(Box<Expression>, Box<Expression>),
    MinusTime(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Modulo(Box<Expression>, Box<Expression>),
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Plus(_, _) => f.write_str("+"),
            Operation::Times(_, _) => f.write_str("*"),
            Operation::Minus(_, _) => f.write_str("-"),
            Operation::PlusTime(_, _) => f.write_str("plusTime"),
            Operation::MinusTime(_, _) => f.write_str("minusTime"),
            Operation::And(_, _) => f.write_str("and"),
            Operation::Or(_, _) => f.write_str("or"),
            Operation::Modulo(_, _) => f.write_str("%"),
        }
    }
}

fn apply_operation(
    left: &Expression,
    right: &Expression,
    data: &serde_json::Value,
    op: fn(left: i128, right: i128) -> i128,
) -> Result<Expression, String> {
    let left = left.eval(data)?;
    let right = right.eval(data)?;
    match (left, right) {
        (Expression::Atomic(Value::Int(left)), Expression::Atomic(Value::Int(right))) => {
            Ok(Expression::Atomic(Value::Int(op(left, right))))
        }
        (Expression::Atomic(Value::Float(left)), Expression::Atomic(Value::Int(right))) => {
            Ok(Expression::Atomic(Value::Int(op(left as i128, right))))
        }
        (Expression::Atomic(Value::Int(left)), Expression::Atomic(Value::Float(right))) => Ok(
            Expression::Atomic(Value::Int(op(left as i128, right as i128))),
        ),
        (Expression::Atomic(Value::Float(left)), Expression::Atomic(Value::Float(right))) => Ok(
            Expression::Atomic(Value::Int(op(left as i128, right as i128))),
        ),
        _ => Err("Not implemented".to_string()),
    }
}

fn parse_date_time_to_timestamp(date: &str) -> Result<i64, String> {
    if let Ok(date) = date.parse::<chrono::NaiveDate>() {
        return Ok(date
            .and_hms_opt(0, 0, 0)
            .unwrap()
            .and_utc()
            .timestamp_millis());
    }
    if let Ok(date) = date.parse::<chrono::DateTime<Utc>>() {
        return Ok(date.timestamp_millis());
    }
    let new_date = format!("{date}Z");
    if let Ok(date) = new_date.parse::<chrono::DateTime<Utc>>() {
        return Ok(date.timestamp_millis());
    }
    Err("Could not parse date".to_string())
}

/// What a value means when it is compared with `is before` / `is after`.
enum DatePoint {
    /// A point in time, in milliseconds since the epoch.
    At(i128),
    /// No date at all: a missing value or an empty string.
    Missing,
}

fn as_date_point(expression: &Expression) -> Result<DatePoint, String> {
    match expression {
        Expression::Atomic(Value::TemporalAmount(millis)) => Ok(DatePoint::At(*millis)),
        Expression::Atomic(Value::Null) => Ok(DatePoint::Missing),
        Expression::Atomic(Value::String(s)) if s.is_empty() => Ok(DatePoint::Missing),
        Expression::Atomic(Value::String(s)) => parse_date_time_to_timestamp(s)
            .map(|millis| DatePoint::At(millis as i128))
            .map_err(|_| format!("cannot read {:?} as a date", s)),
        other => Err(format!("cannot read {:?} as a date", other)),
    }
}

/// Compares two dates, which may be date strings, values cast with
/// `as DateTime`, or the result of date arithmetic.
///
/// A date missing on either side makes the comparison `null` rather than an
/// error, so that `(!claims.x) or ((claims.x as DateTime) is after now())`
/// reads as "unlimited unless it has expired".
fn compare_dates(
    a: &Expression,
    b: &Expression,
    op: fn(i128, i128) -> bool,
) -> Result<Expression, String> {
    match (as_date_point(a)?, as_date_point(b)?) {
        (DatePoint::At(a), DatePoint::At(b)) => Ok(Expression::Atomic(Value::Bool(op(a, b)))),
        _ => Ok(Expression::Atomic(Value::Null)),
    }
}

impl Operation {
    pub fn eval(&self, data: &serde_json::Value) -> Result<Expression, String> {
        match self {
            Operation::Plus(left, right) => apply_operation(
                &left.eval(data)?,
                &right.eval(data)?,
                data,
                |left, right| left + right,
            ),
            Operation::Minus(left, right) => apply_operation(
                &left.eval(data)?,
                &right.eval(data)?,
                data,
                |left, right| left - right,
            ),
            Operation::Times(left, right) => apply_operation(
                &left.eval(data)?,
                &right.eval(data)?,
                data,
                |left, right| left * right,
            ),
            Operation::PlusTime(left, right) => {
                //"2024-01-01" is before "2024-02-02"
                let Expression::Atomic(Value::Int(seconds)) = right.eval(data)? else {
                    return Err(format!(
                        "TimeInterval should be TimeInterval {:?}",
                        right.eval(data)
                    ));
                };

                match left.eval(data)? {
                    Expression::Atomic(Value::String(left)) => {
                        let Ok(left) = parse_date_time_to_timestamp(&left) else {
                            return Err(format!(
                                "Cannot parse DateTime, {:?}",
                                chrono::NaiveDate::parse_from_str(&left, "%Y-%m-%d")
                            ));
                        };

                        Ok(Expression::Atomic(Value::TemporalAmount(
                            left as i128 + (seconds * 1000) as i128,
                        )))
                    }
                    Expression::Atomic(Value::TemporalAmount(left)) => Ok(Expression::Atomic(
                        Value::TemporalAmount(left + (seconds * 1000) as i128),
                    )),
                    _ => Err(format!(
                        "TimeInterval should be TimeInterval {:?}",
                        right.eval(data)
                    )),
                }
            }
            Operation::MinusTime(left, right) => {
                //"2024-01-01" is before "2024-02-02"
                let Expression::Atomic(Value::Int(seconds)) = right.eval(data)? else {
                    return Err(format!(
                        "TimeInterval should be TimeInterval {:?}",
                        right.eval(data)
                    ));
                };

                match left.eval(data)? {
                    Expression::Atomic(Value::String(left)) => {
                        let Ok(left) = parse_date_time_to_timestamp(&left) else {
                            return Err(format!(
                                "Cannot parse DateTime, {:?}",
                                chrono::NaiveDate::parse_from_str(&left, "%Y-%m-%d")
                            ));
                        };

                        Ok(Expression::Atomic(Value::TemporalAmount(
                            left as i128 - (seconds * 1000) as i128,
                        )))
                    }
                    Expression::Atomic(Value::TemporalAmount(left)) => Ok(Expression::Atomic(
                        Value::TemporalAmount(left - (seconds * 1000) as i128),
                    )),
                    _ => Err(format!(
                        "TimeInterval should be TimeInterval {:?}",
                        right.eval(data)
                    )),
                }
            }
            // Both sides stop as soon as one of them settles the answer, so a
            // guard such as `(!claims.x) or (... claims.x ...)` really does
            // guard the expression behind it.
            Operation::And(left, right) => {
                if !left.eval(data)?.as_bool()? {
                    return Ok(Expression::Atomic(Value::Bool(false)));
                }
                Ok(Expression::Atomic(Value::Bool(
                    right.eval(data)?.as_bool()?,
                )))
            }
            Operation::Or(left, right) => {
                if left.eval(data)?.as_bool()? {
                    return Ok(Expression::Atomic(Value::Bool(true)));
                }
                Ok(Expression::Atomic(Value::Bool(
                    right.eval(data)?.as_bool()?,
                )))
            }
            Operation::Modulo(left, right) => apply_operation(
                &left.eval(data)?,
                &right.eval(data)?,
                data,
                |left, right| left % right,
            ),
        }
    }
    fn extract_inner_if_same<'other>(
        &'other self,
        other: &'other Expression,
        storage: &mut Vec<serde_json::Value>,
    ) {
        match (self, other) {
            (Operation::Plus(_, _), Expression::Operation(Operation::Plus(a, b)))
            | (Operation::And(_, _), Expression::Operation(Operation::And(a, b)))
            | (Operation::Or(_, _), Expression::Operation(Operation::Or(a, b))) => {
                self.extract_inner_if_same(a, storage);
                self.extract_inner_if_same(b, storage);
            }
            _ => {
                storage.push(other.to_json_logic());
            }
        }
    }
    pub fn to_json_logic(&self) -> serde_json::Value {
        match self {
            Operation::Plus(a, b)
            | Operation::Minus(a, b)
            | Operation::And(a, b)
            | Operation::Or(a, b)
            | Operation::Modulo(a, b)
            | Operation::Times(a, b) => {
                let mut args = vec![];
                self.extract_inner_if_same(a, &mut args);
                self.extract_inner_if_same(b, &mut args);

                let token = self.to_string();
                json!({ token: args })
            }
            Self::PlusTime(a, b) | Self::MinusTime(a, b) => match b.as_ref() {
                Expression::TimeInterval(amount, unit) => {
                    let a = a.to_json_logic();
                    let amount = amount.to_json_logic();
                    let token = self.to_string();
                    json!({
                        token : [
                            a,
                            amount,
                            unit
                        ]
                    })
                }
                _ => json!({}),
            },
        }
    }
}

#[cfg(test)]
mod tests {

    use serde_json::json;

    use crate::{get, Expression};
    #[test]
    fn test_basic_operation() {
        let plus = super::arithmetic::expression("3 + 7")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Int(10)));

        let plus = super::arithmetic::expression("first + second")
            .unwrap()
            .eval(&json!({
                "first" : 3,
                "second" : 7
            }))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Int(10)));

        let plus = super::arithmetic::expression("3 - 7")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Int(-4)));
        let plus = super::arithmetic::expression("3 * 7")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Int(21)));
        let plus = super::arithmetic::expression("3 % 7")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Int(3)));

        let plus = super::arithmetic::expression("true and false")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Bool(false)));
        let plus = super::arithmetic::expression("true and true")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Bool(true)));
        let plus = super::arithmetic::expression("false and true")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Bool(false)));
        let plus = super::arithmetic::expression("false and false")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Bool(false)));

        let plus = super::arithmetic::expression("true || false").unwrap();
        let plus = plus.eval(&json!({})).unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Bool(true)));
        let plus = super::arithmetic::expression("true or true")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Bool(true)));
        let plus = super::arithmetic::expression("false or true")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Bool(true)));
        let plus = super::arithmetic::expression("false or false")
            .unwrap()
            .eval(&json!({}))
            .unwrap();
        assert_eq!(plus, Expression::Atomic(super::Value::Bool(false)));
    }

    #[test]
    fn test_truthy() {
        let test = super::arithmetic::expression("a && true")
            .unwrap()
            .eval(&json!({"a" : {}}))
            .unwrap();
        println!("{:?}", test);
        let test = super::arithmetic::expression("a && true")
            .unwrap()
            .eval(&json!({"a" : []}))
            .unwrap();
        println!("{:?}", test);
    }

    #[test]
    #[cfg(feature = "jsonlogic")]
    fn test_eval_jsonlogic() {
        let mut duration_eval = 0;
        let mut duration_json = 0;
        let logic: Expression = super::arithmetic::expression(
            r#"if (999 >= b) {[1,2,3,4]::filter { this % 2 == 0 }} else { false }"#,
        )
        .unwrap();
        let json_logic = logic.to_json_logic();
        for i in 0..=10000 {
            let start_eval = std::time::Instant::now();
            let result_eval = logic.eval(&json!({ "b": i })).unwrap();
            let end_eval = std::time::Instant::now();

            let start_json = std::time::Instant::now();
            let result = jsonlogic::apply(&json_logic, &json!({ "b": i })).unwrap();
            let end_json = std::time::Instant::now();
            duration_eval += (end_eval - start_eval).as_nanos();
            duration_json += (end_json - start_json).as_nanos();
            if let Expression::Atomic(val) = result_eval {
                assert_eq!(val.to_serde_json(), result);
            } else {
                panic!("Result eval did not evaluate to an atomic value");
            }
        }
        println!();
        println!("Eval: {}", duration_eval / 10000);
        println!("JsonLogic: {}", duration_json / 10000);
    }
    #[test]
    fn test_array_op_eval() {
        let logic: Expression =
            super::arithmetic::expression("[1,2,3,4]::filter { this % 2 == 0}").unwrap();
        let result = logic.eval(&json! {{}}).unwrap();
        if let Expression::Atomic(crate::Value::Array(arr)) = result {
            assert_eq!(arr.len(), 2);
            assert_eq!(arr[0], crate::Value::Int(2));
            assert_eq!(arr[1], crate::Value::Int(4));
        } else {
            panic!("should be array")
        }
    }
    #[test]
    fn test_json_path() {
        let j = json!( {
            "a" : {
                "b": true
            }
        });
        assert_eq!(
            get(&j, &vec!["a".to_owned(), "b".to_owned()])
                .unwrap()
                .to_json_logic(),
            serde_json::Value::Bool(true)
        );
    }
    #[test]
    fn test_in_operator() {
        let logic: Expression = super::arithmetic::expression("[1,2,3]::in(2)").unwrap();
        let result = logic.eval(&json! {{}}).unwrap();
        if let Expression::Atomic(crate::Value::Bool(b)) = result {
            assert_eq!(b, true);
        } else {
            panic!("should be bool")
        }
    }
}

#[cfg(test)]
mod date_and_boolean_tests {
    use crate::Expression;
    use serde_json::json;

    fn data() -> serde_json::Value {
        json!({
            "external": {"validationClock": "2026-10-27T10:15:00Z"},
            "claims": {
                "expiry": "2031-10-27",
                "expired": "2026-07-06",
                "ageOver18": true,
                "empty": "",
                "count": 1
            }
        })
    }

    fn eval(source: &str) -> serde_json::Value {
        match crate::arithmetic::expression(source)
            .expect("parses")
            .eval(&data())
            .expect("evaluates")
        {
            Expression::Atomic(v) => v.to_serde_json(),
            other => panic!("expected a value, got {:?}", other),
        }
    }

    #[test]
    fn now_reads_the_validation_clock() {
        assert_eq!(eval("now()"), json!("2026-10-27T10:15:00Z"));
    }

    #[test]
    fn dates_from_the_data_compare() {
        assert_eq!(
            eval("(claims.expiry as DateTime) is after now()"),
            json!(true)
        );
        assert_eq!(
            eval("(claims.expired as DateTime) is after now()"),
            json!(false)
        );
        assert_eq!(
            eval("(claims.expiry as DateTime) is not before now()"),
            json!(true)
        );
        assert_eq!(
            eval("(claims.expired as DateTime) is before now()"),
            json!(true)
        );
    }

    #[test]
    fn a_date_nobody_wrote_down_compares_to_null() {
        assert_eq!(
            eval("(claims.missing as DateTime) is after now()"),
            json!(null)
        );
        assert_eq!(
            eval("(claims.empty as DateTime) is after now()"),
            json!(null)
        );
    }

    #[test]
    fn a_guard_protects_the_date_behind_it() {
        let unlimited = |field: &str| {
            eval(&format!(
                "(!claims.{field}) or ((claims.{field} as DateTime) is after now())"
            ))
        };
        assert_eq!(unlimited("missing"), json!(true));
        assert_eq!(unlimited("empty"), json!(true));
        assert_eq!(unlimited("expiry"), json!(true));
        assert_eq!(unlimited("expired"), json!(false));
    }

    #[test]
    fn booleans_compare() {
        assert_eq!(eval("claims.ageOver18 == true"), json!(true));
        assert_eq!(eval("claims.ageOver18 != false"), json!(true));
        assert_eq!(eval("claims.missing == true"), json!(false));
        assert_eq!(eval("true == true"), json!(true));
        assert_eq!(eval("true == false"), json!(false));
    }

    #[test]
    fn missing_values_compare() {
        assert_eq!(eval("claims.missing == null"), json!(true));
        assert_eq!(eval("claims.expiry == null"), json!(false));
        assert_eq!(eval("claims.missing != null"), json!(false));
    }

    #[test]
    fn whole_numbers_stay_whole() {
        assert_eq!(eval("claims.count"), json!(1));
    }

    #[test]
    fn numbers_are_truthy_unless_zero() {
        assert_eq!(eval("!claims.count"), json!(false));
        assert_eq!(eval("claims.count and true"), json!(true));
        assert_eq!(eval("!0"), json!(true));
    }

    #[test]
    fn a_computed_date_reads_back_as_a_date() {
        assert_eq!(eval("now() as DateTime"), json!("2026-10-27T10:15:00+00:00"));
        assert_eq!(
            eval(r#""2022-01-01" + 3#days"#),
            json!("2022-01-04T00:00:00+00:00")
        );
    }

    #[test]
    fn subtracting_from_a_date_goes_backwards() {
        assert_eq!(
            eval("((claims.expiry as DateTime) - 1#day) is before (claims.expiry as DateTime)"),
            json!(true)
        );
    }
}

#[cfg(test)]
mod serialization_tests {
    use serde_json::json;

    /// Objects must come out in a fixed order. Possum keeps them in a
    /// `HashMap`, so without sorting the order would depend on serde_json's
    /// `preserve_order` feature and on the `HashMap`'s per-process seed.
    #[test]
    fn objects_serialize_in_a_stable_key_order() {
        let data = json!({"obj": {"zebra": 1, "apple": 2, "middle": 3}});
        let expression = crate::arithmetic::expression("obj").expect("parses");
        let once = match expression.eval(&data).expect("evaluates") {
            crate::Expression::Atomic(v) => v.to_serde_json().to_string(),
            other => panic!("expected a value, got {:?}", other),
        };
        assert_eq!(once, r#"{"apple":2,"middle":3,"zebra":1}"#);
        for _ in 0..100 {
            let again = match expression.eval(&data).expect("evaluates") {
                crate::Expression::Atomic(v) => v.to_serde_json().to_string(),
                other => panic!("expected a value, got {:?}", other),
            };
            assert_eq!(again, once);
        }
    }
}
