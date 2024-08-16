use std::collections::HashMap;

use crate::interpreter::value::Callable;

use super::{value::NativeFunction, EvalError, LoxValue};

#[derive(Debug, Clone, PartialEq)]
pub(super) struct Env {
    pub(super) enclosing: Option<Box<Env>>,
    pub(super) values: HashMap<String, Option<LoxValue>>,
}

pub(super) enum LookupResult<'l> {
    Ok(&'l LoxValue),
    UndefinedButDeclared,
    UndefinedAndUndeclared,
}

impl Default for Env {
    fn default() -> Self {
        Env::new()
    }
}

impl Env {
    pub fn new_with_builtins() -> Self {
        let mut env = Env::new();

        for builtin_fn in builtins() {
            env.define(builtin_fn.name.clone(), Some(LoxValue::Native(builtin_fn)));
        }

        env
    }

    pub fn define<S: ToString>(&mut self, name: S, value: Option<LoxValue>) {
        self.values.insert(name.to_string(), value);
    }

    pub fn assign<S: ToString>(
        &mut self,
        name: S,
        value: Option<LoxValue>,
    ) -> Result<(), EvalError> {
        let name = name.to_string();

        if self.values.contains_key(&name) {
            self.values.insert(name, value);
            Ok(())
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.assign(name, value)?;
            Ok(())
        } else {
            println!("current values: {:?}", self.values);
            Err(EvalError::UndefinedVariable(name))
        }
    }

    pub fn get<S: ToString>(&self, name: S) -> Option<&LoxValue> {
        match self.lookup(name.to_string()) {
            LookupResult::Ok(val) => Some(val),
            LookupResult::UndefinedButDeclared => todo!("return error"),
            LookupResult::UndefinedAndUndeclared => match &self.enclosing {
                Some(enclosing) => enclosing.get(name.to_string()),
                None => None,
            },
        }
    }

    pub fn lookup<S: ToString>(&self, name: S) -> LookupResult {
        match self.values.get(&name.to_string()) {
            Some(maybe_val) => match maybe_val {
                Some(val) => LookupResult::Ok(val),
                None => LookupResult::UndefinedButDeclared,
            },
            None => LookupResult::UndefinedAndUndeclared,
        }
    }

    pub fn with_enclosing(enclosing: Env) -> Env {
        Env {
            enclosing: Some(Box::new(enclosing)),
            values: HashMap::new(),
        }
    }

    fn new() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }
}

fn builtins() -> [NativeFunction; 1] {
    [NativeFunction {
        name: String::from("clock"),
        arity: 0,
        function: |_args: &[LoxValue]| {
            Ok(LoxValue::Int(
                chrono::offset::Local::now().timestamp_millis(),
            ))
        },
    }]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn define() {
        let mut env = Env::new_with_builtins();

        env.define("n", LoxValue::Int(2).into());

        assert_eq!(env.get("n"), Some(&LoxValue::Int(2)));
    }

    #[test]
    fn assign() {
        let mut env = Env::new_with_builtins();

        env.define("n", Some(LoxValue::Int(2)));

        assert_eq!(env.get("n"), Some(&LoxValue::Int(2)));

        env.assign("n", Some(LoxValue::Int(3)));

        assert_eq!(env.get("n"), Some(&LoxValue::Int(3)));
    }
}
