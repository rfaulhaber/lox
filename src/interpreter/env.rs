use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{Callable, EvalError, LoxValue};

pub type RefEnv = Rc<RefCell<Env>>;

#[derive(Debug, Clone, PartialEq)]
pub(super) struct Env {
    pub(super) outer: Option<Rc<RefCell<Env>>>,
    pub(super) values: HashMap<String, LoxValue>,
}

impl Env {
    pub fn new_with_builtins() -> Self {
        let mut env = Env::new();

        env.define(
            "clock",
            LoxValue::Callable(Callable::Native {
                name: "clock".into(),
                arity: 0,
                func: |_args: Vec<LoxValue>| {
                    Ok(LoxValue::Int(
                        chrono::offset::Local::now().timestamp_millis(),
                    ))
                },
            }),
        );

        env
    }

    pub fn define<S: ToString>(&mut self, name: S, value: LoxValue) {
        self.values.insert(name.to_string(), value);
    }

    pub fn assign<S: ToString>(&mut self, name: S, value: LoxValue) -> Result<(), EvalError> {
        let name = name.to_string();

        if self.values.contains_key(&name) {
            self.values.insert(name, value.clone());

            Ok(())
        } else {
            match self.outer {
                Some(ref outer) => outer.assign(name, value),
                None => Err(EvalError::UndefinedVariable(name)),
            }
        }
    }

    pub fn get<S: ToString>(&mut self, name: S) -> Option<LoxValue> {
        let name = name.to_string();

        self.values
            .get(&name)
            .cloned()
            .or_else(|| match &self.outer {
                Some(ref outer) => outer.get(name),
                None => None,
            })
    }

    pub fn from_outer(outer: Env) -> Env {
        Env {
            outer: Some(Box::new(outer)),
            values: HashMap::new(),
        }
    }

    fn new() -> Self {
        Self {
            outer: None,
            values: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assign_trivial() {
        let val = LoxValue::Int(1);
        let mut env = Env::new();

        env.define("t", val);

        let _ = env.assign("t", LoxValue::Int(2));

        assert_eq!(env.get("t"), Some(LoxValue::Int(2)));
    }

    #[test]
    fn assign_nested() {
        let val = LoxValue::Int(1);
        let mut outer_env = Env::new();

        outer_env.define("t", val);

        let mut test_env = Env::from_outer(outer_env);

        let _ = test_env.assign("t", LoxValue::Int(2));

        assert_eq!(test_env.get("t"), Some(LoxValue::Int(2)));
    }

    #[test]
    fn closure_simulation() {
        let mut outer_env = Env::new();

        outer_env.define("t", LoxValue::Int(1));

        let mut closure = Env::from_outer(outer_env.clone());

        let _ = closure.assign("t", LoxValue::Int(2));

        assert_eq!(closure.get("t"), Some(LoxValue::Int(2)));
        assert_eq!(outer_env.get("t"), Some(LoxValue::Int(1)));
    }
}
