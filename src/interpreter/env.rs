use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{Callable, EvalError, LoxValue};

pub type RefEnv = Rc<RefCell<Env>>;

#[derive(Debug, Clone, PartialEq)]
pub(super) struct Env {
    pub(super) outer: Option<RefEnv>,
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
                Some(ref outer) => outer.borrow_mut().assign(name, value),
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
                Some(ref outer) => outer.borrow_mut().get(name),
                None => None,
            })
    }

    pub fn from_outer(outer: RefEnv) -> RefEnv {
        Rc::new(RefCell::new(Env {
            outer: Some(outer),
            values: HashMap::new(),
        }))
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
    fn environments_correctly_update() {
        let env = Rc::new(RefCell::new(Env::new_with_builtins()));

        env.borrow_mut().define("n", LoxValue::Int(0));

        let inner = Env::from_outer(env.clone());

        inner.borrow_mut().define("n", LoxValue::Int(1));
        let _ = inner.borrow_mut().assign("n", LoxValue::Int(2));

        assert_eq!(inner.borrow_mut().get("n").unwrap(), LoxValue::Int(2));
        assert_eq!(env.borrow_mut().get("n").unwrap(), LoxValue::Int(0));
    }
}
