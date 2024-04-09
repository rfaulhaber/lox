use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{Callable, EvalError, LoxValue};

#[derive(Debug, Clone, PartialEq)]
pub(super) struct Env {
    outer: Option<Rc<RefCell<Env>>>,
    values: HashMap<String, LoxValue>,
}

impl Env {
    pub fn new_with_builtins() -> Self {
        let mut env = Env::new();

        env.define(
            "clock".into(),
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

    pub fn define(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: LoxValue) -> Result<(), EvalError> {
        if self.values.contains_key(&name) {
            self.values.insert(name.clone(), value.clone());

            Ok(())
        } else {
            match self.outer {
                Some(ref outer) => outer.borrow_mut().assign(name, value),
                None => Err(EvalError::UndefinedVariable(name)),
            }
        }
    }

    pub fn get(&mut self, name: String) -> Option<LoxValue> {
        self.values
            .get(&name)
            .cloned()
            .or_else(|| match &self.outer {
                Some(ref outer) => outer.borrow_mut().get(name),
                None => None,
            })
    }

    pub fn from_outer(outer: Env) -> Env {
        Env {
            outer: Some(Rc::new(RefCell::new(outer))),
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
