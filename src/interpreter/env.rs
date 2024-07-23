use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::interpreter::value::Callable;

use super::{EvalError, LoxValue};

pub(super) type RefEnv = Rc<RefCell<Env>>;
pub(super) type Scope = HashMap<String, LoxValue>;

#[derive(Debug, Clone, PartialEq)]
pub(super) struct Env {
    pub(super) scopes: Vec<Scope>,
}

impl Env {
    pub fn new_with_builtins() -> Self {
        let mut env = Env::new();

        for builtin_fn in builtins() {
            match builtin_fn.clone() {
                Callable::Native { ref name, .. } => {
                    env.define(name, LoxValue::Callable(builtin_fn))
                }
                _ => unreachable!("non-native function defined"),
            }
        }

        env
    }

    pub fn define<S: ToString>(&mut self, name: S, value: LoxValue) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), value);
    }

    pub fn assign<S: ToString>(&mut self, name: S, value: LoxValue) -> Result<(), EvalError> {
        let name = name.to_string();
        let last = self.scopes.last_mut().unwrap();

        if last.contains_key(&name) {
            last.insert(name, value.clone());

            Ok(())
        } else {
            match self
                .scopes
                .iter_mut()
                .rev()
                .skip(1)
                .find(|scope| scope.contains_key(&name))
            {
                Some(scope) => {
                    scope.insert(name, value.clone());

                    Ok(())
                }
                None => Err(EvalError::UndefinedVariable(name)),
            }
        }
    }

    pub fn get<S: ToString>(&mut self, name: S) -> Option<LoxValue> {
        let name = name.to_string();

        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&name).cloned())
    }

    pub fn push_scope(&mut self) -> Env {
        self.scopes.push(HashMap::new());

        Env {
            scopes: self.scopes.clone(),
        }
    }

    pub fn pop_scope(&mut self) -> Env {
        self.scopes.pop();

        Env {
            scopes: self.scopes.clone(),
        }
    }

    pub fn from_existing(env: Env) -> Env {
        let mut scopes = env.scopes.clone();
        scopes.push(HashMap::new());

        Env { scopes }
    }

    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }
}

fn builtins() -> [Callable; 1] {
    [Callable::Native {
        name: String::from("clock"),
        arity: 0,
        func: |_args: Vec<LoxValue>| {
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

        env.define("n", LoxValue::Int(2));

        assert_eq!(env.get("n"), Some(LoxValue::Int(2)));
    }

    #[test]
    fn assign() {
        let mut env = Env::new_with_builtins();

        env.define("n", LoxValue::Int(2));

        assert_eq!(env.get("n"), Some(LoxValue::Int(2)));

        env.assign("n", LoxValue::Int(3));

        assert_eq!(env.get("n"), Some(LoxValue::Int(3)));
    }

    #[test]
    fn depth() {
        let mut env = Env::new_with_builtins();

        env.define("a", LoxValue::Int(2));

        env.push_scope();

        env.define("b", LoxValue::Int(3));

        env.push_scope();

        env.define("c", LoxValue::Int(4));

        assert_eq!(env.get("a"), Some(LoxValue::Int(2)));
        assert_eq!(env.get("b"), Some(LoxValue::Int(3)));
        assert_eq!(env.get("c"), Some(LoxValue::Int(4)));
        assert_eq!(env.get("d"), None);
    }

    #[test]
    fn scopes() {
        let mut env = Env::new_with_builtins();

        env.define("n", LoxValue::Int(2));

        env.push_scope();

        env.define("n", LoxValue::Int(3));

        assert_eq!(env.get("n"), Some(LoxValue::Int(3)));
    }
}
