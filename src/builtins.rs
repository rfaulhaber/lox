use crate::tree_walk::EvalResult;
use crate::tree_walk::LoxValue;
use crate::tree_walk::NativeFunction;

pub fn clock(_args: &[LoxValue]) -> EvalResult {
    Ok(LoxValue::Int(
        chrono::offset::Local::now().timestamp_millis(),
    ))
}

pub fn builtins() -> [NativeFunction; 1] {
    [NativeFunction {
        name: String::from("clock"),
        arity: 0,
        function: clock,
    }]
}
