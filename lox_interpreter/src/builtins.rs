use super::EvalResult;
use super::LoxValue;
use super::NativeFunction;

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
