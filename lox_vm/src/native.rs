use lox_value::native::NativeFunction;
use lox_value::native::NativeFunctionResult;
use lox_value::Value;

pub fn native_functions() -> Vec<NativeFunction> {
    vec![NativeFunction::new("clock", 0, clock)]
}

fn clock(_args: &[Value]) -> NativeFunctionResult {
    Ok(jiff::Timestamp::now().as_millisecond().into())
}
