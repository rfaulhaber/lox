use crate::value::native::NativeFunction;
use crate::value::native::NativeFunctionResult;
use crate::value::Value;

pub fn native_functions() -> Vec<NativeFunction> {
    vec![NativeFunction::new("clock", 0, clock)]
}

fn clock(_args: &[Value]) -> NativeFunctionResult {
    Ok(jiff::Timestamp::now().as_millisecond().into())
}
