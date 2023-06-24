use crate::{alloc, ValueInner, ValueT};

pub fn make_number(number: f64) -> *mut ValueT {
    alloc(ValueT {
        inner: ValueInner::Number(number),
    })
}

pub fn make_string(string: String) -> *mut ValueT {
    alloc(ValueT {
        inner: ValueInner::String(string),
    })
}

pub fn make_boolean(boolean: bool) -> *mut ValueT {
    alloc(ValueT {
        inner: ValueInner::Boolean(boolean),
    })
}
