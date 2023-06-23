use crate::{alloc, Pointer, ValueInner, ValueT};

#[no_mangle]
pub extern "C" fn swcjs_bin_eqeq(lhs_raw: *const ValueT, rhs_raw: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs_raw);
    let rhs = Pointer::from(rhs_raw);

    let out = match (lhs, rhs) {
        (Null, Null) | (Undefined, Undefined) | (Null, Undefined) | (Undefined, Null) => true,
        (_, Undefined) | (Undefined, _) | (Null, _) | (_, Null) => false,
        (Value(lhs), Value(rhs)) => match (&lhs.inner, &rhs.inner) {
            // Same types
            (ValueInner::Boolean(lhs), ValueInner::Boolean(rhs)) => lhs == rhs,
            // TODO: handle NaN and +/-0
            (ValueInner::Number(lhs), ValueInner::Number(rhs)) => lhs == rhs,
            (ValueInner::String(lhs), ValueInner::String(rhs)) => lhs == rhs,
            (ValueInner::Object(_), ValueInner::Object(_)) => lhs_raw == rhs_raw,
            (ValueInner::Function(_), ValueInner::Function(_)) => lhs_raw == rhs_raw,
            _ => {
                todo!()
            }
        },
    };

    alloc(ValueT::Boolean(out))
}

#[no_mangle]
pub extern "C" fn swcjs_bin_noteq(lhs_raw: *const ValueT, rhs_raw: *const ValueT) -> *mut ValueT {
    alloc(ValueT::Boolean(
        !(unsafe { &*swcjs_bin_eqeq(lhs_raw, rhs_raw) })
            .as_boolean()
            .unwrap(),
    ))
}

#[no_mangle]
pub extern "C" fn swcjs_bin_lt(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs);
    let rhs = Pointer::from(rhs);

    let out = match (lhs, rhs) {
        (Null, Null) | (Undefined, Undefined) => false,
        (_, Undefined) | (Undefined, _) => false,
        (Null, _) | (_, Null) => todo!(),
        (Value(lhs), Value(rhs)) => match (&lhs.inner, &rhs.inner) {
            (ValueInner::Boolean(true), ValueInner::Boolean(false)) => false,
            (ValueInner::Boolean(false), ValueInner::Boolean(true)) => true,
            (ValueInner::Number(lhs), ValueInner::Number(rhs)) => lhs < rhs,
            (ValueInner::String(lhs), ValueInner::String(rhs)) => lhs < rhs,
            _ => {
                todo!()
            }
        },
    };

    alloc(ValueT::Boolean(out))
}

#[no_mangle]
pub extern "C" fn swcjs_bin_gt(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    swcjs_bin_lt(rhs, lhs)
}

#[no_mangle]
pub extern "C" fn swcjs_bin_lteq(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs);
    let rhs = Pointer::from(rhs);

    let out = match (lhs, rhs) {
        (Null, Null) | (Undefined, Undefined) => true,
        (_, Undefined) | (Undefined, _) => true,
        (Null, _) | (_, Null) => todo!(),
        (Value(lhs), Value(rhs)) => match (&lhs.inner, &rhs.inner) {
            (ValueInner::Boolean(true), ValueInner::Boolean(false)) => false,
            (ValueInner::Boolean(false), ValueInner::Boolean(true)) => true,
            (ValueInner::Number(lhs), ValueInner::Number(rhs)) => lhs <= rhs,
            (ValueInner::String(lhs), ValueInner::String(rhs)) => lhs <= rhs,
            _ => {
                todo!()
            }
        },
    };

    alloc(ValueT::Boolean(out))
}

#[no_mangle]
pub extern "C" fn swcjs_bin_gteq(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    swcjs_bin_lteq(rhs, lhs)
}

#[no_mangle]
pub extern "C" fn swcjs_bin_add(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs);
    let rhs = Pointer::from(rhs);

    match (lhs, rhs) {
        (Undefined, _) | (_, Undefined) => alloc(ValueT::Number(f64::NAN)),
        (Null, _) | (_, Null) => todo!("javascript sucks"),
        (Value(lhs), Value(rhs)) => match (&lhs.inner, &rhs.inner) {
            (ValueInner::Boolean(_lhs), ValueInner::Boolean(_rhs)) => todo!(),
            (ValueInner::Number(lhs), ValueInner::Number(rhs)) => alloc(ValueT::Number(lhs + rhs)),
            (ValueInner::String(lhs), ValueInner::String(rhs)) => {
                alloc(ValueT::String(format!("{}{}", lhs, rhs)))
            }
            _ => {
                todo!()
            }
        },
    }
}

#[no_mangle]
pub extern "C" fn swcjs_bin_sub(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs);
    let rhs = Pointer::from(rhs);

    match (lhs, rhs) {
        (Undefined, _) | (_, Undefined) => alloc(ValueT::Number(f64::NAN)),
        (Null, _) | (_, Null) => todo!("javascript sucks"),
        (Value(lhs), Value(rhs)) => match (&lhs.inner, &rhs.inner) {
            (ValueInner::Boolean(_lhs), ValueInner::Boolean(_rhs)) => todo!(),
            (ValueInner::Number(lhs), ValueInner::Number(rhs)) => alloc(ValueT::Number(lhs - rhs)),
            (ValueInner::String(_lhs), ValueInner::String(_rhs)) => todo!(),
            _ => {
                todo!()
            }
        },
    }
}

#[no_mangle]
pub extern "C" fn swcjs_bin_mul(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs);
    let rhs = Pointer::from(rhs);

    match (lhs, rhs) {
        (Undefined, _) | (_, Undefined) => alloc(ValueT::Number(f64::NAN)),
        (Null, _) | (_, Null) => todo!("javascript sucks"),
        (Value(lhs), Value(rhs)) => match (&lhs.inner, &rhs.inner) {
            (ValueInner::Boolean(_lhs), ValueInner::Boolean(_rhs)) => todo!(),
            (ValueInner::Number(lhs), ValueInner::Number(rhs)) => alloc(ValueT::Number(lhs * rhs)),
            (ValueInner::String(_lhs), ValueInner::String(_rhs)) => todo!(),
            _ => {
                todo!()
            }
        },
    }
}

#[no_mangle]
pub extern "C" fn swcjs_bin_div(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs);
    let rhs = Pointer::from(rhs);

    match (lhs, rhs) {
        (Undefined, _) | (_, Undefined) => alloc(ValueT::Number(f64::NAN)),
        (Null, _) | (_, Null) => todo!("javascript sucks"),
        (Value(lhs), Value(rhs)) => match (&lhs.inner, &rhs.inner) {
            (ValueInner::Boolean(_lhs), ValueInner::Boolean(_rhs)) => todo!(),
            (ValueInner::Number(lhs), ValueInner::Number(rhs)) => alloc(ValueT::Number(lhs / rhs)),
            (ValueInner::String(_lhs), ValueInner::String(_rhs)) => todo!(),
            _ => {
                todo!()
            }
        },
    }
}

#[no_mangle]
pub extern "C" fn swcjs_bin_mod(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs);
    let rhs = Pointer::from(rhs);

    match (lhs, rhs) {
        (Undefined, _) | (_, Undefined) => alloc(ValueT::Number(f64::NAN)),
        (Null, _) | (_, Null) => todo!("javascript sucks"),
        (Value(lhs), Value(rhs)) => match (&lhs.inner, &rhs.inner) {
            (ValueInner::Boolean(_lhs), ValueInner::Boolean(_rhs)) => todo!(),
            (ValueInner::Number(lhs), ValueInner::Number(rhs)) => alloc(ValueT::Number(lhs % rhs)),
            (ValueInner::String(_lhs), ValueInner::String(_rhs)) => todo!(),
            _ => {
                todo!()
            }
        },
    }
}
