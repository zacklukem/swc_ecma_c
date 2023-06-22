use crate::{alloc, Pointer, ValueT};

#[no_mangle]
pub extern "C" fn swcjs_bin_eqeq(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs);
    let rhs = Pointer::from(rhs);

    let out = match (lhs, rhs) {
        (Null, Null) | (Undefined, Undefined) => true,
        (_, Undefined) | (Undefined, _) | (Null, _) | (_, Null) => false,
        (Value(ValueT::Boolean(lhs)), Value(ValueT::Boolean(rhs))) => lhs == rhs,
        (Value(ValueT::Number(lhs)), Value(ValueT::Number(rhs))) => lhs == rhs,
        (Value(ValueT::String(lhs)), Value(ValueT::String(rhs))) => lhs == rhs,
        (Value(_), Value(_)) => {
            todo!()
        }
    };

    alloc(ValueT::Boolean(out))
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
        (Value(ValueT::Boolean(true)), Value(ValueT::Boolean(false))) => false,
        (Value(ValueT::Boolean(false)), Value(ValueT::Boolean(true))) => true,
        (Value(ValueT::Number(lhs)), Value(ValueT::Number(rhs))) => lhs < rhs,
        // TODO: javascript string less than
        (Value(ValueT::String(lhs)), Value(ValueT::String(rhs))) => lhs < rhs,
        (Value(_), Value(_)) => {
            todo!()
        }
    };

    alloc(ValueT::Boolean(out))
}

#[no_mangle]
pub extern "C" fn swcjs_bin_gt(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    swcjs_bin_lt(rhs, lhs)
}

#[no_mangle]
pub extern "C" fn swcjs_bin_add(lhs: *const ValueT, rhs: *const ValueT) -> *mut ValueT {
    use Pointer::*;

    let lhs = Pointer::from(lhs);
    let rhs = Pointer::from(rhs);

    match (lhs, rhs) {
        (Undefined, _) | (_, Undefined) => alloc(ValueT::Number(f64::NAN)),
        (Null, _) | (_, Null) => todo!("javascript sucks"),
        (Value(ValueT::Boolean(_lhs)), Value(ValueT::Boolean(_rhs))) => todo!(),
        (Value(ValueT::Number(lhs)), Value(ValueT::Number(rhs))) => {
            alloc(ValueT::Number(lhs + rhs))
        }
        (Value(ValueT::String(lhs)), Value(ValueT::String(rhs))) => {
            alloc(ValueT::String(lhs.clone() + rhs))
        }
        (Value(_), Value(_)) => {
            todo!()
        }
    }
}
