use crate::{internal_function, undefined_mut, ArgsT, Pointer, ValueT};

#[no_mangle]
pub static mut swcjs_global_assert: *mut ValueT = undefined_mut();

#[no_mangle]
extern "C" fn assert(args: &ArgsT) -> *mut ValueT {
    let message = args.args.get(1).cloned().unwrap_or(undefined_mut());
    let message = Pointer::from(message)
        .as_value()
        .map(|v| v.to_log_string())
        .unwrap_or_else(|| "".into());

    // TODO: use exceptions
    assert!(
        Pointer::from(args.args[0])
            .as_value()
            .unwrap()
            .as_boolean()
            .unwrap(),
        "message: {message}"
    );

    undefined_mut()
}

pub(crate) fn init() {
    unsafe {
        swcjs_global_assert = internal_function(assert);
    }
}
